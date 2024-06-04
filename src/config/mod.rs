use error::ConfigError;

pub mod error;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Config {
    pub opt_level: OptLevel,
}
impl Config {
    pub fn new(opt_level: OptLevel) -> Self {
        Self { opt_level }
    }

    pub fn from_env() -> Self {
        let opt_level = std::env::var("TINY_OPT_LEVEL")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or_default();
        Self { opt_level }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OptLevel {
    // None, TODO: Add None optimization level
    Default,
    Full,
}

impl std::str::FromStr for OptLevel {
    type Err = ConfigError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            // "0" | "none" => Ok(OptLevel::None),
            "1" | "default" => Ok(OptLevel::Default),
            "2" | "full" => Ok(OptLevel::Full),
            _ => Err(ConfigError::InvalidOptLevel(s.to_string())),
        }
    }
}

impl Default for OptLevel {
    fn default() -> Self {
        OptLevel::Default
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_from_env() {
        // std::env::set_var("TINY_OPT_LEVEL", "0");
        // let config = Config::from_env();
        // assert_eq!(config.opt_level, OptLevel::None);

        std::env::set_var("TINY_OPT_LEVEL", "1");
        let config = Config::from_env();
        assert_eq!(config.opt_level, OptLevel::Default);

        std::env::set_var("TINY_OPT_LEVEL", "2");
        let config = Config::from_env();
        assert_eq!(config.opt_level, OptLevel::Full);
    }

    #[test]
    fn test_opt_level_from_str() {
        // assert_eq!("0".parse(), Ok(OptLevel::None));
        assert_eq!("1".parse(), Ok(OptLevel::Default));
        assert_eq!("2".parse(), Ok(OptLevel::Full));
        assert_eq!(
            "3".parse::<OptLevel>(),
            Err(ConfigError::InvalidOptLevel("3".to_string()))
        );

        // assert_eq!("None".parse(), Ok(OptLevel::None));
        assert_eq!("Default".parse(), Ok(OptLevel::Default));
        assert_eq!("Full".parse(), Ok(OptLevel::Full));

        // assert_eq!("nOnE".parse(), Ok(OptLevel::None),);
        assert_eq!("DEFAULT".parse(), Ok(OptLevel::Default),);
        assert_eq!("fULL".parse(), Ok(OptLevel::Full),);

        assert_eq!(
            "invalid".parse::<OptLevel>(),
            Err(ConfigError::InvalidOptLevel("invalid".to_string()))
        );
    }

    #[test]
    fn test_opt_level_default() {
        assert_eq!(OptLevel::default(), OptLevel::Default);
    }
}
