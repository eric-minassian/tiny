use super::Config;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ConfigError {
    InvalidOptLevel(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InvalidOptLevel(s) => {
                write!(f, "Invalid optimization level: {}", s)
            }
        }
    }
}

pub type ConfigResult = Result<Config, ConfigError>;
