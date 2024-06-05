/* Edge */
#[inline]
pub(crate) fn dot_edge(
    from: &str,
    to: &str,
    label: EdgeLabel,
    color: EdgeColor,
    style: EdgeStyle,
) -> String {
    format!(
        "{} -> {} [label=\"{}\", color={}, style={}, fontsize=10];\n",
        from, to, label, color, style
    )
}

pub(crate) enum EdgeLabel {
    Branch,
    Fallthrough,
    Dom,
}

impl std::fmt::Display for EdgeLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeLabel::Branch => write!(f, "branch"),
            EdgeLabel::Fallthrough => write!(f, "fall-through"),
            EdgeLabel::Dom => write!(f, "dom"),
        }
    }
}

pub(crate) enum EdgeColor {
    Black,
    Blue,
}

impl std::fmt::Display for EdgeColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeColor::Black => write!(f, "black"),
            EdgeColor::Blue => write!(f, "blue"),
        }
    }
}

pub(crate) enum EdgeStyle {
    Solid,
    Dotted,
}

impl std::fmt::Display for EdgeStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeStyle::Solid => write!(f, "solid"),
            EdgeStyle::Dotted => write!(f, "dotted"),
        }
    }
}
