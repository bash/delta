//! Delta doesn't have a formal concept of a "theme". What it has is
//!
//! 1. The choice of "theme". This is the language syntax highlighting theme; you have to make this
//!    choice when using `bat` also.
//! 2. The choice of "light vs dark mode". This determines whether the background colors should be
//!    chosen for a light or dark terminal background. (`bat` has no equivalent.)
//!
//! Basically:
//! 1. The theme is specified by the `--syntax-theme` option. If this isn't supplied then it is specified
//!    by the `BAT_THEME` environment variable.
//! 2. Light vs dark mode is specified by the `--light` or `--dark` options. If these aren't
//!    supplied then it detected from the terminal. If this fails it is inferred from the chosen theme.
//!
//! In the absence of other factors, the default assumes a dark terminal background.

use bat::theme::{
    DetectColorScheme as BatDetectColorScheme, ThemeName as BatThemeName,
    ThemeOptions as BatThemeOptions, ThemePreference as BatThemePreference,
};

use crate::cli::DetectDarkLight;
use crate::color::ColorMode;

pub fn choose_theme(options: ThemeOptions) -> ThemeResult {
    match bat_theme_preference(&options) {
        None => {
            // TODO: detect, respect opt.color_only
            ThemeResult::no_syntax_highlighting(options.color_mode.unwrap_or_default())
        }
        Some(bat_theme) => {
            // TODO: detect, respect opt.color_only
            let bat_options = BatThemeOptions {
                theme: bat_theme,
                theme_dark: options.syntax_theme_dark,
                theme_light: options.syntax_theme_light,
            };
            let result = bat::theme::theme(bat_options);
            todo!()
        }
    }
}

fn bat_theme_preference(options: &ThemeOptions) -> Option<BatThemePreference> {
    if let SyntaxThemePreference::Disable(_) = &options.syntax_theme {
        None
    } else if let Some(color_mode) = options.color_mode {
        Some(color_mode.into())
    } else if let (
        SyntaxThemePreference::Bat(BatThemePreference::Auto(_)),
        Some(detect_dark_light),
    ) = (&options.syntax_theme, options.detect_dark_light)
    {
        use DetectDarkLight::*;
        match detect_dark_light {
            Auto => Some(BatThemePreference::Auto(BatDetectColorScheme::Auto)),
            Always => Some(BatThemePreference::Auto(BatDetectColorScheme::Always)),
            Never => Some(BatThemePreference::Dark),
        }
    } else if let SyntaxThemePreference::Bat(bat_theme) = &options.syntax_theme {
        Some(bat_theme.clone())
    } else {
        unreachable!()
    }
}

/// All the inputs needed to choose a syntax theme and
/// a color mode (dark or light).
#[derive(Debug, Clone)]
pub struct ThemeOptions {
    /// See: [`crate::cli::Opt::syntax_theme`].
    pub syntax_theme: SyntaxThemePreference,
    pub syntax_theme_dark: Option<BatThemeName>,
    pub syntax_theme_light: Option<BatThemeName>,
    /// See: [`crate::cli::Opt::dark`] / [`crate::cli::Opt::light`].
    pub color_mode: Option<ColorMode>,
    /// See: [`crate::cli::Opt::detect_dark_light`].
    pub detect_dark_light: Option<DetectDarkLight>,
}

/// The choice of syntax theme.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntaxThemePreference {
    /// Use one of bat's themes.
    Bat(BatThemePreference),
    /// An explicit request to disable syntax highlighting.
    Disable(String),
}

impl Default for SyntaxThemePreference {
    fn default() -> Self {
        SyntaxThemePreference::Bat(BatThemePreference::default())
    }
}

impl SyntaxThemePreference {
    /// Creates a theme preference from a string.
    pub fn new(s: impl Into<String>) -> Self {
        let s = s.into();
        if is_no_syntax_highlighting_syntax_theme_name(&s) {
            SyntaxThemePreference::Disable(s)
        } else {
            SyntaxThemePreference::Bat(BatThemePreference::new(s))
        }
    }
}

impl From<ColorMode> for BatThemePreference {
    fn from(value: ColorMode) -> Self {
        match value {
            ColorMode::Dark => BatThemePreference::Dark,
            ColorMode::Light => BatThemePreference::Light,
        }
    }
}

/// The result of choosing a syntax theme and a color mode.
#[derive(Debug, Clone)]
pub struct ThemeResult {
    pub syntax_theme: Option<SyntaxThemeName>,
    pub color_mode: ColorMode,
}

impl ThemeResult {
    pub fn no_syntax_highlighting(color_mode: ColorMode) -> Self {
        ThemeResult {
            color_mode,
            syntax_theme: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxThemeName(pub String);

fn is_no_syntax_highlighting_syntax_theme_name(theme_name: &str) -> bool {
    theme_name.to_lowercase() == "none"
}
