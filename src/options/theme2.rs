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

use bat::assets::HighlightingAssets;
use bat::theme::ThemePreference as BatThemePreference;
use syntect::highlighting::Theme as SyntaxTheme;
use syntect::parsing::SyntaxSet;

use crate::cli::{self, DetectDarkLight};
use crate::color::ColorMode;

pub fn choose_theme(options: ThemeOptions, assets: HighlightingAssets) -> ThemeResult {
    todo!()
}

/// All the inputs needed to choose a syntax theme and
/// a color mode (dark or light).
#[derive(Debug, Clone)]
pub struct ThemeOptions {
    /// See: [`crate::cli::Opt::syntax_theme`].
    pub syntax_theme: Option<SyntaxThemePreference>,
    /// See: [`crate::cli::Opt::dark`] / [`crate::cli::Opt::light`].
    pub color_mode: Option<ColorMode>,
    /// See: [`crate::cli::Opt::detect_dark_light`].
    pub detect_dark_light: Option<DetectDarkLight>,
}

/// The result of choosing a syntax theme and a color mode.
#[derive(Debug, Clone)]
pub struct ThemeResult {
    pub syntax_theme: SyntaxTheme,
    pub syntax_set: SyntaxSet,
    pub color_mode: ColorMode,
}

/// The choice of syntax theme.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntaxThemePreference {
    /// Use one of bat's themes.
    Bat(BatThemePreference),
    /// An explicit request to disable syntax highlighting.
    Disable(String),
}
