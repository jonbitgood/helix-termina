// CREDIT: this is a quite shallow copy of <https://github.com/wezterm/wezterm/blob/a87358516004a652ad840bc1661bdf65ffc89b43/termwiz/src/escape/osc.rs>.
// I've replaced some macros and the base64 implementation however, as well as make the commands
// borrow a `str` instead of own a `String`.

use std::fmt::{self, Display};

use crate::{base64, style::RgbColor};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Osc<'a> {
    SetIconNameAndWindowTitle(&'a str),
    SetWindowTitle(&'a str),
    SetWindowTitleSun(&'a str),
    SetIconName(&'a str),
    SetIconNameSun(&'a str),
    ClearSelection(Selection),
    QuerySelection(Selection),
    SetSelection(Selection, &'a str),
    ChangeDynamicColors(DynamicColorNumber, Vec<ColorOrQuery>),
    ResetDynamicColor(DynamicColorNumber),
    // TODO: I didn't copy many available commands yet...
}

impl Display for Osc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(super::OSC)?;
        match self {
            Self::SetIconNameAndWindowTitle(s) => write!(f, "0;{s}")?,
            Self::SetWindowTitle(s) => write!(f, "2;{s}")?,
            Self::SetWindowTitleSun(s) => write!(f, "l{s}")?,
            Self::SetIconName(s) => write!(f, "1;{s}")?,
            Self::SetIconNameSun(s) => write!(f, "L{s}")?,
            Self::ClearSelection(selection) => write!(f, "52;{selection}")?,
            Self::QuerySelection(selection) => write!(f, "52;{selection};?")?,
            Self::SetSelection(selection, content) => {
                // TODO: it'd be nice to avoid allocating a string to base64 encode.
                write!(f, "52;{selection};{}", base64::encode(content.as_bytes()))?
            }
            Self::ChangeDynamicColors(color, colors) => {
                write!(f, "{}", *color as u8)?;
                for color in colors {
                    write!(f, ";{color}")?
                }
            }
            Self::ResetDynamicColor(color) => write!(f, "{}", 100 + *color as u8)?,
        }
        f.write_str(super::ST)?;
        Ok(())
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Selection : u16 {
        const NONE = 0;
        const CLIPBOARD = 1<<1;
        const PRIMARY=1<<2;
        const SELECT=1<<3;
        const CUT0=1<<4;
        const CUT1=1<<5;
        const CUT2=1<<6;
        const CUT3=1<<7;
        const CUT4=1<<8;
        const CUT5=1<<9;
        const CUT6=1<<10;
        const CUT7=1<<11;
        const CUT8=1<<12;
        const CUT9=1<<13;
    }
}

impl Display for Selection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.contains(Self::CLIPBOARD) {
            write!(f, "c")?;
        }
        if self.contains(Self::PRIMARY) {
            write!(f, "p")?;
        }
        if self.contains(Self::SELECT) {
            write!(f, "s")?;
        }
        if self.contains(Self::CUT0) {
            write!(f, "0")?;
        }
        if self.contains(Self::CUT1) {
            write!(f, "1")?;
        }
        if self.contains(Self::CUT2) {
            write!(f, "2")?;
        }
        if self.contains(Self::CUT3) {
            write!(f, "3")?;
        }
        if self.contains(Self::CUT4) {
            write!(f, "4")?;
        }
        if self.contains(Self::CUT5) {
            write!(f, "5")?;
        }
        if self.contains(Self::CUT6) {
            write!(f, "6")?;
        }
        if self.contains(Self::CUT7) {
            write!(f, "7")?;
        }
        if self.contains(Self::CUT8) {
            write!(f, "8")?;
        }
        if self.contains(Self::CUT9) {
            write!(f, "9")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DynamicColorNumber {
    TextForegroundColor = 10,
    TextBackgroundColor = 11,
    TextCursorColor = 12,
    MouseForegroundColor = 13,
    MouseBackgroundColor = 14,
    TektronixForegroundColor = 15,
    TektronixBackgroundColor = 16,
    HighlightBackgroundColor = 17,
    TektronixCursorColor = 18,
    HighlightForegroundColor = 19,
}

impl DynamicColorNumber {
    pub(crate) fn from_index(index: u8) -> Option<Self> {
        match index {
            10 => Some(Self::TextForegroundColor),
            11 => Some(Self::TextBackgroundColor),
            12 => Some(Self::TextCursorColor),
            13 => Some(Self::MouseForegroundColor),
            14 => Some(Self::MouseBackgroundColor),
            15 => Some(Self::TektronixForegroundColor),
            16 => Some(Self::TektronixBackgroundColor),
            17 => Some(Self::HighlightBackgroundColor),
            18 => Some(Self::TektronixCursorColor),
            19 => Some(Self::HighlightForegroundColor),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorOrQuery {
    Color(RgbColor),
    Query,
}

impl Display for ColorOrQuery {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ColorOrQuery::Query => write!(f, "?"),
            ColorOrQuery::Color(c) => {
                // rgb:RRRR/GGGG/BBBB
                write!(
                    f,
                    "rgb:{red:02x}{red:02x}/{green:02x}{green:02x}/{blue:02x}{blue:02x}",
                    red = c.red,
                    green = c.green,
                    blue = c.blue
                )
            }
        }
    }
}

impl From<RgbColor> for ColorOrQuery {
    fn from(color: RgbColor) -> Self {
        Self::Color(color)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn encoding() {
        // OSC 11 query, asks the terminal for the background color.
        // <https://terminalguide.namepad.de/seq/osc-11/>
        // <https://terminalguide.namepad.de/seq/osc-4/>
        assert_eq!(
            "\x1b]11;?\x1b\\",
            Osc::ChangeDynamicColors(
                DynamicColorNumber::TextBackgroundColor,
                vec![ColorOrQuery::Query]
            )
            .to_string()
        );

        assert_eq!(
            "\x1b]11;rgb:2828/2828/2828\x1b\\",
            Osc::ChangeDynamicColors(
                DynamicColorNumber::TextBackgroundColor,
                vec![RgbColor::new(40, 40, 40).into()]
            )
            .to_string()
        );
    }
}
