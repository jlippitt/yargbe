use getopts::Options;
use std::env;

const DEFAULT_SCALE: u32 = 1;

pub enum Action {
    Run(Config),
    ShowUsage(String),
    ShowError(String)
}

pub struct Config {
    pub path: String,
    pub scale: u32
}

pub fn parse() -> Action {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();

    opts.optopt(
        "s",
        "scale",
        "Upscales the display by a factor of N in each direction. The default is 1 (i.e. no scaling).",
        "N"
    );

    opts.optflag(
        "h",
        "help",
        "Displays usage information."
    );

    let matches = match opts.parse(&args[1..]) {
        Ok(matches) => matches,
        Err(err) => return Action::ShowError(err.to_string())
    };

    if matches.opt_present("h") {
        let brief = format!("Usage: {} [options] ROM_PATH", args[0]);
        return Action::ShowUsage(opts.usage(&brief));
    };

    let path = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        return Action::ShowError("No ROM path specified".to_string())
    };

    let scale = match matches.opt_str("s") {
        Some(scale) => match scale.parse::<u32>() {
            Ok(scale) => scale,
            Err(err) => return Action::ShowError(err.to_string())
        },
        None => DEFAULT_SCALE
    };

    Action::Run(Config {
        path: path,
        scale: scale
    })
}
