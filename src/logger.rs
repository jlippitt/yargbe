#[cfg(feature="logging")]
use log::{self, Log, LogRecord, LogLevelFilter, LogMetadata, MaxLogLevelFilter, SetLoggerError};
#[cfg(feature="logging")]
use std::env;

#[cfg(feature="logging")]
pub struct LogControl {
    log_level: MaxLogLevelFilter,
    env_filter: LogLevelFilter,
    trace_mode: bool
}

#[cfg(not(feature="logging"))]
pub struct LogControl;

#[cfg(feature="logging")]
struct Logger;

#[cfg(feature="logging")]
impl LogControl {
    pub fn new(log_level: MaxLogLevelFilter, env_filter: LogLevelFilter) -> LogControl {
        LogControl {
            log_level: log_level,
            env_filter: env_filter,
            trace_mode: false
        }
    }

    pub fn toggle_trace_mode(&mut self) {
        self.trace_mode = !self.trace_mode;

        if self.trace_mode {
            self.log_level.set(LogLevelFilter::Trace);
        } else {
            self.log_level.set(self.env_filter);
        }
    }
}

#[cfg(not(feature="logging"))]
impl LogControl {
    pub fn toggle_trace_mode(&mut self) {
        // Do nothing
    }
}

#[cfg(feature="logging")]
impl Log for Logger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        true
    }
    
    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            println!("{}", record.args());
        }
    }
}

#[cfg(feature="logging")]
pub fn init() -> Result<LogControl, SetLoggerError> {
    let env_log_level = match env::var("LOG_LEVEL") {
        Ok(level) => match level.to_uppercase().as_str() {
            "ERROR" => LogLevelFilter::Error,
            "WARN" => LogLevelFilter::Warn,
            "INFO" => LogLevelFilter::Info,
            "DEBUG" => LogLevelFilter::Debug,
            "TRACE" => LogLevelFilter::Trace,
            _ => panic!("Invalid log level: {}", level)
        },
        Err(_) => LogLevelFilter::Info // Default
    };

    let mut log_control: Option<LogControl> = None;

    try!(log::set_logger(|max_log_level| {
        max_log_level.set(env_log_level);
        log_control = Some(LogControl::new(max_log_level, env_log_level));
        Box::new(Logger)
    }));

    Ok(log_control.unwrap())
}

#[cfg(not(feature="logging"))]
pub fn init() -> Result<LogControl, ()> {
    Ok(LogControl)
}

#[cfg(not(feature="logging"))]
#[macro_export]
macro_rules! error {
    (target: $target:expr, $($arg:tt)*) => {{}};
    ($($arg:tt)*) => {{}};
}

#[cfg(not(feature="logging"))]
#[macro_export]
macro_rules! warn {
    (target: $target:expr, $($arg:tt)*) => {{}};
    ($($arg:tt)*) => {{}};
}

#[cfg(not(feature="logging"))]
#[macro_export]
macro_rules! info {
    (target: $target:expr, $($arg:tt)*) => {{}};
    ($($arg:tt)*) => {{}};
}

#[cfg(not(feature="logging"))]
#[macro_export]
macro_rules! debug {
    (target: $target:expr, $($arg:tt)*) => {{}};
    ($($arg:tt)*) => {{}};
}

#[cfg(not(feature="logging"))]
#[macro_export]
macro_rules! trace {
    (target: $target:expr, $($arg:tt)*) => {{}};
    ($($arg:tt)*) => {{}};
}
