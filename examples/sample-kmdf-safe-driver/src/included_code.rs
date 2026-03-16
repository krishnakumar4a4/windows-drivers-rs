// This file is brought in via include!(), not as a module

pub fn included_function() {
    trace!(Information, "Trace: from included file with value {}", 777);
}
