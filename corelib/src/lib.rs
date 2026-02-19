#![allow(non_snake_case)]
// use libc::printf;
// use std::ffi::CString;
use std::slice;

// ####################### PRINT ##########################

// Only print a return
#[unsafe(export_name = "builtin.printEmpty")]
pub extern "C" fn printEmpty() {
    // unsafe {
    //     let empty = CString::new("").unwrap();
    //     libc::puts(empty.as_ptr());
    // }
    println!("");
}

#[unsafe(export_name = "builtin.printInt")]
pub extern "C" fn printInt(int: i64) {
    // unsafe {
    //     let fmt = CString::new("%ld\n").unwrap();
    //     printf(fmt.as_ptr(), int);
    // }
    println!("{}", int);
}

#[unsafe(export_name = "builtin.printDec")]
pub extern "C" fn printDec(dec: f64) {
    // unsafe {
    //     let fmt = CString::new("%g\n").unwrap();
    //     printf(fmt.as_ptr(), dec);
    // }
    println!("{}", dec);
}

#[unsafe(export_name = "builtin.printBool")]
pub extern "C" fn printBool(boolean: bool) {
    // unsafe {
    //     let boolStr;
    //     if boolean {
    //         boolStr = CString::new("true").unwrap();
    //     } else {
    //         boolStr = CString::new("false").unwrap();
    //     }
    //     libc::puts(boolStr.as_ptr()); // Also add \n
    // }
    println!("{}", boolean);
}

#[unsafe(export_name = "builtin.printStr")]
pub extern "C" fn printStr(ptr: *const u8, len: usize) {
    if ptr.is_null() || len == 0 {
        println!("[Runtime Error] builtin.printStr: Empty Str");
        return;
    }

    unsafe {
        let bytes = slice::from_raw_parts(ptr, len);
        if let Ok(s) = str::from_utf8(bytes) {
            println!("{}", s);
            // let cstr = CString::new(s).unwrap();
            // libc::puts(cstr.as_ptr()); // Also add \n
        } else {
            // let errMsg = CString::new("[runtime] Invalid UTF-8 sequence").unwrap();
            // libc::puts(errMsg.as_ptr()); // Also add \n
            println!("[Runtime Error] builtin.printStr: Invalid UTF-8 sequence");
        }
    }
}

// ####################### PANIC ##########################

#[unsafe(export_name = "builtin.panicWithMsg")]
pub extern "C" fn panicWithMsg(message: *const u8, len: usize) -> ! {
    let slice = unsafe { std::slice::from_raw_parts(message, len) };
    let s = std::str::from_utf8(slice).unwrap_or("<invalid utf8>");
    eprintln!("[Programm panic] {}", s);
    std::process::abort();
}

#[unsafe(export_name = "builtin.panicWithoutMsg")]
pub extern "C" fn panicWithoutMsg() -> ! {
    eprintln!("[Programm panic]");
    std::process::abort();
}
