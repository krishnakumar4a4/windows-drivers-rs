# Sample KMDF Safe Rust Driver

A sample driver written in 100% safe Rust demonstrating request processing and cancellation.

When a write request arrives it stores the request in context object and starts a timer. When the timer fires it completes the request. This simulates I/O processing on real hardware. At any time before its completion the request can be cancelled.

This sample shows how Rust:
- Enforces a lock on the request object and thus eliminates concurrency issues between completion and cancellation.
- Removes the possibility of double completions through its unique ownership semantics
- Ensures all locks are automatically released when the surrounding scope ends
- Ensures that only the correct type of arguments are passed to functions with its stronger typing
- Eliminates any possibility of null or dangling pointers anywhere in the code

## Build
### Prerequisites

* WDK environment (either via eWDK or installed WDK)
* LLVM

Once prerequests are installed, run `cargo make` in this directory to build and package the driver.

## Install

1. Copy the following to the DUT (Device Under Test: the computer you want to test the driver on):
   1. The driver `package` folder located in the [Cargo Output Directory](https://doc.rust-lang.org/cargo/guide/build-cache.html). The Cargo Output Directory changes based off of build profile, target architecture, etc.
     * Ex. `<REPO_ROOT>\target\x86_64-pc-windows-msvc\debug\package`, `<REPO_ROOT>\target\x86_64-pc-windows-msvc\release\package`, `<REPO_ROOT>\target\aarch64-pc-windows-msvc\debug\package`, `<REPO_ROOT>\target\aarch64-pc-windows-msvc\release\package`,
     `<REPO_ROOT>\target\debug\package`,
     `<REPO_ROOT>\target\release\package`
   2. The version of `devgen.exe` from the WDK Developer Tools that matches the archtecture of your DUT
     * Ex. `C:\Program Files\Windows Kits\10\Tools\10.0.22621.0\x64\devgen.exe`. Note: This path will vary based off your WDK environment
2. Install the Certificate on the DUT:
   1. Double click the certificate
   2. Click Install Certificate
   3. Store Location: Local Machine -> Next
   4. Place all certificates in the following Store -> Browse -> Trusted Root Certification Authorities -> Ok -> Next
   5. Repeat 2-4 for Store -> Browse -> Trusted Publishers -> Ok -> Next
   6. Finish
3. Install the driver:
   * In the package directory, run: `pnputil.exe /add-driver sample_kmdf_safe_driver.inf /install`
4. Create a software device:
   * In the directory that `devgen.exe` was copied to, run: `devgen.exe /add /hardwareid "root\SAMPLE_KMDF_SAFE_HW_ID"`

## Test

In order to see the driver in action you will have to send I/O requests to it. For that use the `sample-test` application located at `../sample-test` relative to this directory.

Build that app by executing `cargo build` and then run it as below:
* Copy the binary `sample-test.exe` to the DUT
* Make sure you have printing enabled in DebugView or WinDbg (see below for how to do that)
* Run the command `sample-test.exe 2aa02ab1-c26e-431b-8efe-85ee8de102e4`.

You will see the driver print that it has received the request and after a few seconds it will print lines indicating that the timer has fired and the request is completed

If you want to cancel the request, press CTRL+C in the console where you are running `sample-test.exe` and you should see the driver indicating that the request was cancelled. The timer will still fire, but this time it will print that the request is already completed and not try to complete it.


### Capturing Print Lines
To capture prints:
* Start [DebugView](https://learn.microsoft.com/en-us/sysinternals/downloads/debugview)
  1. Enable `Capture Kernel`
  2. Enable `Enable Verbose Kernel Output`
* Alternatively, you can see prints in an active Windbg session.
  1. Attach WinDbg
  2. `ed nt!Kd_DEFAULT_Mask 0xFFFFFFFF`

