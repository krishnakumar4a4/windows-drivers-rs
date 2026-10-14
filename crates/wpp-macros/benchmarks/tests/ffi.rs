// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

#![feature(codeview_annotation)]

include!("..\\support.rs");

#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
struct MockRecord {
    event_id: u32,
    level: u32,
    keyword: u64,
    field_count: u32,
    sizes: [u32; 8],
    decode_guid: [u8; 16],
    payload_len: u32,
    payload: [u8; 256],
}

unsafe extern "C" {
    fn wpp_bench_records(etw: *mut MockRecord, ifr: *mut MockRecord);
}

impl Session {
    fn records(&self) -> (MockRecord, MockRecord) {
        let mut etw = std::mem::MaybeUninit::uninit();
        let mut ifr = std::mem::MaybeUninit::uninit();
        // SAFETY: C initializes both repr(C) records in full under SESSION.
        unsafe {
            wpp_bench_records(etw.as_mut_ptr(), ifr.as_mut_ptr());
            (etw.assume_init(), ifr.assume_init())
        }
    }
}

#[test]
fn registration_filtering_and_cleanup_use_native_mocks() {
    let session = Session::new(true, false, true);
    let stats = session.stats();
    assert_eq!(stats.registrations, 1);
    assert_eq!(stats.set_information, 2);
    assert_eq!(stats.starts, 1);

    // Initialization is idempotent and must not register a second time.
    // SAFETY: IFR is already initialized and the fixture will clean up.
    unsafe { BenchProvider::init() };
    assert_eq!(session.stats().registrations, 1);

    session.enable(4, BenchProvider::BENCHMARK);
    trace!(VERBOSE, BENCHMARK, "filtered by level");
    trace!(INFO, FILTERED, "filtered by keyword");
    assert_eq!(session.stats().etw_writes, 0);
    trace!(INFO, BENCHMARK, "enabled");
    trace!(INFO, "default keyword");
    assert_eq!(session.stats().etw_writes, 2);
    session.enable(0, 0);
    trace!(INFO, BENCHMARK, "disabled");
    assert_eq!(session.stats().etw_writes, 2);
    assert_eq!(session.stats().ifr_writes, 0);

    BenchProvider::clean_up();
    __wpp_ifr::cleanup();
    BenchProvider::clean_up();
    __wpp_ifr::cleanup();
    assert_eq!(session.stats().unregistrations, 1);
    assert_eq!(session.stats().stops, 1);
    assert_eq!(BenchProvider::STATE.reg_handle(), 0);
    assert!(__wpp_ifr::STATE.auto_log_context().is_null());
    assert_eq!(
        BenchProvider::STATE
            .init_state
            .load(std::sync::atomic::Ordering::Relaxed),
        wpp::provider::UNINITIALIZED,
    );
    assert_eq!(
        __wpp_ifr::STATE
            .init_state
            .load(std::sync::atomic::Ordering::Relaxed),
        wpp::ifr::UNINITIALIZED,
    );
    trace!(INFO, BENCHMARK, "after cleanup");
    assert_eq!(session.stats().etw_writes, 2);
    assert_eq!(session.stats().ifr_writes, 0);
}

#[test]
fn etw_descriptors_and_ifr_varargs_have_identical_payloads() {
    let session = Session::new(true, true, true);
    trace!(
        INFO,
        BENCHMARK, "number={}, flag={}, text={}", 42_u64, true, "hello"
    );
    let (etw, ifr) = session.records();
    assert_eq!(etw, ifr);
    assert_eq!(
        etw.event_id,
        u32::from(wpp::field::compute_event_id(
            "number={}, flag={}, text={}",
            &["ItemULongLong", "ItemListByte(false,true)", "ItemString"],
        ))
    );
    assert_eq!(etw.level, 4);
    assert_eq!(etw.keyword, BenchProvider::BENCHMARK);
    assert_eq!(etw.field_count, 3);
    assert_eq!(&etw.sizes[..3], &[8, 1, 6]);
    let mut expected = 42_u64.to_ne_bytes().to_vec();
    expected.push(1);
    expected.extend_from_slice(b"hello\0");
    assert_eq!(etw.payload_len as usize, expected.len());
    assert_eq!(&etw.payload[..expected.len()], expected);
    assert_ne!(etw.decode_guid, [0; 16]);

    // Exercises the other macro expansion and the immediate varargs terminator.
    trace!(INFO, BENCHMARK, "no fields");
    let (empty_etw, empty_ifr) = session.records();
    assert_eq!(empty_etw, empty_ifr);
    assert_eq!(empty_etw.field_count, 0);
    assert_eq!(empty_etw.payload_len, 0);
    assert_eq!(empty_etw.decode_guid, etw.decode_guid);
    assert_eq!(session.stats().etw_writes, 2);
    assert_eq!(session.stats().ifr_writes, 2);
}

#[test]
fn etw_and_ifr_gates_are_independent_across_sessions() {
    for capture in [true, false] {
        for (etw, ifr) in [(false, false), (true, false), (false, true), (true, true)] {
            let session = Session::new(etw, ifr, capture);
            let before = session.stats();
            trace!(INFO, BENCHMARK, "no fields");
            trace!(INFO, BENCHMARK, "value={}", 7_u32);
            session.assert_activity_since(before, etw, ifr);
            assert_eq!(session.stats().etw_writes, if etw { 2 } else { 0 });
            assert_eq!(session.stats().ifr_writes, if ifr { 2 } else { 0 });
        }
    }
}
