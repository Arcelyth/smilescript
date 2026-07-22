mod common;
use test_generator::test_resources;

#[test_resources("./tests/sms/*.sms")]
fn verify_script(path: &str) {
    println!("Testing script: {}", path);
    common::should_ok(path);
}
