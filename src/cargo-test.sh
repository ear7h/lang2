#!/usr/bin/env sh
cargo test --color=always "$@" | ./cargo-test-filter.awk
