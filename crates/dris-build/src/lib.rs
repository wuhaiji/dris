#![doc = include_str!("../README.md")]

mod cargo;
mod codegen;
mod graph;
mod inject;
mod model;
mod pipeline;
mod resolve;
mod scan;
mod type_util;
mod validate;

pub fn generate() -> anyhow::Result<()> {
    pipeline::generate()
}
