# Development specifications

## Versioning

This configuration is not gonna follow semver, instead, it use calendar versioning and
release new version in each weekend.
Version will be released in `cvYYYY.0M.0D` format.

## Benchmark

I've write a Perl script to handle the benchmark.

```bash
perl ./fixtures/benchmark.pl
```

It will write result into the `./fixtures/benchmark.txt` file.

## RELEASE.md

If any of the commits contains API compatibility changes, we must write them into
RELEASE.md file for the next version release.
