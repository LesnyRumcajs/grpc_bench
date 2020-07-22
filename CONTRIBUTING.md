# Contributing

Please note we have a code of conduct, please follow it in all your interactions with the project.

## Pull Request Process

1. All new benchmarks must be performed through docker, no additional dependencies must be required.
2. Ensure all docker images needed by the global benchmark suite are built in `build.sh`.
3. Add new benchmarks to the global suite in `bench.sh`.
4. Ensure all docker images built are removed by the `clean.sh` script.
5. Update the README.md with details of changes to the interface, this includes new environment variables, exposed ports, useful file locations and container parameters.
6. You may merge the Pull Request in once you have the sign-off of one contributor, or if you do not have permission to do that, you may request the reviewer to merge it for you.
