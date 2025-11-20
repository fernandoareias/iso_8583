.PHONY: all compile clean test dialyzer docs shell format check

# Default target
all: compile

# Compile the project
compile:
	@echo "Compiling ISO 8583 library..."
	@rebar3 compile

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rebar3 clean
	@rm -rf _build ebin doc

# Run tests
test: compile
	@echo "Running tests..."
	@rebar3 test

# Run dialyzer
dialyzer: compile
	@echo "Running dialyzer..."
	@rebar3 dialyzer

# Generate documentation
docs:
	@echo "Generating documentation..."
	@rebar3 edoc

# Start Erlang shell with the application loaded
shell: compile
	@echo "Starting Erlang shell..."
	@rebar3 shell

# Format code
format:
	@echo "Formatting code..."
	@rebar3 format

# Check code formatting
check:
	@echo "Checking code format..."
	@rebar3 format --verify

# Full check (compile, format check, tests, dialyzer)
full-check: clean compile check test dialyzer
	@echo "All checks passed!"

# Help
help:
	@echo "Available targets:"
	@echo "  all         - Compile the project (default)"
	@echo "  compile     - Compile the project"
	@echo "  clean       - Clean build artifacts"
	@echo "  test        - Run tests"
	@echo "  dialyzer    - Run dialyzer"
	@echo "  docs        - Generate documentation"
	@echo "  shell       - Start Erlang shell"
	@echo "  format      - Format code"
	@echo "  check       - Check code formatting"
	@echo "  full-check  - Run all checks"
	@echo "  help        - Show this help"
