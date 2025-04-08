# Build stage
FROM haskellstack/stack:latest as build

WORKDIR /app
COPY stack.yaml package.yaml stack.yaml.lock ./
RUN stack setup
COPY . .
RUN stack build --copy-bins

# Runtime stage
FROM debian:bullseye-slim

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy the compiled binary from the build stage
COPY --from=build /root/.local/bin /usr/local/bin

# Run your application
CMD ["your-app-name"]