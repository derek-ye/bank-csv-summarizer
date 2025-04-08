# Build stage
<<<<<<< HEAD
FROM haskell:9.4 AS build

WORKDIR /app
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup --resolver lts-20.26
COPY . .
RUN stack install
=======
FROM haskellstack/stack:latest as build

WORKDIR /app
COPY stack.yaml package.yaml stack.yaml.lock ./
RUN stack setup
COPY . .
RUN stack build --copy-bins
>>>>>>> 1460e4f5fe79bcb686259f40abff710556428064

# Runtime stage
FROM debian:bullseye-slim

<<<<<<< HEAD
# Install only the runtime dependencies your app needs
=======
# Install runtime dependencies only
>>>>>>> 1460e4f5fe79bcb686259f40abff710556428064
RUN apt-get update && apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

<<<<<<< HEAD
# Copy just the built executable from the build stage
COPY --from=build /root/.local/bin/your-app-name /usr/local/bin/

# Set the entry point
ENTRYPOINT ["bank-transaction-categorizer"]
=======
# Copy the compiled binary from the build stage
COPY --from=build /root/.local/bin /usr/local/bin

# Run your application
CMD ["your-app-name"]
>>>>>>> 1460e4f5fe79bcb686259f40abff710556428064
