# Build stage
FROM haskell:9.2 AS build

WORKDIR /app
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup --resolver lts-20.26
COPY . .
RUN stack install

# Runtime stage
FROM debian:bullseye-slim

# Install only the runtime dependencies your app needs
RUN apt-get update && apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy just the built executable from the build stage
COPY --from=build /root/.local/bin/your-app-name /usr/local/bin/

# Set the entry point
ENTRYPOINT ["bank-transaction-categorizer"]