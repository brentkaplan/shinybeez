#!/bin/bash
# ============================================================================
# Update Docker Images Script
# ============================================================================
# Quick script to pull the latest Docker images on your server.
# Run this after pushing new images to Docker Hub.
#
# Usage:
#   chmod +x update-images.sh
#   ./update-images.sh
# ============================================================================

set -e

echo "🔄 Pulling latest Docker images..."

docker pull brentkaplan/shinybeez:latest
docker pull brentkaplan/shinybeez:develop

echo ""
echo "✅ Images updated!"
echo ""
echo "Current images:"
docker images | grep shinybeez

echo ""
echo "Note: New user sessions will automatically use the updated images."
echo "Existing sessions will continue using the old image until they end."
echo ""
echo "To force all users to get the new version, restart ShinyProxy:"
echo "  sudo systemctl restart shinyproxy"
