DEB_BUILD_OPTIONS="parallel=$(nproc) nocheck" gbp buildpackage -us -uc -B "$@"
