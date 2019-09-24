docker pull daquirm/hs-deps:latest || true

docker build --target dependencies --cache-from daquirm/hs-deps:latest -t daquirm/hs-deps:latest .

docker build --target app --cache-from daquirm/hs-deps:latest -t daquirm/rslang-service .

docker push daquirm/hs-deps:latest
docker push daquirm/rslang-service:latest