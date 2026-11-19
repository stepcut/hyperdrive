-- 1 MiB uploads
wrk.method = "POST"
wrk.path = "/upload"
wrk.body = string.rep("x", 1048576)
wrk.headers["Content-Type"] = "application/octet-stream"
