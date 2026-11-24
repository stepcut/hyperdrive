//! The benchmark application on ntex: the same routes as ../../Hyperdrive.hs.
//!
//!   bench-ntex PORT [WORKERS]
//!
//! WORKERS (default 8) is the number of threads, as -N8 is for the
//! Haskell servers. ntex pins each worker to a core, among the CPUs the
//! process is allowed to run on.

use ntex::util::Bytes;
use ntex::web::{self, HttpResponse};
use ntex::SharedCfg;

static LARGE: [u8; 1024 * 1024] = [120; 1024 * 1024];

#[web::get("/")]
async fn hello() -> HttpResponse {
    HttpResponse::Ok().content_type("text/plain").body("Hello, World!")
}

#[web::get("/large")]
async fn large() -> HttpResponse {
    HttpResponse::Ok().content_type("text/plain").body(Bytes::from_static(&LARGE))
}

#[web::post("/upload")]
async fn upload(mut body: web::types::Payload) -> Result<HttpResponse, web::error::PayloadError> {
    let mut n = 0;
    while let Some(chunk) = body.recv().await {
        n += chunk?.len();
    }
    Ok(HttpResponse::Ok().content_type("text/plain").body(n.to_string()))
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    let mut args = std::env::args().skip(1);
    let port: u16 = args.next().and_then(|a| a.parse().ok()).expect("usage: bench-ntex PORT [WORKERS]");
    let workers: usize = args.next().map_or(8, |a| a.parse().expect("WORKERS is a number"));
    web::HttpServer::new(async |_| web::App::new().service((hello, large, upload)))
        .workers(workers)
        .bind(("127.0.0.1", port), SharedCfg::new("bench"))?
        .run()
        .await
}
