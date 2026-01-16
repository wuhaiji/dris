use std::{
    collections::HashMap,
    net::SocketAddr,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
};

use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    middleware,
    response::{IntoResponse, Response},
    routing::get,
};
use dris_rt::{component, constructor};
use serde::{Deserialize, Serialize};
use tower_http::trace::TraceLayer;

#[derive(Debug, Clone, Serialize)]
pub struct User {
    pub id: u64,
    pub username: String,
}

#[derive(Debug, Deserialize)]
pub struct CreateUserRequest {
    pub username: String,
}

#[derive(Debug, Deserialize)]
pub struct UpdateUserRequest {
    pub username: String,
}

#[component(singleton)]
pub struct ShutdownSwitch {
    shutting_down: AtomicBool,
}

impl ShutdownSwitch {
    #[constructor]
    pub fn new() -> Self {
        Self {
            shutting_down: AtomicBool::new(false),
        }
    }

    pub fn is_shutting_down(&self) -> bool {
        self.shutting_down.load(Ordering::Relaxed)
    }

    pub fn prepare_shutdown(&self) {
        self.shutting_down.store(true, Ordering::Relaxed);
    }
}

#[component(singleton)]
pub struct AppConfig {
    pub addr: SocketAddr,
}

impl AppConfig {
    #[constructor]
    pub fn new() -> Self {
        let addr = std::env::var("BIND_ADDR")
            .ok()
            .and_then(|s| s.parse::<SocketAddr>().ok())
            .unwrap_or_else(|| "127.0.0.1:3000".parse().expect("默认地址必须可解析"));

        Self { addr }
    }
}

#[async_trait::async_trait]
pub trait UserRepository: Send + Sync {
    async fn list(&self) -> Vec<User>;
    async fn get(&self, id: u64) -> Option<User>;
    async fn create(&self, username: String) -> User;
    async fn update(&self, id: u64, username: String) -> Option<User>;
    async fn delete(&self, id: u64) -> bool;
}

#[component(singleton)]
pub struct InMemoryUserRepo {
    next_id: AtomicU64,
    users: tokio::sync::RwLock<HashMap<u64, User>>,
}

impl InMemoryUserRepo {
    #[constructor]
    pub fn new() -> Self {
        Self {
            next_id: AtomicU64::new(1),
            users: tokio::sync::RwLock::new(HashMap::new()),
        }
    }
}

#[async_trait::async_trait]
impl UserRepository for InMemoryUserRepo {
    async fn list(&self) -> Vec<User> {
        let users = self.users.read().await;
        users.values().cloned().collect()
    }

    async fn get(&self, id: u64) -> Option<User> {
        let users = self.users.read().await;
        users.get(&id).cloned()
    }

    async fn create(&self, username: String) -> User {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let user = User { id, username };
        let mut users = self.users.write().await;
        users.insert(id, user.clone());
        user
    }

    async fn update(&self, id: u64, username: String) -> Option<User> {
        let mut users = self.users.write().await;
        let u = users.get_mut(&id)?;
        u.username = username;
        Some(u.clone())
    }

    async fn delete(&self, id: u64) -> bool {
        let mut users = self.users.write().await;
        users.remove(&id).is_some()
    }
}

#[component(singleton)]
pub struct UserService {
    repo: Arc<dyn UserRepository>,
}

impl UserService {
    #[constructor]
    pub fn new(repo: Arc<dyn UserRepository>) -> Self {
        Self { repo }
    }

    pub async fn list(&self) -> Vec<User> {
        self.repo.list().await
    }

    pub async fn get(&self, id: u64) -> Option<User> {
        self.repo.get(id).await
    }

    pub async fn create(&self, username: String) -> User {
        self.repo.create(username).await
    }

    pub async fn update(&self, id: u64, username: String) -> Option<User> {
        self.repo.update(id, username).await
    }

    pub async fn delete(&self, id: u64) -> bool {
        self.repo.delete(id).await
    }
}

#[component(singleton)]
pub struct AppState {
    users: Arc<UserService>,
    shutdown: Arc<ShutdownSwitch>,
}

impl AppState {
    #[constructor]
    pub fn new(users: Arc<UserService>, shutdown: Arc<ShutdownSwitch>) -> Self {
        Self { users, shutdown }
    }
}

#[component]
pub struct App {
    pub cfg: Arc<AppConfig>,
    pub shutdown: Arc<ShutdownSwitch>,
    pub router: Router,
}

impl App {
    #[constructor]
    pub fn new(cfg: Arc<AppConfig>, shutdown: Arc<ShutdownSwitch>, router: AppRouter) -> Self {
        Self {
            cfg,
            shutdown,
            router: router.0,
        }
    }
}

#[component]
pub struct AppRouter(pub Router);

impl AppRouter {
    #[constructor]
    pub fn new(state: Arc<AppState>) -> Self {
        let app = Router::new()
            .route("/health", get(health))
            .route("/users", get(list_users).post(create_user))
            .route(
                "/users/:id",
                get(get_user).put(update_user).delete(delete_user),
            )
            .with_state(state.clone())
            .layer(middleware::from_fn_with_state(
                state.clone(),
                reject_when_shutting_down,
            ))
            .layer(TraceLayer::new_for_http());
        Self(app)
    }
}

async fn reject_when_shutting_down(
    State(state): State<Arc<AppState>>,
    req: axum::http::Request<axum::body::Body>,
    next: middleware::Next,
) -> Response {
    if state.shutdown.is_shutting_down() {
        return (StatusCode::SERVICE_UNAVAILABLE, "shutting down").into_response();
    }
    next.run(req).await
}

async fn health() -> &'static str {
    "ok"
}

async fn list_users(State(state): State<Arc<AppState>>) -> Json<Vec<User>> {
    Json(state.users.list().await)
}

async fn create_user(
    State(state): State<Arc<AppState>>,
    Json(req): Json<CreateUserRequest>,
) -> Json<User> {
    Json(state.users.create(req.username).await)
}

async fn get_user(
    Path(id): Path<u64>,
    State(state): State<Arc<AppState>>,
) -> Result<Json<User>, StatusCode> {
    match state.users.get(id).await {
        Some(user) => Ok(Json(user)),
        None => Err(StatusCode::NOT_FOUND),
    }
}

async fn update_user(
    Path(id): Path<u64>,
    State(state): State<Arc<AppState>>,
    Json(req): Json<UpdateUserRequest>,
) -> Result<Json<User>, StatusCode> {
    match state.users.update(id, req.username).await {
        Some(user) => Ok(Json(user)),
        None => Err(StatusCode::NOT_FOUND),
    }
}

async fn delete_user(Path(id): Path<u64>, State(state): State<Arc<AppState>>) -> StatusCode {
    if state.users.delete(id).await {
        StatusCode::NO_CONTENT
    } else {
        StatusCode::NOT_FOUND
    }
}

include!(concat!(env!("OUT_DIR"), "/dris_gen.rs"));

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    let container = dris_gen::Container::build();

    let app = container.app();

    tracing::info!(addr=%app.cfg.addr, "listening");
    let listener = tokio::net::TcpListener::bind(app.cfg.addr).await.unwrap();

    let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel::<()>();
    let shutdown_switch = app.shutdown.clone();
    tokio::spawn({
        async move {
            tokio::signal::ctrl_c().await.unwrap();
            shutdown_switch.prepare_shutdown();
            let _ = shutdown_tx.send(());
        }
    });

    axum::serve(listener, app.router)
        .with_graceful_shutdown(async {
            let _ = shutdown_rx.await;
        })
        .await
        .unwrap();
}
