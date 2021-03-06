mod loader;
pub use loader::*;

use bevy_app::prelude::*;
use bevy_asset::AddAsset;

/// Adds support for Obj file loading to Apps
#[derive(Default)]
pub struct PlyPlugin;

impl Plugin for PlyPlugin {
    fn build(&self, app: &mut AppBuilder) {
        app.init_asset_loader::<PlyLoader>();
    }
}
