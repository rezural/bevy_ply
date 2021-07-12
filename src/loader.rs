use anyhow::Result;
use bevy_asset::{AssetLoader, LoadContext, LoadedAsset};
use bevy_render::{
    mesh::{Indices, Mesh, VertexAttributeValues},
    pipeline::PrimitiveTopology,
};
use bevy_utils::BoxedFuture;
use bytes::Buf;
use ply_rs::parser;
use ply_rs::ply;
use thiserror::Error;
#[derive(Default)]
pub struct PlyLoader;

impl AssetLoader for PlyLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut bevy_asset::LoadContext,
    ) -> BoxedFuture<'a, Result<(), anyhow::Error>> {
        Box::pin(async move { Ok(load_obj(bytes, load_context).await?) })
    }

    fn extensions(&self) -> &[&str] {
        static EXTENSIONS: &[&str] = &["ply"];
        EXTENSIONS
    }
}

#[derive(Error, Debug)]
pub enum ObjError {
    // #[error("Invalid Ply file.")]
    // Gltf(#[from] obj::ObjError),
    #[error("Unknown vertex format.")]
    UnknownVertexFormat,
}

async fn load_obj<'a, 'b>(
    bytes: &'a [u8],
    load_context: &'a mut LoadContext<'b>,
) -> Result<(), ObjError> {
    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
    load_obj_from_bytes(bytes, &mut mesh)?;
    load_context.set_default_asset(LoadedAsset::new(mesh));
    Ok(())
}

fn load_obj_from_bytes(
    bytes: &[u8],
    mesh: &mut Mesh,
) -> Result<(), ObjError> {
    let mut reader = bytes.reader();

    // Create a parser for each struct. Parsers are cheap objects.
    let vertex_parser = parser::Parser::<VertexWithNormal>::new();
    let face_parser = parser::Parser::<Face>::new();

    // lets first consume the header
    // We also could use `face_parser`, The configuration is a parser's only state.
    // The reading position only depends on `f`.
    let header = vertex_parser.read_header(&mut reader);

    let mut vertex_normal_list = Vec::new();
    let mut face_list = Vec::new();

    if let Ok(header) = &header {
        for (_, element) in &header.elements {
            match element.name.as_ref() {
                "vertex" => {
                    let vnl =
                        vertex_parser.read_payload_for_element(&mut reader, &element, &header);
                    match vnl {
                        Ok(vnl) => vertex_normal_list = vnl,
                        Err(e) => println!("Vertex Err: {:?}", e),
                    }
                }
                "face" => {
                    match face_parser.read_payload_for_element(&mut reader, &element, &header) {
                        Ok(vnl) => face_list = vnl,
                        Err(e) => println!("Face Err: {:?}", e),
                    }
                }
                _ => {}
            }
        }
    }

    let vertex_list: Vec<[f32; 3]> = vertex_normal_list
        .iter()
        .map(|v| [v.vertex.x, v.vertex.y, v.vertex.z])
        .collect();
    let face_list: Vec<[u32; 3]> = face_list
        .iter()
        .map(|f| [f.vertex_index[0], f.vertex_index[1], f.vertex_index[2]])
        .collect();

    set_position_data(mesh, vertex_list.clone());
    if face_list.len() == vertex_list.len() {
        set_mesh_indices(
            mesh,
            face_list.iter().flatten().map(|&f| f).collect::<Vec<u32>>(),
        );
    }

    if let Some(first_vertex) = vertex_normal_list.first() {
        if let Some(_) = first_vertex.normal {
            // assume all have normals
            let normals_list: Vec<[f32; 3]> = vertex_normal_list
                .iter()
                .map(|a| {
                    [
                        a.normal.unwrap().x,
                        a.normal.unwrap().y,
                        a.normal.unwrap().z,
                    ]
                })
                .collect();
            set_normal_data(mesh, normals_list);
        } else {
            let normals_list: Vec<[f32; 3]> = vertex_list.iter().map(|_| [0., 1., 0.]).collect();
            set_normal_data(mesh, normals_list);
        }
    }

    if true {
        let vertex_uv_data = vec![[0.0, 0.0, 0.0]; vertex_normal_list.len()];
        set_uv_data(mesh, vertex_uv_data)
    }
    Ok(())
}

fn set_position_data(
    mesh: &mut Mesh,
    data: Vec<[f32; 3]>,
) {
    let positions = VertexAttributeValues::Float3(data);
    mesh.set_attribute(Mesh::ATTRIBUTE_POSITION, positions);
}

fn set_normal_data(
    mesh: &mut Mesh,
    data: Vec<[f32; 3]>,
) {
    let normals = VertexAttributeValues::Float3(data);
    mesh.set_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
}

fn set_uv_data(
    mesh: &mut Mesh,
    data: Vec<[f32; 3]>,
) {
    let uvs = VertexAttributeValues::Float3(data);
    mesh.set_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
}

fn set_mesh_indices(
    mesh: &mut Mesh,
    indices: Vec<u32>,
) {
    mesh.set_indices(Some(Indices::U32(indices)));
}

#[derive(Default)]
struct VertexWithNormal {
    vertex: Vertex,
    normal: Option<Vertex>,
}

impl VertexWithNormal {
    fn create_optional_normal(&mut self) {
        if let None = self.normal {
            self.normal = Some(Default::default())
        }
    }
}
#[derive(Default, Clone, Copy)]
struct Vertex {
    x: f32,
    y: f32,
    z: f32,
}

#[derive(Default)]
struct Face {
    vertex_index: Vec<u32>,
}

// The structs need to implement the PropertyAccess trait, otherwise the parser doesn't know how to write to them.
// Most functions have default, hence you only need to implement, what you expect to need.

impl ply::PropertyAccess for VertexWithNormal {
    fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
    fn set_property(
        &mut self,
        key: String,
        property: ply::Property,
    ) {
        match (key.as_ref(), property) {
            ("x", ply::Property::Float(v)) => self.vertex.x = v,
            ("y", ply::Property::Float(v)) => self.vertex.y = v,
            ("z", ply::Property::Float(v)) => self.vertex.z = v,
            ("nx", ply::Property::Float(v)) => {
                self.create_optional_normal();
                self.normal.unwrap().x = v;
            }
            ("ny", ply::Property::Float(v)) => {
                self.create_optional_normal();
                self.normal.unwrap().y = v;
            }
            ("nz", ply::Property::Float(v)) => {
                self.create_optional_normal();
                self.normal.unwrap().z = v;
            }
            (_, _) => {}
        }
    }
}

impl ply::PropertyAccess for Vertex {
    fn new() -> Self {
        Vertex {
            ..Default::default()
        }
    }
    fn set_property(
        &mut self,
        key: String,
        property: ply::Property,
    ) {
        match (key.as_ref(), property) {
            ("x", ply::Property::Float(v)) => self.x = v,
            ("y", ply::Property::Float(v)) => self.y = v,
            ("z", ply::Property::Float(v)) => self.z = v,
            (_, _) => {}
        }
    }
}

// same thing for Face
impl ply::PropertyAccess for Face {
    fn new() -> Self {
        Face {
            ..Default::default()
        }
    }
    fn set_property(
        &mut self,
        key: String,
        property: ply::Property,
    ) {
        match (key.as_ref(), property) {
            ("vertex_index", ply::Property::ListInt(vec)) => {
                self.vertex_index = vec.iter().map(|&fi| fi as u32).collect()
            }
            (k, _) => panic!("Face: Unexpected key/value combination: key: {}", k),
        }
    }
}
