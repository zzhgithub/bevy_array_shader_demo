use std::borrow::BorrowMut;
use std::num::NonZeroU32;

use bevy::asset::LoadState;
use bevy::prelude::*;
use bevy::reflect::{TypeData, TypeUuid};
use bevy::render::mesh::{Indices, MeshVertexAttribute, VertexAttributeValues};
use bevy::render::render_asset::RenderAssets;
use bevy::render::render_resource::{
    AddressMode, AsBindGroup, AsBindGroupError, BindGroupDescriptor, BindGroupEntry,
    BindGroupLayout, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingResource, BindingType,
    Extent3d, PreparedBindGroup, PrimitiveTopology, SamplerBindingType, SamplerDescriptor,
    ShaderRef, ShaderStages, TextureSampleType, TextureViewDimension, VertexFormat, Sampler,
};
use bevy::render::renderer::RenderDevice;
use bevy::render::texture::{self, FallbackImage, ImageSampler};
use bevy_flycam::PlayerPlugin;
use block_mesh::ndshape::{ConstShape, ConstShape3u32};
use block_mesh::{
    greedy_quads, GreedyQuadsBuffer, MergeVoxel, Voxel, VoxelVisibility, RIGHT_HANDED_Y_UP_CONFIG,
};

#[derive(Default, Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum AppState {
    #[default]
    Loading,
    Run,
}

impl States for AppState {
    type Iter = std::array::IntoIter<AppState, 2>;

    fn variants() -> Self::Iter {
        [Self::Loading, Self::Run].into_iter()
    }
}

const UV_SCALE: f32 = 1.0 / 20.0;

#[derive(Resource)]
struct Loading {
    is_loaded: bool,
    handle: Handle<Image>,
}

// 给Vertex Attribute 添加的值
pub const ATTRIBUTE_DATA: MeshVertexAttribute =
    MeshVertexAttribute::new("Vertex_Data", 0x696969, VertexFormat::Uint32);

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(MaterialPlugin::<BindlessMaterial>::default())
        .add_plugin(PlayerPlugin)
        .add_state::<AppState>()
        .add_startup_system(setup)
        // .add_system(load_assets.in_schedule(OnEnter(AppState::Loading)))
        // .add_system(check_loaded.in_set(OnUpdate(AppState::Loading)))
        // .add_system(setup.in_schedule(OnEnter(AppState::Run)))
        // .add_system(camera_rotation_system.in_set(OnUpdate(AppState::Run)))
        .run();
}

fn load_assets(mut commands: Commands, asset_server: Res<AssetServer>) {
    debug!("load");
    // let handle = asset_server.load("uv_checker.png");
    let handle = asset_server.load("texture_zzh.png");

    commands.insert_resource(Loading {
        is_loaded: false,
        handle,
    });
}

/// Make sure that our texture is loaded so we can change some settings on it later
fn check_loaded(
    mut next_state: ResMut<NextState<AppState>>,
    mut handle: ResMut<Loading>,
    mut images: ResMut<Assets<Image>>,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ArrayTextureMaterial>>,
    mut commands: Commands,
) {
    debug!("check loaded");
    if let LoadState::Loaded = asset_server.get_load_state(&handle.handle) {
        handle.is_loaded = true;
        let image: &mut Image = images.get_mut(&handle.handle).unwrap();
        let array_layers = 3;
        image.reinterpret_stacked_2d_as_array(array_layers);
        // image.reinterpret_size(Extent3d {
        //     width: image.texture_descriptor.size.width / array_layers,
        //     height: image.texture_descriptor.size.height,
        //     depth_or_array_layers: array_layers,
        // });
        image.sampler_descriptor = ImageSampler::Descriptor(SamplerDescriptor {
            address_mode_u: AddressMode::Repeat,
            address_mode_v: AddressMode::Repeat,
            ..Default::default()
        });
        let a = materials.add(ArrayTextureMaterial {
            array_texture: handle.handle.clone(),
        });
        commands.insert_resource(MaterialStorge(a));
        next_state.set(AppState::Run);
    }
}

#[derive(Debug, Resource)]
struct MaterialStorge(Handle<ArrayTextureMaterial>);

#[derive(AsBindGroup, Debug, Clone, TypeUuid)]
#[uuid = "9c5a0ddf-1eaf-41b4-9832-ed736fd26af3"]
struct ArrayTextureMaterial {
    #[texture(0, dimension = "2d_array")]
    #[sampler(1)]
    array_texture: Handle<Image>,
}

impl Material for ArrayTextureMaterial {
    fn fragment_shader() -> ShaderRef {
        "shaders/array_texture-2.wgsl".into()
    }

    fn vertex_shader() -> ShaderRef {
        "shaders/array_texture-2.wgsl".into()
    }

    fn specialize(
        pipeline: &bevy::pbr::MaterialPipeline<Self>,
        descriptor: &mut bevy::render::render_resource::RenderPipelineDescriptor,
        layout: &bevy::render::mesh::MeshVertexBufferLayout,
        key: bevy::pbr::MaterialPipelineKey<Self>,
    ) -> Result<(), bevy::render::render_resource::SpecializedMeshPipelineError> {
        let vertex_layout = layout.get_layout(&[
            Mesh::ATTRIBUTE_POSITION.at_shader_location(0),
            ATTRIBUTE_DATA.at_shader_location(1),
            Mesh::ATTRIBUTE_UV_0.at_shader_location(2),
        ])?;
        descriptor.vertex.buffers = vec![vertex_layout];
        Ok(())
    }
}

/// Basic voxel type with one byte of texture layers
#[derive(Default, Clone, Copy)]
struct BoolVoxel(u8);

impl BoolVoxel {
    pub const Empty: Self = BoolVoxel(0);
    pub const Grass: Self = BoolVoxel(1);
    pub const Sold: Self = BoolVoxel(2);
    pub const Sonw: Self = BoolVoxel(3);
}

impl MergeVoxel for BoolVoxel {
    type MergeValue = u8;

    fn merge_value(&self) -> Self::MergeValue {
        self.0
    }
}

impl Voxel for BoolVoxel {
    fn get_visibility(&self) -> VoxelVisibility {
        if self.0 > 0 {
            VoxelVisibility::Opaque
        } else {
            VoxelVisibility::Empty
        }
    }
}

const TILE_ID: [usize; 3] = [0, 1, 2];

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<BindlessMaterial>>,
    // mut material_storge: ResMut<MaterialStorge>,
) {
    debug!("setup");
    // let mut texture = textures.get_mut(&texture_handle.0).unwrap();

    // Set the texture to tile over the entire quad
    // texture.sampler_descriptor = ImageSampler::Descriptor(SamplerDescriptor {
    //     address_mode_u: AddressMode::Repeat,
    //     address_mode_v: AddressMode::Repeat,
    //     ..Default::default()
    // });

    let textures: Vec<_> = TILE_ID
        .iter()
        .map(|id| {
            let path = format!("textures/{id:0>2}.png");
            asset_server.load(path)
        })
        .collect();
    let mat = materials.add(BindlessMaterial { textures });

    type SampleShape = ConstShape3u32<22, 22, 22>;
    // Just a solid cube of voxels. We only fill the interior since we need some empty voxels to form a boundary for the mesh.
    // 这是一堆立方体体素组成的实心立方体。我们只填充内部，因为我们需要一些空的体素来形成网格的边界。
    let mut voxels = [BoolVoxel::Empty; SampleShape::SIZE as usize];
    // 这里用一串数据表示一个立体的空间
    for z in 1..21 {
        for y in 1..21 {
            for x in 1..21 {
                let i = SampleShape::linearize([x, y, z]);
                if ((x * x + y * y + z * z) as f32).sqrt() < 20.0 {
                    if y < 5 {
                        voxels[i as usize] = BoolVoxel::Grass;
                    } else if y < 10 {
                        voxels[i as usize] = BoolVoxel::Sonw;
                    } else {
                        voxels[i as usize] = BoolVoxel::Sold;
                    }
                }
                // }
            }
        }
    }
    // 21 x 21 x 21

    let faces = RIGHT_HANDED_Y_UP_CONFIG.faces;

    let mut buffer = GreedyQuadsBuffer::new(voxels.len());
    greedy_quads(
        &voxels,
        &SampleShape {},
        [0; 3],
        [21; 3],
        &faces,
        &mut buffer,
    );
    let num_indices = buffer.quads.num_quads() * 6;
    let num_vertices = buffer.quads.num_quads() * 4;
    let mut indices = Vec::with_capacity(num_indices);
    let mut positions = Vec::with_capacity(num_vertices);
    let mut normals = Vec::with_capacity(num_vertices);
    let mut tex_coords = Vec::with_capacity(num_vertices);

    let mut data = Vec::with_capacity(num_vertices);

    for (block_face_normal_index, (group, face)) in buffer
        .quads
        .groups
        .as_ref()
        .into_iter()
        .zip(faces.into_iter())
        .enumerate()
    {
        for quad in group.into_iter() {
            indices.extend_from_slice(&face.quad_mesh_indices(positions.len() as u32));
            positions.extend_from_slice(&face.quad_mesh_positions(&quad, 1.0));
            normals.extend_from_slice(&face.quad_mesh_normals());
            tex_coords.extend_from_slice(&face.tex_coords(
                RIGHT_HANDED_Y_UP_CONFIG.u_flip_face,
                true,
                &quad,
            ));

            // 计算出 data
            // let a: [u32; 3] = quad.minimum.map(|x| x - 1);
            let a = quad.minimum;
            let index = SampleShape::linearize(a);
            let aa = voxels[index as usize].0;
            let c = (aa - 1) as u32;
            let d = (block_face_normal_index as u32) << 8u32;
            data.extend_from_slice(&[d | c; 4]);
            // data.extend_from_slice(&[(block_face_normal_index as u32) << 8u32 | c; 4],);
            // &[voxels[index as usize].0 as u32; 4],);
        }
    }

    let mut render_mesh = Mesh::new(PrimitiveTopology::TriangleList);

    // 这里没有缩放 每个格子会占据一个图片？

    // for uv in tex_coords.iter_mut() {
    //     for c in uv.iter_mut() {
    //         *c *= UV_SCALE;
    //     }
    // }

    render_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    render_mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    render_mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, tex_coords);
    render_mesh.insert_attribute(ATTRIBUTE_DATA, VertexAttributeValues::Uint32(data));
    render_mesh.set_indices(Some(Indices::U32(indices)));

    commands.spawn(MaterialMeshBundle {
        mesh: meshes.add(render_mesh),
        // material: materials.add(texture_handle.0.clone().into()),
        material: mat.clone(),
        transform: Transform::from_translation(Vec3::splat(-10.0)),
        ..Default::default()
    });

    commands.spawn(PointLightBundle {
        transform: Transform::from_translation(Vec3::new(0.0, 50.0, 50.0)),
        point_light: PointLight {
            range: 200.0,
            intensity: 20000.0,
            ..Default::default()
        },
        ..Default::default()
    });
    //
}

#[derive(Debug, Clone, TypeUuid)]
#[uuid = "8dd2b424-45a2-4a53-ac29-7ce356b2d5fe"]
struct BindlessMaterial {
    textures: Vec<Handle<Image>>,
}

const MAX_TEXTURE_COUNT: usize = 3;

impl AsBindGroup for BindlessMaterial {
    type Data = ();

    fn as_bind_group(
        &self,
        layout: &BindGroupLayout,
        render_device: &RenderDevice,
        image_assets: &RenderAssets<Image>,
        fallback_image: &FallbackImage,
    ) -> Result<PreparedBindGroup<Self::Data>, AsBindGroupError> {
        // retrieve the render resources from handles
        let mut images = vec![];

        let sampler = render_device.create_sampler(&SamplerDescriptor {
            address_mode_u: AddressMode::Repeat,
            address_mode_v: AddressMode::Repeat,
            ..Default::default()
        });

        for handle in self.textures.iter().take(MAX_TEXTURE_COUNT) {
            match image_assets.get(handle) {
                Some(image) => {
                    // let sampler = ImageSampler::Descriptor(SamplerDescriptor {
                    //     address_mode_u: AddressMode::Repeat,
                    //     address_mode_v: AddressMode::Repeat,
                    //     ..Default::default()
                    // });
                    // image.sampler_descriptor = sampler;
                    let mut img = image.clone();
                    // img.sampler = render_device.create_sampler(&SamplerDescriptor {
                    //         address_mode_u: AddressMode::Repeat,
                    //         address_mode_v: AddressMode::Repeat,
                    //         ..Default::default()
                    //     });
                    images.push(image);
                },
                None => return Err(AsBindGroupError::RetryNextUpdate),
            }
        }

        let textures = vec![&fallback_image.texture_view; MAX_TEXTURE_COUNT];

        // convert bevy's resource types to WGPU's references
        let mut textures: Vec<_> = textures.into_iter().map(|texture| &**texture).collect();

        // fill in up to the first `MAX_TEXTURE_COUNT` textures and samplers to the arrays
        for (id, image) in images.into_iter().enumerate() {
            textures[id] = &*image.texture_view;
        }

        let bind_group = render_device.create_bind_group(&BindGroupDescriptor {
            label: "bindless_material_bind_group".into(),
            layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: BindingResource::TextureViewArray(&textures[..]),
                },
                BindGroupEntry {
                    binding: 1,
                    
                    resource: BindingResource::Sampler(&sampler),
                },
            ],
        });

        Ok(PreparedBindGroup {
            bindings: vec![],
            bind_group,
            data: (),
        })
    }

    fn bind_group_layout(render_device: &RenderDevice) -> BindGroupLayout
    where
        Self: Sized,
    {
        render_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: "bindless_material_layout".into(),
            entries: &[
                // @group(1) @binding(0) var textures: binding_array<texture_2d<f32>>;
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: NonZeroU32::new(MAX_TEXTURE_COUNT as u32),
                },
                // @group(1) @binding(1) var nearest_sampler: sampler;
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None,
                    // Note: as textures, multiple samplers can also be bound onto one binding slot.
                    // One may need to pay attention to the limit of sampler binding amount on some platforms.
                    // count: NonZeroU32::new(MAX_TEXTURE_COUNT as u32),
                },
            ],
        })
    }
}

impl Material for BindlessMaterial {
    fn fragment_shader() -> ShaderRef {
        "shaders/array_texture-3.wgsl".into()
    }

    fn vertex_shader() -> ShaderRef {
        "shaders/array_texture-3.wgsl".into()
    }

    fn specialize(
        pipeline: &bevy::pbr::MaterialPipeline<Self>,
        descriptor: &mut bevy::render::render_resource::RenderPipelineDescriptor,
        layout: &bevy::render::mesh::MeshVertexBufferLayout,
        key: bevy::pbr::MaterialPipelineKey<Self>,
    ) -> Result<(), bevy::render::render_resource::SpecializedMeshPipelineError> {
        let vertex_layout = layout.get_layout(&[
            Mesh::ATTRIBUTE_POSITION.at_shader_location(0),
            ATTRIBUTE_DATA.at_shader_location(1),
            Mesh::ATTRIBUTE_UV_0.at_shader_location(2),
        ])?;
        descriptor.vertex.buffers = vec![vertex_layout];
        Ok(())
    }
}
