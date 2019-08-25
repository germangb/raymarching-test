use gl::types::*;
use glm::{Mat4, Vec3};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::Duration;
use std::{mem, ptr};

macro_rules! check {
    ($call:expr) => {{
        gl::GetError();
        let res = $call;
        let err = gl::GetError();
        if err != gl::NO_ERROR {
            panic!("GL error = {}!", err);
        }
        res
    }};
}

#[derive(imgui_ext::Gui)]
pub struct App {
    #[imgui(checkbox)]
    pub app_running: bool,
    #[imgui(checkbox)]
    app_time_enabled: bool,
    #[imgui]
    app_time: f32,
    #[imgui(color(edit))]
    app_back: [f32; 4],
    // gl
    #[imgui]
    gl_cuboid_vbo: GLuint,
    #[imgui]
    gl_cuboid_ebo: GLuint,
    #[imgui]
    gl_cuboid_vao: GLuint,
    #[imgui]
    gl_cuboid_program: GLuint,
    #[imgui]
    gl_cuboid_transform: GLint,
    // camera
    #[imgui(checkbox)]
    cam_orbit: bool,
    #[imgui(slider(min = "0.0", max = "8.0"))]
    cam_orbit_speed: f32,
    #[imgui(drag(map = "na_to_gui_vec3"))]
    cam_eye: Vec3,
    #[imgui(drag(map = "na_to_gui_vec3"))]
    cam_center: Vec3,
    #[imgui(drag(map = "na_to_gui_vec3"))]
    cam_up: Vec3,
    #[imgui(drag(map = "na_to_gui_vec3"))]
    cam_offset: Vec3,
    #[imgui(drag)]
    cam_fov_rad: f32,
    #[imgui(drag)]
    cam_far: f32,
    #[imgui(drag)]
    cam_near: f32,
    #[imgui(tree(node(input(map = "na_to_gui_mat4", flags = "read_only"))))]
    cam_proj: Mat4,
    #[imgui(tree(node(input(map = "na_to_gui_mat4", flags = "read_only"))))]
    cam_view: Mat4,
    // volumes
    #[imgui(tree(node(nested)))]
    volume: Volume,
}

impl Drop for App {
    fn drop(&mut self) {
        unsafe {
            check!(gl::DeleteProgram(self.gl_cuboid_program));
            check!(gl::DeleteBuffers(1, &self.gl_cuboid_vbo));
            check!(gl::DeleteBuffers(1, &self.gl_cuboid_ebo));
            check!(gl::DeleteVertexArrays(1, &self.gl_cuboid_vao));
        }
    }
}

#[derive(imgui_ext::Gui)]
pub struct Volume {
    #[imgui(checkbox)]
    enabled: bool,
    #[imgui(checkbox)]
    wireframe: bool,
    #[imgui]
    program: GLuint,
    #[imgui]
    u_resolution: GLint,
    #[imgui]
    u_time: GLint,
    #[imgui]
    u_mvp: GLint,
    #[imgui]
    u_mvp_inv: GLint,
    #[imgui]
    u_vp: GLint,
    #[imgui]
    u_vp_inv: GLint,
    #[imgui]
    u_proj: GLint,
    #[imgui]
    u_proj_inv: GLint,
    #[imgui]
    u_world: GLint,
    #[imgui]
    u_world_inv: GLint,
    #[imgui(drag(map = "na_to_gui_vec3"))]
    position: Vec3,
    #[imgui(drag(map = "na_to_gui_vec3"))]
    scale: Vec3,
    // euler rotation angles
    #[imgui(drag(map = "na_to_gui_vec3", speed = "0.1"))]
    euler: Vec3,
    #[imgui(tree(node(input(map = "na_to_gui_mat4", flags = "read_only"))))]
    world: Mat4,
}

impl Drop for Volume {
    fn drop(&mut self) {
        unsafe {
            check!(gl::DeleteProgram(self.program));
        }
    }
}

impl Default for App {
    #[rustfmt::skip]
    fn default() -> Self {
        let (gl_cuboid_vbo, gl_cuboid_ebo, gl_cuboid_vao) = unsafe {
            let mut buffers: [GLuint; 2] = mem::zeroed();
            let mut vao = mem::zeroed();
            check!(gl::CreateVertexArrays(1, &mut vao));
            check!(gl::GenBuffers(buffers.len() as _, buffers.as_mut_ptr()));
            check!(gl::BindVertexArray(vao));
            check!(gl::BindBuffer(gl::ARRAY_BUFFER, buffers[0]));
            check!(gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, buffers[1]));
            let vert: &[f32] = &[
                -1.0, -1.0, 1.0,
                1.0, -1.0, 1.0,
                1.0, -1.0, -1.0,
                -1.0, -1.0, -1.0,

                -1.0, 1.0, 1.0,
                1.0, 1.0, 1.0,
                1.0, 1.0, -1.0,
                -1.0, 1.0, -1.0,
            ];
            let index: &[u16] = &[
                0, 1, 4,
                1, 5, 4,
                1, 2, 5,
                2, 6, 5,
                2, 3, 6,
                3, 7, 6,
                3, 0, 4,
                3, 4, 7,
                0, 3, 2,
                0, 2, 1,
                4, 5, 6,
                4, 6, 7,
            ];
            check!(gl::BufferData(gl::ARRAY_BUFFER, (mem::size_of::<f32>() * vert.len()) as _, vert.as_ptr() as _, gl::STATIC_DRAW));
            check!(gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (mem::size_of::<u16>() * index.len()) as _, index.as_ptr() as _, gl::STATIC_DRAW));
            check!(gl::EnableVertexAttribArray(0));
            check!(gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, 3 << 2, (0 << 2) as _));
            check!(gl::BindVertexArray(0));
            check!(gl::DisableVertexAttribArray(0));
            check!(gl::BindBuffer(gl::ARRAY_BUFFER, 0));
            check!(gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, 0));
            (buffers[0], buffers[1], vao)
        };
        #[rustfmt::skip]
        unsafe fn create_program(vert: &[u8], frag: &[u8], uniforms: &[&str]) -> (GLuint, Vec<GLint>) {
            unsafe fn create_shader(stype: GLenum, source: &[u8]) -> GLuint {
                let shader = check!(gl::CreateShader(stype));
                check!(gl::ShaderSource(shader, 1, [source.as_ptr() as _].as_ptr(), [source.len() as _].as_ptr()));
                check!(gl::CompileShader(shader));
                static mut LOG: [u8; 1024] = [0; 1024];
                let mut len = 0;
                check!(gl::GetShaderInfoLog(shader, LOG.len() as _, &mut len, LOG.as_mut_ptr() as *mut _));
                if len > 0 {
                    let error = std::str::from_utf8_unchecked(&LOG);
                    panic!("{}", error);
                }
                shader
            }
            let vert = create_shader(gl::VERTEX_SHADER, vert);
            let frag = create_shader(gl::FRAGMENT_SHADER, frag);
            let program = check!(gl::CreateProgram());
            check!(gl::AttachShader(program, vert));
            check!(gl::AttachShader(program, frag));
            check!(gl::LinkProgram(program));
            check!(gl::DeleteShader(vert));
            check!(gl::DeleteShader(frag));
            let mut uniform_loc = Vec::with_capacity(uniforms.len());
            for name in uniforms {
                let loc = check!(gl::GetUniformLocation(program, name.as_ptr() as _));
                uniform_loc.push(loc);
            }
            (program, uniform_loc)
        }
        let (gl_cuboid_program, gl_cuboid_transform) = unsafe {
            let (program, unif) = create_program(
                include_bytes!("shaders/debug.vert"),
                include_bytes!("shaders/debug.frag"),
                &["u_transform\0"],
            );
            (program, unif[0])
        };
        let (cam_proj, cam_view) = (glm::identity(), glm::identity());
        let volume = unsafe {
            let (program, uniforms) = create_program(
                include_bytes!("shaders/volume.vert"),
                include_bytes!("shaders/volume.frag"),
                &[
                    "u_resolution\0",
                    "u_time\0",
                    "u_mvp\0",
                    "u_mvp_inv\0",
                    "u_vp\0",
                    "u_vp_inv\0",
                    "u_proj\0",
                    "u_proj_inv\0",
                    "u_world\0",
                    "u_world_inv\0",
                ],
            );
            Volume {
                enabled: true,
                wireframe: true,
                program,
                u_resolution: uniforms[0],
                u_time: uniforms[1],
                u_mvp: uniforms[2],
                u_mvp_inv: uniforms[3],
                u_vp: uniforms[4],
                u_vp_inv: uniforms[5],
                u_proj: uniforms[6],
                u_proj_inv: uniforms[7],
                u_world: uniforms[8],
                u_world_inv: uniforms[9],
                position: glm::vec3(0.0, 0.0, 0.0),
                scale: glm::vec3(1.0, 1.0, 1.0),
                euler: glm::vec3(0.0, 0.0, 0.0),
                world: glm::identity()
            }
        };
        Self {
            app_back: [0.5, 0.5, 0.5, 1.0],
            app_time: 0.0,
            app_time_enabled: true,
            app_running: true,
            gl_cuboid_vbo,
            gl_cuboid_ebo,
            gl_cuboid_vao,
            gl_cuboid_program,
            gl_cuboid_transform,
            cam_orbit: true,
            cam_orbit_speed: 1.0,
            cam_eye: glm::vec3(3.3, 1.9, 3.5),
            cam_center: glm::vec3(0.0, 0.0, 0.0),
            cam_up: glm::vec3(0.0, 1.0, 0.0),
            cam_offset: glm::vec3(1.25, 0.0, 0.0),
            cam_fov_rad: 55.0 * std::f32::consts::PI / 180.0,
            cam_near: 0.01,
            cam_far: 1000.0,
            cam_proj,
            cam_view,
            volume,
        }
    }
}

impl App {
    #[rustfmt::skip]
    pub fn handle_event(&mut self, event: &Event) {
        match event {
            Event::MouseWheel { x, y, .. } => {
                let c = -8.0;
                self.move_radius(*y as f32 * c, 1.0 / 60.0);
            }
            Event::MouseMotion { xrel, yrel, mousestate, .. } if mousestate.left() => {
                let c = -4.0;
                self.move_left(*xrel as f32 * c, 1.0 / 60.0);
                self.move_up(-*yrel as f32 * c, 1.0 / 60.0);
            },
            Event::KeyDown { keycode: Some(Keycode::Q), .. } => self.app_running = false,
            Event::KeyDown { keycode: Some(Keycode::O), .. } => self.cam_orbit = !self.cam_orbit,
            Event::KeyDown { keycode: Some(Keycode::T), .. } => self.app_time_enabled = !self.app_time_enabled,

            //offset
            Event::KeyDown { keycode: Some(Keycode::W), .. } => self.cam_offset.y -= 0.2,
            Event::KeyDown { keycode: Some(Keycode::S), .. } => self.cam_offset.y += 0.2,
            Event::KeyDown { keycode: Some(Keycode::A), .. } => self.cam_offset.x += 0.2,
            Event::KeyDown { keycode: Some(Keycode::D), .. } => self.cam_offset.x -= 0.2,

            Event::KeyDown { keycode: Some(Keycode::Left), .. } => {
                self.cam_orbit_speed -= 0.5;
                if self.cam_orbit_speed < 0.0 {
                    self.cam_orbit_speed = 0.0;
                }
            }
            Event::KeyDown { keycode: Some(Keycode::Right), .. } => {
                self.cam_orbit_speed += 0.5;
                if self.cam_orbit_speed > 8.0 {
                    self.cam_orbit_speed = 8.0;
                }
            }
            _ => {},
        }
    }

    fn update_volume(&mut self) {
        if self.volume.enabled {
            // update world transform
            let world = glm::translation(&self.volume.position)
                * glm::scaling(&self.volume.scale)
                * glm::rotation(self.volume.euler.x, &Vec3::x())
                * glm::rotation(self.volume.euler.y, &Vec3::y())
                * glm::rotation(self.volume.euler.z, &Vec3::z());
            self.volume.world = world;
        }
    }

    fn move_radius(&mut self, velo: f32, dt: f32) {
        let r0 = (self.cam_eye - self.cam_center).normalize();
        self.cam_eye += r0 * velo * dt;
    }

    fn move_left(&mut self, velo: f32, dt: f32) {
        let r0 = self.cam_eye - self.cam_center;
        let tan = self.cam_up.cross(&r0).normalize();
        let new_eye = self.cam_eye + tan * velo * dt;
        self.cam_eye = new_eye;
        let r1 = self.cam_eye - self.cam_center;
        self.cam_eye = self.cam_center + r1.normalize() * r0.magnitude();
    }

    fn move_up(&mut self, velo: f32, dt: f32) {
        let r0 = self.cam_eye - self.cam_center;
        let tan = self.cam_up;
        let new_eye = self.cam_eye + tan * velo * dt;
        self.cam_eye = new_eye;
        let r1 = self.cam_eye - self.cam_center;
        self.cam_eye = self.cam_center + r1.normalize() * r0.magnitude();
    }
    fn update_camera(&mut self) {
        if self.cam_orbit {
            // integration constants
            let velo = self.cam_orbit_speed;
            let delta = 1.0 / 60.0;

            self.move_left(velo, delta);
        }
    }

    #[rustfmt::skip]
    pub fn render(&mut self) {
        if self.app_time_enabled {
            self.app_time += 1.0 / 60.0;
        }
        self.update_camera();
        self.update_volume();
        unsafe {
            let [r, g, b, a] = self.app_back;
            check!(gl::ClearColor(r, g, b, a));
            check!(gl::Enable(gl::DEPTH_TEST));
            check!(gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT));

            let aspect = 4.0 / 3.0;
            self.cam_proj = glm::perspective(aspect, self.cam_fov_rad, self.cam_near, self.cam_far);
            let offset = glm::translation(&self.cam_offset);
            self.cam_view = offset * glm::look_at(&self.cam_eye, &self.cam_center, &self.cam_up);
            let vp = self.cam_proj * self.cam_view;
            let mvp = vp * self.volume.world;

            if self.volume.enabled {
                check!(gl::BindVertexArray(self.gl_cuboid_vao));

                check!(gl::UseProgram(self.volume.program));
                check!(gl::Uniform2f(self.volume.u_resolution, 800.0, 600.0));
                check!(gl::Uniform1f(self.volume.u_time, self.app_time));
                check!(gl::UniformMatrix4fv(self.volume.u_mvp, 1, gl::FALSE, mvp.as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_mvp_inv, 1, gl::FALSE, glm::inverse(&mvp).as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_vp, 1, gl::FALSE, vp.as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_vp_inv, 1, gl::FALSE, glm::inverse(&vp).as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_proj, 1, gl::FALSE, self.cam_proj.as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_proj_inv, 1, gl::FALSE, glm::inverse(&self.cam_proj).as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_world, 1, gl::FALSE, self.volume.world.as_ptr()));
                check!(gl::UniformMatrix4fv(self.volume.u_world_inv, 1, gl::FALSE, glm::inverse(&self.volume.world).as_ptr()));
                check!(gl::DrawElements(gl::TRIANGLES, 36, gl::UNSIGNED_SHORT, ptr::null()));

                if self.volume.wireframe {
                    check!(gl::UseProgram(self.gl_cuboid_program));
                    check!(gl::UniformMatrix4fv(self.gl_cuboid_transform, 1, gl::FALSE, mvp.as_ptr()));

                    check!(gl::Enable(gl::POLYGON_OFFSET_LINE));
                    check!(gl::PolygonOffset(-1.0, -1.0));
                    check!(gl::PolygonMode(gl::FRONT_AND_BACK, gl::LINE));
                    check!(gl::DrawElements(gl::TRIANGLES, 36, gl::UNSIGNED_SHORT, ptr::null()));
                    check!(gl::PolygonMode(gl::FRONT_AND_BACK, gl::FILL));
                    check!(gl::Disable(gl::POLYGON_OFFSET_LINE));
                }

                check!(gl::BindVertexArray(0));
                check!(gl::UseProgram(0));
            }
        }
    }
}

fn read_only() -> imgui::ImGuiInputTextFlags {
    imgui::ImGuiInputTextFlags::ReadOnly
}

fn na_to_gui_mat4(na: &mut Mat4) -> &mut [[f32; 4]; 4] {
    unsafe { &mut *(na.as_mut_ptr() as *mut [[f32; 4]; 4]) }
}

fn na_to_gui_vec3(na: &mut Vec3) -> &mut [f32; 3] {
    use std::convert::TryInto;
    na.as_mut_slice().try_into().unwrap()
}
