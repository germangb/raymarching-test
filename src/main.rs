use imgui::Context;
use imgui_ext::UiExt;
use imgui_opengl_renderer::Renderer as ImguiGl;
use imgui_sdl2::ImguiSdl2;

mod structs;

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();

    let (w, h) = (1024, 600);
    let window = video
        .window("Window", w as u32, h as u32)
        .position_centered()
        //.resizable()
        .opengl()
        .build()
        .unwrap();

    // opengl init
    let gl_ctx = window.gl_create_context().unwrap();
    window.gl_make_current(&gl_ctx).unwrap();
    gl::load_with(|s| video.gl_get_proc_address(s) as _);

    // imgui
    let mut imgui = Context::create();
    let mut imgui_sdl2 = ImguiSdl2::new(&mut imgui, &window);
    let imgui_render = ImguiGl::new(&mut imgui, |s| video.gl_get_proc_address(s) as _);
    let mut event_pump = sdl.event_pump().unwrap();
    let mut app = structs::App::new(w, h);

    while app.app_running {
        event_pump.poll_iter().for_each(|e| {
            if imgui_sdl2.ignore_event(&e) {
                imgui_sdl2.handle_event(&mut imgui, &e);
            } else {
                app.handle_event(&e);
            }
        });

        app.render();

        imgui_sdl2.prepare_frame(imgui.io_mut(), &window, &event_pump.mouse_state());
        let ui = imgui.frame();
        ui.draw_gui(&mut app);
        imgui_sdl2.prepare_render(&ui, &window);
        imgui_render.render(ui);

        window.gl_swap_window();
    }
}
