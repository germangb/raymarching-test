#version 130

in vec3 a_position;

out vec4 v_world_pos;

uniform mat4 u_mvp;

void main() {
    gl_Position = u_mvp * vec4(a_position, 1.0);
    v_world_pos = vec4(a_position, 1.0);
}