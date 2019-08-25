#version 130

in vec3 a_position;

uniform mat4 u_transform;

void main() {
    gl_Position = u_transform * vec4(a_position, 1.0);
}
