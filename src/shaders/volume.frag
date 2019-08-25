#version 130
#define EPSILON 0.001
#define DOMAIN(x)

uniform float u_time;

uniform mat4 u_world;
uniform mat4 u_world_inv;

float map(in vec3 p) {
    float t = u_time;
    vec3 p0 = vec3( sin(t)) * 0.5;
    vec3 p1 = vec3(-sin(t)) * 0.5;
    vec3 p2 = vec3(0.0, sin(t), cos(t)) * 0.5;

    float d = 0.0;
    float r = 0.5;
    float f = 1.2;

    float c0 = exp(-length(p - p0) * f);
    float c1 = exp(-length(p - p1) * f);
    float c2 = exp(-length(p - p2) * f);

    float ct = c0 + c1 + c2;

    d += (length(p - p0) - r) * c0 / ct;
    d += (length(p - p1) - r) * c1 / ct;
    d += (length(p - p2) - r) * c2 / ct;

    return d;
}

vec3 grad(vec3 p) {
    vec2 ep = vec2(EPSILON, 0.0);
    return normalize(vec3(
        map(p + ep.xyy) - map(p - ep.xyy),
        map(p + ep.yxy) - map(p - ep.yxy),
        map(p + ep.yyx) - map(p - ep.yyx)
    ));
}

void render(in vec3 ro, in vec3 rd, out vec4 frag_color) {
    float h = EPSILON;

    for (int i = 0; i < 256; ++i) {
        vec3 p = ro + rd * h;

        // out of domain
        if (abs(p.x) > 1.0 || abs(p.y) > 1.0 || abs(p.z) > 1.0)
            break;

        float dist = map(p);

        if (dist < EPSILON) {
            frag_color = vec4(1.0, 0.0, 1.0, 1.0);

            vec3 nor = grad(p);
            vec3 sun = normalize(u_world_inv * normalize(vec4(3.0, 2.0, 0.0, 0.0))).xyz;
            float lit = dot(nor, sun);

            frag_color.rgb *= lit;
            frag_color.rgb = sqrt(frag_color.rgb);
            return;
        }

        h += max(dist, EPSILON);
    }

    discard;
}

in vec4 v_world_pos;

out vec4 _frag_color;

uniform vec2 u_resolution;
uniform mat4 u_mvp;
uniform mat4 u_mvp_inv;
uniform mat4 u_vp;
uniform mat4 u_vp_inv;
uniform mat4 u_proj;
uniform mat4 u_proj_inv;

void main() {
    // compute local-space ray
    vec2 res = u_resolution;

    vec4 near = vec4(gl_FragCoord.xy / res * 2.0 - 1.0, -1.0, 1.0);
    vec4 far = vec4(gl_FragCoord.xy / res * 2.0 - 1.0, 1.0, 1.0);

    vec4 near_world = u_mvp_inv * near;
    vec4 far_world = u_mvp_inv * far;

    near_world.xyz /= near_world.w;
    far_world.xyz /= far_world.w;

    vec3 ro = v_world_pos.xyz;
    vec3 rd = normalize(far_world.xyz - near_world.xyz);

    _frag_color = vec4(0.0, 0.0, 0.0, 1.0);
    render(ro, rd, _frag_color);
}
