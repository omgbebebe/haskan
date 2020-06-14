#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 fragPos;

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

vec3 lightPos = vec3( -20.0, 45.0, -50.0);
vec3 lightColor = vec3( 1.0, 0.2, 1.0);
vec3 ambient = 0.1 * lightColor;

void main() {
    vec3 lightDir = normalize (lightPos - fragPos);
    float diff = max(dot(normal, lightDir), 0.0);
    vec3 diffuse = diff * lightColor;
    vec3 objColor = texture(texSampler, fragTexCoord).rgb;
    vec3 result = (ambient + diffuse) * objColor;
    outColor = vec4(result, 1.0);
}
