#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 0) uniform UniformBufferObject {
  mat4 model;
  mat4 view;
  mat4 projection;
} ubo;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in uvec4 inColor;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 fragTexCoord;
layout(location = 2) out vec3 flatNormal;
layout(location = 3) out vec3 fragPos;
layout(location = 4) out vec3 lightPos;

vec3 constLightPos = vec3( 10.0, -30.0, -15.0);

void main() {
//    gl_Position = ubo.mvp * vec4(inPosition, 1.0);
    gl_Position = ubo.projection * ubo.view * ubo.model * vec4(inPosition, 1.0);
    fragColor = inColor * vec4(1.0 / 255.0);
    fragTexCoord = inTexCoord;
    flatNormal = mat3(transpose(inverse(ubo.model))) * inNormal;
    mat4 mv = ubo.view * ubo.model;
    fragPos = (mv * vec4(inPosition, 1.0)).xyz;
//    flatNormal = inNormal;
    lightPos = (mv * vec4(constLightPos, 1.0)).xyz;
}
