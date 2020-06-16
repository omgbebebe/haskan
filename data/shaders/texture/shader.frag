#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in flat vec3 flatNormal;
layout(location = 3) in vec3 fragPos;
layout(location = 4) in vec3 lightPos;

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

vec3 lightColor = vec3( 1.0, 0.2, 1.0);
vec3 ambient = 0.1 * lightColor;

void main() {
//    vec3 xTangent = dFdx( viewPosition );
//    vec3 yTangent = dFdy( viewPosition );
//    vec3 faceNormal = normalize( cross( xTangent, yTangent ) );
//    vec3 norm = normalize(faceNormal);
    vec3 norm = flatNormal;

    vec3 lightDir = normalize (lightPos - fragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 diffuse = diff * lightColor;
    vec3 objColor = fragColor.rgb; //texture(texSampler, fragTexCoord).rgb;
    vec3 result = (ambient + diffuse) * objColor;
    outColor = vec4(result, 1.0);
    //outColor = vec4((flatNormal+1.0)/2.0, 1.0);
}
