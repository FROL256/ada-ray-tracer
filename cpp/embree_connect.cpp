#include "embree3/rtcore.h"

#include <memory.h>
#include <vector>
#include <iostream>

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct Vertex   { float x,y,z,r;  }; // FIXME: rename to Vertex4f
struct Triangle { int v0, v1, v2; };

struct GlobalData
{
  GlobalData() : device(nullptr) {}

  RTCDevice device;

  RTCScene                 m_scene;
  std::vector<RTCScene>    m_meshes;
  std::vector<RTCGeometry> m_instances;
  
} g_data;


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern "C" void gcore_destroy()
{
  if(g_data.device != nullptr)
  {
    rtcReleaseDevice(g_data.device);
    g_data.device = nullptr;
  }
}

extern "C" void gcore_init_and_clear()
{
  gcore_destroy();
  g_data.m_meshes.resize(0);
  g_data.m_instances.resize(0);
  g_data.device = rtcNewDevice("");
}

extern "C" int gcore_add_mesh_3f(const float* a_vertices3f, int a_vertexNum, const int* a_indices, int a_indicesNum)
{
  RTCGeometry geom = rtcNewGeometry(g_data.device, RTC_GEOMETRY_TYPE_TRIANGLE);

  if(geom == NULL)
  {
    std::cout << "[c_gcore]: rtcNewGeometry failed to create new geometry" << std::endl;
    return 0;
  }

  /* set triangles */
  Triangle* triangles = (Triangle*) rtcSetNewGeometryBuffer(geom, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3,sizeof(Triangle), a_indicesNum/3);

  if(triangles == nullptr)
  {
    std::cout << "[c_gcore]: rtcSetNewGeometryBuffer failed to allocate index memory" << std::endl;
    return 0;
  }

  // put indices
  //
  const int triNum = a_indicesNum/3;
  int maxVertexId = 0;
  for(int i=0; i< triNum; i++)
  {
    int A = a_indices[i*3 + 0]; 
    int B = a_indices[i*3 + 1]; 
    int C = a_indices[i*3 + 2];

    if(A >= a_vertexNum || B >= a_vertexNum || C >= a_vertexNum) // #NOTE: why this is happen ??? Check scenes !!! 
    {
      A = a_vertexNum-1;   
      B = a_vertexNum-1;
      C = a_vertexNum-1;
    }
    else if(A < 0 || B < 0 || C < 0)                             // #NOTE: why this is happen ??? Check scenes !!! 
    {
      A = a_vertexNum-1;  
      B = a_vertexNum-1;
      C = a_vertexNum-1;
    }

    if(A > maxVertexId) maxVertexId = A;
    if(B > maxVertexId) maxVertexId = B;
    if(C > maxVertexId) maxVertexId = C;

    triangles[i].v0 = A; 
    triangles[i].v1 = B; 
    triangles[i].v2 = C;
  }

  // put vertices
  //
  Vertex* vertices = (Vertex*)rtcSetNewGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, sizeof(Vertex), maxVertexId);
  if(vertices == nullptr)
  {
    std::cout << "[c_gcore]: rtcSetNewGeometryBuffer failed to allocate vertices memory" << std::endl;
    return 0;
  }

  for(int vertexId=0; vertexId < maxVertexId; vertexId++)
  {
    vertices[vertexId].x = a_vertices3f[vertexId*3+0];
    vertices[vertexId].y = a_vertices3f[vertexId*3+1];
    vertices[vertexId].z = a_vertices3f[vertexId*3+2];
  }

  rtcCommitGeometry(geom);
  {
    g_data.m_meshes.push_back( rtcNewScene(g_data.device) );
    rtcAttachGeometry(g_data.m_meshes[g_data.m_meshes.size()-1], geom); // returned geomID is always equal to 0
  }
  rtcReleaseGeometry(geom);
  
  return g_data.m_meshes.size()-1;
}


extern "C" int gcore_instance_meshes(int a_geomId, const float* a_matrices16f, int a_matrixNum)
{
  g_data.m_instances.resize(a_matrixNum);

  for(size_t i=0; i < g_data.m_instances.size(); i++)
  {
    g_data.m_instances[i] = rtcNewGeometry (g_data.device, RTC_GEOMETRY_TYPE_INSTANCE);
    rtcSetGeometryInstancedScene(g_data.m_instances[i], g_data.m_scene);
    rtcSetGeometryTimeStepCount (g_data.m_instances[i], 1);
    rtcAttachGeometry           (g_data.m_scene,        g_data.m_instances[i]);
  }

  // decrease fucking reference counter (embree3 equals shit)
  //
  for(size_t i=0; i < g_data.m_instances.size(); i++) 
    rtcReleaseGeometry(g_data.m_instances[i]);

  // set matrices
  //
  for(size_t i=0; i < g_data.m_instances.size(); i++)
    rtcSetGeometryTransform(g_data.m_instances[i], 0, RTC_FORMAT_FLOAT4X4_ROW_MAJOR, a_matrices16f + 16*i);

  // commit evetithing
  //
  for(size_t i=0; i < g_data.m_instances.size(); i++)
    rtcCommitGeometry(g_data.m_instances[i]);

  rtcCommitScene (g_data.m_scene);

  return 0;
}


