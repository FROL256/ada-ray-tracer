#include "embree3/rtcore.h"

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

extern "C" void c_gcore_destroy()
{
  if(g_data.device != nullptr)
  {
    rtcReleaseDevice(g_data.device);
    g_data.device = nullptr;
  }
}

extern "C" void c_gcore_init_and_clear()
{
  c_gcore_destroy();

  g_data.m_meshes.resize(0);
  g_data.m_instances.resize(0);

  g_data.device = rtcNewDevice("");
  
}

extern "C" int c_gcore_add_mesh(const float* a_vertices4f, const int* a_indices, int a_indicesNum)
{

  RTCGeometry geom = rtcNewGeometry(g_data.device, RTC_GEOMETRY_TYPE_TRIANGLE);

  if(geom == NULL)
  {
    std::cout << "[c_gcore]: rtcNewGeometry failed to create new geometry" << std::endl;
    return 0;
  }

  Vertex* vertices = (Vertex*)rtcSetNewGeometryBuffer(geom, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3,sizeof(Vertex),4);

  if(vertices == nullptr)
  {
    std::cout << "[c_gcore]: rtcSetNewGeometryBuffer failed to allocate memory" << std::endl;
    return 0;
  }

  vertices[0].x = -10.0f; vertices[0].y = -2.0f; vertices[0].z = -10.0f;  /////////////////////////////////////////////////////////// <<<---------------------------- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  vertices[1].x = -10.0f; vertices[1].y = -2.0f; vertices[1].z = +10.0f;
  vertices[2].x = +10.0f; vertices[2].y = -2.0f; vertices[2].z = -10.0f;
  vertices[3].x = +10.0f; vertices[3].y = -2.0f; vertices[3].z = +10.0f;

  /* set triangles */
  Triangle* triangles = (Triangle*) rtcSetNewGeometryBuffer(geom,RTC_BUFFER_TYPE_INDEX,0,RTC_FORMAT_UINT3,sizeof(Triangle),2);

  if(triangles == nullptr)
  {
    std::cout << "[c_gcore]: rtcSetNewGeometryBuffer failed to allocate memory" << std::endl;
    return 0;
  }

  triangles[0].v0 = 0; triangles[0].v1 = 1; triangles[0].v2 = 2;
  triangles[1].v0 = 1; triangles[1].v1 = 3; triangles[1].v2 = 2;

  rtcCommitGeometry(geom);

  g_data.m_meshes.push_back( rtcNewScene(g_data.device) );

  unsigned int geomID = rtcAttachGeometry(g_data.m_meshes[g_data.m_meshes.size()-1], geom);
  rtcReleaseGeometry(geom);

  if(geomID != g_data.m_meshes.size()-1)
    std::cout << "[c_gcore]: BROKEN GEOM INDEX" << geomID << std::endl;
  else
    std::cout << "[c_gcore]: CORECT GEOM INDEX" << geomID << std::endl;

  return geomID;
}


extern "C" int c_gcore_instance_meshes(int a_geomId, const float* a_matrices16f, int a_matrixNum)
{
  return 0;
}


extern "C" int c_test_add(int a, int b)
{
  return a+b;
}
