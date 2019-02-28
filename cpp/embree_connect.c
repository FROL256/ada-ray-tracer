#include "embree3/rtcore.h"

#include <stdlib.h>

typedef struct SceneDataT
{
  RTCScene  m_scene;
  RTCScene* m_meshes;
  int       m_numMeshes;
} SceneData;

typedef struct SceneInstT
{
  RTCGeometry* m_instances;
  int          m_instanceNum;
} SceneInst;


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct GlobalDataT
{
  SceneData scnData;
  SceneInst scnInst;

} g_data;


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void free_scene_data(SceneData* a_pScnData)
{
  if(a_pScnData == 0)
    return;

  if(a_pScnData->m_meshes != 0)
  {
    free(a_pScnData->m_meshes); 
    a_pScnData->m_meshes = 0;
  }
}

void alloc_scene_data(int a_numMeshes, SceneData* a_pScnData)
{
  if(a_pScnData == 0)
    return;

  free_scene_data(a_pScnData);

  a_pScnData->m_meshes    = (RTCScene*)malloc(sizeof(RTCScene*)*a_numMeshes);  
  a_pScnData->m_numMeshes = a_numMeshes;
}


void c_init_hydra_scene()
{
  g_data.scnData.m_meshes    = 0;
  g_data.scnData.m_numMeshes = 0;

  g_data.scnInst.m_instances   = 0;
  g_data.scnInst.m_instanceNum = 0;
        

}

int c_test_add(int a, int b)
{
  return a+b;
}
