pro tdtest

; Create a 48x64 cylinder with a constant radius of 0.25:
MESH_OBJ, 3, Vertex_List, Polygon_List, $
   Replicate(0.25, 48, 64), P4=0.5
; Transform the vertices:
T3D, /RESET 
T3D, ROTATE=[0.0, 30.0, 0.0] 
T3D, ROTATE=[0.0, 0.0, 40.0] 
T3D, TRANSLATE=[0.25, 0.25, 0.25]
VERTEX_LIST = VERT_T3D(Vertex_List)
; Create the window and view:
WINDOW, 0, XSIZE=512, YSIZE=512 
CREATE_VIEW, WINX=512, WINY=512
; Render the mesh:
SET_SHADING, LIGHT=[-0.5, 0.5, 2.0], REJECT=0 
TVSCL, POLYSHADE(Vertex_List, Polygon_List, /NORMAL)
; Create a cone (surface of revolution):
MESH_OBJ, 6, Vertex_List, Polygon_List, $
   [[0.75, 0.0, 0.25], [0.5, 0.0, 0.75]], $
   P1=16, P2=[0.5, 0.0, 0.0]
; Create the window and view:
WINDOW, 0, XSIZE=512, YSIZE=512 
CREATE_VIEW, WINX=512, WINY=512, AX=30.0, AY=(140.0), ZOOM=0.5
; Render the mesh:
SET_SHADING, LIGHT=[-0.5, 0.5, 2.0], REJECT=0 
TVSCL, POLYSHADE(Vertex_List, Polygon_List, /DATA, /T3D)

END