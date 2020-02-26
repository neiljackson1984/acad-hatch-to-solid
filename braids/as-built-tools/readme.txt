2016-12-28

This is a set of scripts and templates that I use to create as-built drawings.

The goal is that, when walking away from the building that you are measuring, the following will exist.

A dwg file that contains in model space (optionally nested within block definitions, arbitrary nesting depth allowed):
even that interpretation is not quite correct because the floor boundary diverges from wallUnbroken wherever there is a door)
Closed polylines on the wallUnbroken layer (think about distinction between wall and floorPatchBoundary.  In full generality, 'wall' is a mode that can exist on a portion of floorBoundary.  In practice here, we have often conflated the two concepts.  It may make sense to regard polyline in the wallUnbroken layer as denoting floorBoundary with wall mode == true.  ) 

lines in the doorAnchor layer
lines in the doorGuideNonPivotingSide

lines in the windowAnchor layer
lines in the windowGuide layer

'door' and 'window' in this context have a particular meaning that is broader than the colloquial sense of these words.  
'door' is (I am defining the term) an opening in wall which causes a swath of floor to sweep from one side of the opening to the other.  If you start with two disconnected regions of floor, each bounded by a closed loop of wallUnbroken, then
you insert a door in two adjacent portions of the floorPatch's respective wallUnbroken/floorPatchBoundary boundaries, the result is that the two disconnected regions of floor become connected via the door.
'window' is (I am defining the term) an opening in wall which does not imply the connection between floor regions that a door would imply.

An opening in a wall (e.g. a door or a window) is modelled with two lines: one being collinear with one portion of wallUnbroken and the other being collinear with another (adjacent) portion of wallUnbroken.
These two lines represent the measurable features that the surveyor will encounter on both sides of the wall that is pierced by an opening.  In order to facilitate identifying a single opening, and to record the correspondence between the lines and the wall openings, 
we say that an opening in a wall has one 'anchor' line and one 'guide' line.  For an opening (regarded as the complete opening feature, meaning the literal opening and the hardware that sits within that opening) whose chirality needs
to be specified (chirality is not the perfect word - what I am after is specifying which end of the door line the hinge is on, and which direction the door swings, for instance), the direction (start point to end point) of the anchor line is significant. 
Also, the   distinction between anchor and guide is signifcant for swinging doors - the anchor line represents the side of the opening that the door swings into.  The start point of the anchor line represents the door's pivot point.
There is a one-to-one correspondence between wall openings and (anchor line, guide line) pairs.

The anchor and guide line corresponding to an opening shall each be tagged (via the attachedString functions) with a string that contains the opening's id.  
Door ids are of the form "door[\d]+".  Window ids are of the form "window[\d]+".

To automatically assign an id to each line in the doorAnchor and windowAnchor layer, run 
	(autoNumberDoorsAndWindows)
This will assign an id to each line on the doorAnchor layer and each line on the windowAnchor layer.  
This routine overwrites all existing assigned ids, so use caution.
TO DO: respect existing id assignments rather than overwriting, perhaps.
It is possible that the same line entity appears more than once in modelSpace.  This can happen if modelSpace contains multiple block references pointing to the block definition to which that line belongs.  The autoNumbering 
routine checks the entity handles of the lines that it processes and is designed to process each line only once.



To see the ids assigned to each line (and which end of each line is the start (akak hinge) point, which will be circled), run:
	(annotateAllDoorAndWindowLines) 


This will draw some entities in modelSpace on the programmaticAnnotations layer.  These entities are entirely for the user's benefit - they have no formal significance to the rest of the system.  Everything on the programmaticAnnotations layer will be deleted when you call the function again.
To manually clear the programmaticAnnotations, run
	(clearAnnotations)

The algorithm that is used by autoNumberDoorsAndWindows, annotateAllDoorAndWindowLines to traverse through and recurse into (possibly nested) block references has no way of 
seeing whether an entity in a block definition is construction geometry (i.e. has been converted to construction geometry by means of the BCONSTRUCTION command).
This is because I can find no programmatic way to detect whether an entity has been marked as construction geometry.
In general, I want the block-definition traversal routine to ignore entities (to ignore a block reference means not to recurse into it) that are construction geometry.
As a work-around to this issue, the block traversal routine looks at the attachedString of each entity, and if the attachedString is exactly "construction", 
the block traversal routine ignores that entity.  Therefore, when the user invokes the BCONSTRUCTION command to mark an entity as construction geometry, he ought to, at the same time,
set the attachedString of that entity to "construction".  He can do this by using the command 
	FLAG-AS-CONSTRUCTION 
Note: this will overwrite any existing attachedSring (this is a very crude hack), so it should not be used on meaningful doorAnchor lines and windowAnchor lines.
Update: I figured out how to programatically detect BCONSTRUCTION status (it is stored as xdata).  The function (isBConstrution) is where the magic is.
	
	
The automatic assignment of ids only affects anchor lines.  The user will have to manually set the ids of guide lines.  To facilitate this, run
	COPY-ATTACHED-STRING
This will prompt you to select a source and a destination entity and will copy the source entity's attached string to the destination entity (but only if the destination entity does not already have an id string).

To manually set the attached string of an entity, run
	nSET-ATTACHED-STRING  (for nested entities)
	or  SET-ATTACHED-STRING  (traditional selection mode)
These functions, which support both noun-verb and verb-noun usage, prompt you to enter the string that you want to assign.

Door ids and window ids are the keys into a database that specifies properties that I will describe later.  It is by means of this database that you specify, among other things) the type of door (sliding glass door, double door, ...) or window.

