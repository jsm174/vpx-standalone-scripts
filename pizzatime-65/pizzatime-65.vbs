'                        ___
'                        |  ~~--.
'                        |%=@%%/
'                        |o%%%/
'                     __ |%%o/
'               _,--~~ | |(_/ ._
'            ,/'  m%%%%| |o/ /  `\.
'           /' m%%o(_)%| |/ /o%%m `\
'         /' %%@=%o%%%o|   /(_)o%%% `\
'        /  %o%%%%%=@%%|  /%%o%%@=%%  \
'       |  (_)%(_)%%o%%| /%%%=@(_)%%%  |
'       | %%o%%%%o%%%(_|/%o%%o%%%%o%%% |
'       | %%o%(_)%%%%%o%(_)%%%o%%o%o%% |
'       |  (_)%%=@%(_)%o%o%%(_)%o(_)%  |
'        \ ~%%o%%%%%o%o%=@%%o%%@%%o%~ /
'         \. ~o%%(_)%%%o%(_)%%(_)o~ ,/
'           \_ ~o%=@%(_)%o%%(_)%~ _/
'             `\_~~o%%%o%%%%%~~_/'
'                `--..____,,--'
'
'         d8b                                d8b
'         Y8P                           888  Y8P               
'                                       888
' 88888b. 8888888888888888888 8888b.  8888888888888     888.888888.
' 888 "88b888   d88P    d88P     "88b   888  8888888   8888888  888
' 888  888888  d88P    d88P  .d888888   888  88888888 8888888888888
' 888 d88P888 d88P    d88P   888  888   888  88888 8888 88888b
' 88888P" 8888888888888888888"Y888888   888  88888  88  888"88888Y
' 888                                 
' 888                                 
' 888
' By ScottyWic
'////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////
' Version Notes
' Thanks to my wife,
' without her constant disapproval there's no way i would have finished.
'
'////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////
Option Explicit
Randomize

'TODOs
'
Const WeakComputer		= False	' IF your machine is having trouble running this smoothly, try this. 
Const DMDMode			= 0 	' 0=None, 2=PUP (make sure you set PuPDMDDriverType below)
Const helpfulcalls		= 1     ' 0=off 1=on for helpful callouts by Crust
Const houseband			= 1     ' 0=off commercial punk soundtrack 1=on for the PizzaTime House band!
Const BallFinderOn		= 1     ' 0=off this allows the tables ball finder script to be on. If you're having issues with it, turn it off.
Const osbactive			= 0 	' Orbital Scoreboard: Set to 0 for off, 1 for only player 1 to be sent, 2 for all scores to be sent.	
								'     See link to create obs.vbs: https://docs.orbitalpin.com/vpx-user-settings
Const FontScale			= .5	' Scales the PupFonts up/down for different sized DMDs  [0.5 Desktop]
Const PreloadMe 		= 1     ' Go through flasher sequence at table start, to prevent in-game slowdowns 
Const TableName = "pizzatime"
Const cGameName = "pizzatime"
Const myVersion = "0.64"

'**************************
'   High Score Resetter
'**************************
' please be careful with this. It will kill your score and we can't get them back.
' games played will not be affected.

Const resetScores				= False	' set to true to set the first flag
Const areYouSure				= ""  	' change to "i am serious" to set the second flag
'once you set both of these, load the table. It will clear your scores on startup. 
'Then close the table and change these values back to false and ""


'Const UseFullColor = "True" 			'	ULTRA: Enable full Color on UltraDMD "True" / "False"
'Const UltraDMDVideos = True				'	ULTRA: Works on my DMDv3 but seems it causes issues on others
'**************************
'   PinUp Player USER Config
'**************************
dim PuPDMDDriverType: PuPDMDDriverType=0   ' 0=LCD DMD, 1=RealDMD (For FULLDMD use the batch scripts in the pup pack)
dim useRealDMDScale : useRealDMDScale=0    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=True	   ' true or false to use DMD splash videos.


Const VolBGMusic = 90  ' Volume for Video Clips  
Const VolMusic = 90	' Volume for Gameplay music
Const VolDef = 0.5		' Default volume 
Const VolSfx = 60		' Volume for table Sound effects 


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  1 LOAD CORE & MATH
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  



	Const BallSize = 51
	Const BallMass = 1.2


	Dim WshShell
	Set WshShell = CreateObject("WScript.Shell")
	LoadCoreFiles

	Sub LoadCoreFiles
		On Error Resume Next
		ExecuteGlobal GetTextFile("core.vbs")
		If Err Then MsgBox "Can't open core.vbs"
		ExecuteGlobal GetTextFile("controller.vbs")
		If Err Then MsgBox "Can't open controller.vbs"
		On Error Goto 0

		'============================'  Orbital Scoreboard'============================	
		if osbactive = 1 or osbactive = 2 Then		
			On Error Resume Next		
			ExecuteGlobal GetTextFile("osb.vbs")	
			On Error Goto 0
		end if


	End Sub

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Math Functions
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Const Pi = 3.1415927

	Function dSin(degrees)
		dsin = sin(degrees * Pi/180)
	End Function

	Function dCos(degrees)
		dcos = cos(degrees * Pi/180)
	End Function

	Function RndNum(min, max)
		RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
	End Function

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'
'   Player States 
'
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


Const bHardMode	= True
		if bHardMode then 
			Primitive28.visible = False
			Primitive27.visible = False 
			Rubber7.collidable = False
			Rubber7.Visible = False 
			Rubber6.collidable = False
			Rubber6.Visible = False 
		End If 

Dim PlayerState(5)
Class TableState
	Public SkillshotValue
	Public s_ufoPizzasCollected
	Public s_ufolock
	Public ufo1lon
	Public ufo2lon
	Public ufo3lon
	Public ufo4lon
	Public ufo5lon
	Public pinel1on
	Public pinel2on
	Public olivel1on
	Public olivel2on
	Public mushl1on
	Public mushl2on
	Public mushl3on
	Public sausl1on
	Public sausl2on
	Public sausl3on
	Public pepl1on
	Public pepl2on
	Public pepl3on
	Public baconl1on
	Public baconl2on
	Public baconl3on
	Public fish1on
	Public fish2on
	Public roni1on
	Public roni2on
	Public roni3on
	Public fish3on
	Public pz1l1on
	Public pz1l2on
	Public pz1l3on
	Public pz1l4on
	Public pz2l1on
	Public pz2l2on
	Public pz2l3on
	Public pz2l4on
	Public pz2l5on
	Public pz2l6on
	Public pz2l7on
	Public pz2l8on
	Public pz3l1on
	Public pz3l2on
	Public pz3l3on
	Public pz3l4on
	Public pz3l5on
	Public pz3l6on
	Public pz3l7on
	Public pz3l8on
	Public pz3l9on
	Public pz3l10on
	Public pz3l11on
	Public pz3l12on
	Public pz4l1on
	Public pz4l2on
	Public pz4l3on
	Public pz4l4on
	Public pz4l5on
	Public pz4l6on
	Public pz4l7on
	Public pz4l8on
	Public pz4l9on
	Public pz4l10on
	Public pz4l11on
	Public pz4l12on
	Public pz4l13on
	Public pz4l14on
	Public pz4l15on
	Public pz4l16on
	Public tipjartlon
	Public tipjarilon
	Public tipjarplon
	Public tipjarjlon
	Public tipjaralon
	Public tipjarrlon
	Public tipped1on
	Public tipped2on
	Public tipped3on
	Public tipped4on
	Public s_addaballmade
	Public s_extraballgiven
	Public s_currentpizza		' Type Of Pizza we are making 
	Public s_pizzasize			' Which size mode are we on
	Public s_pzlnum				' Number of ingredients so far
	Public s_pzlnum2			' For second time through game
	Public s_ronibumps			' Current Pepperoni bumper count 
	Public s_totalronis			
	Public s_ptready			' PizzaTimeReady
	Public s_videoready			' PizzaTimeReady
	Public s_modelight
	Public s_pizzaorder			' PizzaOrder State

	Public s_tips
	Public s_gottips
	Public VideoModeCount			' How many time did we hit video mode
	Public Specialties(7)
	Public SMode					' Current Specialty Mode we are in 
	Public SModeProgress(6)			' Current progress for Specialty modes 
	Public SModePercent(6)			' Current progress % for Specialty modes 
	Public pizzaPartyProgress(9)	' How many PizzaParty hits do we have so far
	Public ecwheel1on
	Public ecwheel2on
	Public ecwheel3on
	Public ecwheel4on
	Public aablighton
	Public ebnowlit
	Public s_glorycount
	Public ismysteryon
	Public s_cheesevalue
	Public s_ufolightlock
	Public s_bOnTheFirstBall
	Public completedBBCount		' How many times we beat the game
	Public s_bEB_Video
	Public s_bEB_Modes
	Public s_bEB_Eat
	Public s_EBQueue
	Public s_spinCount
	Public s_bWizardModeAteFinished
	Public s_bWizardModeIllumFinished
	Public s_bWizardModeSupremeFinished

	Public Sub Reset()
				dim a
				For each a in aGiLights
				a.color = RGB(253, 181, 47)
				a.colorfull = RGB(255, 255, 255)
				next
		dim i, j		

		SMode = -1			' CurrentMode - Default to No mode
		VideoModeCount = 0	' Number of video mode starts 
		For j = 0 to 5 
			SModePercent(j) = 0
			SModeProgress(j) = 0
		next 
		for j = 0 to 8
			pizzaPartyProgress(j) = 0
		next 

		SkillshotValue = 60000 ' increases by 100000 each time it is collected

		Specialties(0) = 25	' spins
		Specialties(1) = 2	' Locks
		Specialties(2) = 9	' Ramps
		Specialties(3) = 6	' Multipliers
		Specialties(4) = 8	' slices Eaten
		Specialties(5) = 20	' Loop-de-Loops
		Specialties(6) = 6	' Tip Bumper

		ismysteryon = False

	End Sub

	Public Sub Save()	' Grab all the current Table states and save into this player
		dim i

		ufo1lon = ufo1l.state
		ufo2lon = ufo2l.state
		ufo3lon = ufo3l.state
		ufo4lon = ufo4l.state
		ufo5lon = ufo5l.state

		pz1l1on = pz1l1.state 
		pz1l2on = pz1l2.state 
		pz1l3on = pz1l3.state 
		pz1l4on = pz1l4.state 
		pz2l1on = pz2l1.state 
		pz2l2on = pz2l2.state 
		pz2l3on = pz2l3.state 
		pz2l4on = pz2l4.state 
		pz2l5on = pz2l5.state 
		pz2l6on = pz2l6.state 
		pz2l7on = pz2l7.state 
		pz2l8on = pz2l8.state 
		pz3l1on = pz3l1.state 
		pz3l2on = pz3l2.state 
		pz3l3on = pz3l3.state 
		pz3l4on = pz3l4.state 
		pz3l5on = pz3l5.state 
		pz3l6on = pz3l6.state 
		pz3l7on = pz3l7.state 
		pz3l8on = pz3l8.state 
		pz3l9on = pz3l9.state 
		pz3l10on=pz3l10.state
		pz3l11on=pz3l11.state
		pz3l12on=pz3l12.state
		pz4l1on = pz4l1.state 
		pz4l2on = pz4l2.state 
		pz4l3on = pz4l3.state 
		pz4l4on = pz4l4.state 
		pz4l5on = pz4l5.state 
		pz4l6on = pz4l6.state 
		pz4l7on = pz4l7.state 
		pz4l8on = pz4l8.state 
		pz4l9on = pz4l9.state 
		pz4l10on=pz4l10.state
		pz4l11on=pz4l11.state
		pz4l12on=pz4l12.state
		pz4l13on=pz4l13.state
		pz4l14on=pz4l14.state
		pz4l15on=pz4l15.state
		pz4l16on=pz4l16.state
		tipjartlon=tipjartl.state
		tipjarilon=tipjaril.state
		tipjarplon=tipjarpl.state
		tipjarjlon=tipjarjl.state
		tipjaralon=tipjaral.state
		tipjarrlon=tipjarrl.state
		tipped1on=tipped1.state
		tipped2on=tipped2.state
		tipped3on=tipped3.state
		tipped4on=tipped4.state
		'gottips = False

		pinel1on =pinel1.state
		pinel2on =pinel2.state
		olivel1on=olivel1.state
		olivel2on=olivel2.state
		mushl1on =mushl1.state
		mushl2on =mushl2.state
		mushl3on =mushl3.state
		sausl1on =sausl1.state
		sausl2on =sausl2.state
		sausl3on =sausl3.state
		pepl1on  =pepl1.state
		pepl2on  =pepl2.state
		pepl3on	 =pepl3.state
		baconl1on=baconl1.state
		baconl2on=baconl2.state
		fish1on  =fish1.state
		fish2on	 =fish2.state
		fish3on	 =fish3.state
		roni1on=roni1.state
		roni2on=roni2.state
		roni3on=roni3.state

		ecwheel1on = extracheesel1.state
		ecwheel2on = extracheesel2.state
		ecwheel3on = extracheesel3.state
		ecwheel4on = extracheesel4.state
		'aablighton = aablight.state
		'ebnowlit = extraballlight.state
		'ismysteryon = False
'		currentpizza = 0
'		tips = 0
		s_modelight = modelight.state
		s_pizzaorder = pizzaorder.state

		s_glorycount = glorycount
		s_ufoPizzasCollected = ufoPizzasCollected
		s_gottips = gottips
		s_tips = tips
		s_ufolock = ufolock
		s_ronibumps = ronibumps
		s_ptready = ptready
		s_pzlnum = pzlnum
		s_pzlnum2 = pzlnum2
		s_videoready = videoready
		s_addaballmade = addaballmade
		s_extraballgiven = extraballgiven
		s_currentpizza = currentpizza
		s_pizzasize = pizzasize
		s_cheesevalue = cheesevalue
		s_totalronis = totalronis
		s_ufolightlock = ufolightlock
		s_bOnTheFirstBall = bOnTheFirstBall
		s_bEB_Video = bEB_Video
		s_bEB_Modes = bEB_Modes
		s_bEB_Eat = bEB_Eat
		s_EBQueue = EBQueue
		s_spinCount = spinCount
		s_bWizardModeAteFinished = bWizardModeAteFinished
		s_bWizardModeIllumFinished = bWizardModeIllumFinished
		s_bWizardModeSupremeFinished = bWizardModeSupremeFinished

'		pizzasize = 1
'		glorycount = 0
'		cheesevalue = 0

	End Sub

	Public Sub Restore()	' Retore all the Table states
		Dim i
		'if PlayersPlayingGame = 1 then exit sub ' Just in case I missed something We dont have to do this on single player

		ufo1l.state = ufo1lon
		ufo2l.state = ufo2lon
		ufo3l.state = ufo3lon
		ufo4l.state = ufo4lon
		ufo5l.state = ufo5lon

		pz1l1.state =pz1l1on 
		pz1l2.state =pz1l2on 
		pz1l3.state =pz1l3on 
		pz1l4.state =pz1l4on 
		pz2l1.state =pz2l1on 
		pz2l2.state =pz2l2on 
		pz2l3.state =pz2l3on 
		pz2l4.state =pz2l4on 
		pz2l5.state =pz2l5on 
		pz2l6.state =pz2l6on 
		pz2l7.state =pz2l7on 
		pz2l8.state =pz2l8on 
		pz3l1.state =pz3l1on 
		pz3l2.state =pz3l2on 
		pz3l3.state =pz3l3on 
		pz3l4.state =pz3l4on 
		pz3l5.state =pz3l5on 
		pz3l6.state =pz3l6on 
		pz3l7.state =pz3l7on 
		pz3l8.state =pz3l8on 
		pz3l9.state =pz3l9on 
		pz3l10.state =pz3l10on
		pz3l11.state =pz3l11on
		pz3l12.state =pz3l12on
		pz4l1.state =pz4l1on 
		pz4l2.state =pz4l2on 
		pz4l3.state =pz4l3on 
		pz4l4.state =pz4l4on 
		pz4l5.state =pz4l5on 
		pz4l6.state =pz4l6on 
		pz4l7.state =pz4l7on 
		pz4l8.state =pz4l8on 
		pz4l9.state =pz4l9on 
		pz4l10.state =pz4l10on
		pz4l11.state =pz4l11on
		pz4l12.state =pz4l12on
		pz4l13.state =pz4l13on
		pz4l14.state =pz4l14on
		pz4l15.state =pz4l15on
		pz4l16.state =pz4l16on
		tipjartl.state = tipjartlon
		tipjaril.state = tipjarilon
		tipjarpl.state = tipjarplon
		tipjarjl.state = tipjarjlon
		tipjaral.state = tipjaralon
		tipjarrl.state = tipjarrlon
		if tipjarrlon>0 then tiplock

		if tips>5 then 
			if gottips then
			else
			tiplock
			end If
		end if

		tipped1.state =tipped1on
		tipped2.state =tipped2on
		tipped3.state =tipped3on
		tipped4.state =tipped4on
		'gottips = False

		pinel1.state=  pinel1on 
		pinel2.state=  pinel2on 
		olivel1.state= olivel1on
		olivel2.state= olivel2on
		mushl1.state=  mushl1on 
		mushl2.state=  mushl2on 
		mushl3.state=  mushl3on 
		sausl1.state=  sausl1on 
debug.print "sausl1.state:" & sausl1.state & " stored:" & sausl1on
		sausl2.state=  sausl2on 
		sausl3.state=  sausl3on
		pepl1.state=   pepl1on
		pepl2.state=   pepl2on
		pepl3.state=   pepl3on
		baconl1.state= baconl1on
		baconl2.state= baconl2on
		fish1.state=   fish1on
		fish2.state=   fish2on
		fish3.state=   fish3on
		roni1.state=   roni1on
		roni2.state=   roni2on
		roni3.state=   roni3on

		extracheesel1.state = ecwheel1on
		extracheesel2.state = ecwheel2on
		extracheesel3.state = ecwheel3on
		extracheesel4.state = ecwheel4on
		modelight.state = s_modelight
		'pizzaorder.state = s_pizzaorder				' Dont restore/use this one

		glorycount = s_glorycount
		ufoPizzasCollected =  s_ufoPizzasCollected
		gottips = s_gottips
		tips = s_tips
		ufolock = s_ufolock
		ronibumps = s_ronibumps
		ptready = s_ptready
		pzlnum = s_pzlnum
		pzlnum2 = s_pzlnum2
		videoready = s_videoready
		addaballmade = s_addaballmade
		extraballgiven = s_extraballgiven
		currentpizza = s_currentpizza
		pizzasize = s_pizzasize 
		cheesevalue = s_cheesevalue
		totalronis = s_totalronis
		ufolightlock = s_ufolightlock
		bEB_Video = s_bEB_Video
		bEB_Modes = s_bEB_Modes
		bEB_Eat = s_bEB_Eat
		spinCount = s_spinCount
		bWizardModeAteFinished = s_bWizardModeAteFinished
		bWizardModeIllumFinished = s_bWizardModeIllumFinished
		bWizardModeSupremeFinished = s_bWizardModeSupremeFinished

		pizzatype

		if s_EBQueue = 0 then 
			StopExtraBall
		Else
			StartExtraBall
		End if 
		EBQueue = s_EBQueue

		if bWizardModeAteFinished then 
			iateitall.state = 2
		Else 
			iateitall.state = 0
		End if 

		if bWizardModeIllumFinished then 
			illuminati.state = 2
		Else 
			illuminati.state = 0
		End if 

		if bWizardModeSupremeFinished then 
			supreme.state = 2
		Else 
			supreme.state = 0
		End if 

		'bOnTheFirstBall = s_bOnTheFirstBall		' Dont think we need this (yet)

		'aablight.state = aablighton
		'extraballlight.state = ebnowlit

		If s_cheesevalue = 0 Then
			cheese4l.opacity = 0
			cheese3l.opacity = 0
			cheese2ll.opacity = 0
			cheese1l.opacity = 0
		End If

		If s_cheesevalue = 1 Then
			cheese4l.opacity = 2000
			cheese3l.opacity = 0
			cheese2ll.opacity = 0
			cheese1l.opacity = 0
		End If

		If s_cheesevalue = 2 Then
			cheese4l.opacity = 2000
			cheese3l.opacity = 2000
			cheese2ll.opacity = 0
			cheese1l.opacity = 0
		End If

		If s_cheesevalue = 3 Then
			cheese4l.opacity = 2000
			cheese3l.opacity = 2000
			cheese2ll.opacity = 2000
			cheese1l.opacity = 0
		End If

		If s_cheesevalue = 4 Then
			cheese4l.opacity = 2000
			cheese3l.opacity = 2000
			cheese2ll.opacity = 2000
			cheese1l.opacity = 2000
		End If

		' Restore mode states
		L34.state = 0
		L35.state = 0
		L39.state = 0
		L40.state = 0
		L41.state = 0
		L43.state = 0
		if SModePercent(0) > 0 then L34.state = 2
		if SModePercent(0) >= 100 then L34.state = 1
		if SModePercent(1) > 0 then L35.state = 2
		if SModePercent(1) >= 100 then L35.state = 1
		if SModePercent(2) > 0 then L39.state = 2
		if SModePercent(2) >= 100 then L39.state = 1
		if SModePercent(3) > 0 then L40.state = 2
		if SModePercent(3) >= 100 then L40.state = 1
		if SModePercent(4) > 0 then L41.state = 2
		if SModePercent(4) >= 100 then L41.state = 1
		if SModePercent(5) > 0 then L43.state = 2
		if SModePercent(5) >= 100 then L43.state = 1

		' Update based on player
		puPlayer.LabelSet pBackglass,"CollectVal1",	PlayerState(CurrentPlayer).Specialties(0)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal2",	PlayerState(CurrentPlayer).Specialties(1)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal3",	PlayerState(CurrentPlayer).Specialties(2)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal4",	PlayerState(CurrentPlayer).Specialties(3)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal5",	PlayerState(CurrentPlayer).Specialties(4)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal6",	PlayerState(CurrentPlayer).Specialties(5)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal7",	PlayerState(CurrentPlayer).Specialties(6)	,1,""

	End Sub
End Class 

Sub TableState_Init(Index)
	Set PlayerState(Index) = New TableState
	PlayerState(Index).Reset
	PlayerState(Index).completedBBCount = 0
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Overall Constants / Global Variables / Table Init
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	' Define any Constants

	Const MaxPlayers = 4
	Const MaxMultiplier = 6 'limit to 6x in this game
	Const MaxMultiballs = 4  ' max number of balls during multiballs

	' Special mode indexes
	Const kSpecial_Spins		=0
	Const kSpecial_Locks		=1
	Const kSpecial_Ramps		=2
	Const kSpecial_Multiplier	=3
	Const kSpecial_SlicesEaten	=4
	Const kSpecial_LoopDeLoop	=5
	Const kSpecial_Tip			=6

	' Bonus mode indexes
	Const kBonus_Spins		=0
	Const kBonus_Locks		=1
	Const kBonus_Ramps		=2
	Const kBonus_SlicesEaten	=3
	Const kBonus_LoopDeLoop	=4
	Const kBonus_Bumpers		=5


	' Define Global Variables
	Dim PlayersPlayingGame
	Dim CurrentPlayer
	Dim Credits
	Dim BonusTotals(6)				' Stores Bonuses for this ball
	Dim BonusPoints(4)
	Dim BonusHeldPoints(4)
	Dim BonusMultiplier(4)
	Dim bBonusHeld
	Dim BallsRemaining(4)
	Dim ExtraBallsAwards(4)
	Dim Score(4)
	Dim HighScore(4)
	Dim HighScoreName(4)
	Dim Jackpot
	Dim SuperJackpot
	Dim Tilt
	Dim TiltSensitivity
	Dim Tilted
	Dim TotalGamesPlayed
	Dim mBalls2Eject
	Dim bAutoPlunger
	Dim bAutoPlunged
	Dim bInstantInfo
	Dim bromconfig
	Dim bAttractMode
	Dim bpgcurrent
	Dim bstcurrent
	'Dim gamemodecurrent
	Dim profanitycurrent
	Dim Magnet
	Dim ModeTimerInc

	' Define Game Control Variables
	Dim LastSwitchHit
	Dim BallsOnPlayfield
	Dim BallsInLock(4)
	Dim BallsInHole

	Dim bWizardModeAteReady
	Dim bWizardModeAte
	Dim bWizardModeAteFinished

	Dim bWizardModeIllumReady
	Dim bWizardModeIllum
	Dim bWizardModeIllumFinished

	Dim bWizardModeSupreme
	Dim bWizardModeSupremeFinished
	Dim bWizardModeSupremeReady
	Dim bWizardModeSupremeMB1
	Dim bWizardModeSupremeMB2
	Dim WizardModeSupremeProgress(9)	' How many wizard mode hits do we have so far

	Dim P1ScoreColor
	Dim P2ScoreColor
	Dim P3ScoreColor
	Dim P4ScoreColor

	Const kCheckSize = 20					' Max Entries in the check message array
	Dim CheckArray(20,2)					' Stores the messages for the chec k


	' Define Game Flags
	Dim bFreePlay
	Dim bGameInPlay
	Dim bOnTheFirstBall
	Dim bBallInPlungerLane
	Dim bBallSaverActive
	Dim bBallSaverReady
	Dim bMultiBallMode
	Dim bMusicOn
	Dim bSkillshotReady
	Dim bExtraBallWonThisBall
	Dim bEB_Video
	Dim bEB_Modes
	Dim bEB_Eat
	Dim EBQueue

	Dim bJustStarted
	Dim plungerIM 'used mostly as an autofire plunger
	Dim DisableInstantInfo
	Dim DisableFlippers
	Dim bUsePUPDMD
	dim strModeSong
	Dim MusicDir
	MusicDir="Music"
	Dim LFPress, RFPress
	Dim bSuperSkillShotEnabled


	Sub Table1_Init()

		Set Magnet = New cvpmMagnet
		With Magnet
			.InitMagnet Magnet11, 60
			.GrabCenter = True
			.MagnetOn = 0
			.CreateEvents "Magnet"
		End With
		Dim i
		Randomize
		'Impulse Plunger as autoplunger
		Const IMPowerSetting = 45 ' Plunger Power
		Const IMTime = 1.1        ' Time in seconds for Full Plunge
		Set plungerIM = New cvpmImpulseP
		With plungerIM
			.InitImpulseP swplunger, IMPowerSetting, IMTime
			.Random 1.5
			.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
			.CreateEvents "plungerIM"
		End With
		LoadEM

		bUsePUPDMD = True
			

		' Needs to start up so it is actually collidable (VPX doesnt actually move things physically, it just changes colidable and animateds visually 
		'VideoModePopup.z = -86
		'VideoModePopup.collidable = False 

		'ExtraBallPopup.z = -86
		'ExtraBallPopup.collidable = False

		StackState_Init

		bSuperSkillShotEnabled = False 
		UpdateNumberPlayers
		bResetCurrentGame = False
		strModeSong = ""
		Loadhs
		bpgcurrent = 3
		bstcurrent = 5
		bFreePlay = True
		profanitycurrent = True
		bAttractMode = False
		bOnTheFirstBall = False
		bBallInPlungerLane = False
		bBallSaverActive = False
		bBallSaverReady = False
		bMultiBallMode = False
		bGameInPlay = False
		bAutoPlunger = False
		bAutoPlunged = False 
		bMusicOn = True
		DisableInstantInfo = false
		BallsOnPlayfield = 0
		For i = 0 to 4
			BallsInLock(i) = 0
		Next

		BallsInHole = 0
		LastSwitchHit = ""
		Tilt = 0
		Saves = 0
		Drains = 0
		TiltSensitivity = 6
		Tilted = False
		bBonusHeld = False
		bJustStarted = True
		bInstantInfo = False
		DisableFlippers = False
		PuPInit
		GiOff
		StartAttractMode()
	End Sub

	Sub Table1_Exit()
		If b2son then Controller.Stop
		Savehs
	End Sub 



	'//////////////////////////////////////////////////////////////////////
	'// SAUCER MOVEMENT 
	'//////////////////////////////////////////////////////////////////////
	Dim cBall, mMagnet

	Sub WobbleMagnet_Init
		Set mMagnet = new cvpmMagnet
		With mMagnet
			.InitMagnet WobbleMagnet, 2
			.Size = 100
			.CreateEvents mMagnet
			.MagnetOn = True
		End With
		mMagnet.MagnetON = True
		Set cBall = ckicker.createball:ckicker.Kick 0,0:mMagnet.addball cball
		UfoShaker.Interval = 10
		UfoShaker.Enabled = True
	End Sub

	Sub UfoShake(Enabled) 
		If Enabled Then
			BigUfoShake
			PlaySound SoundFX("SaucerShake",DOFShaker),0,2,0,0,0,0,1,-.3
		End If
	End Sub

	Sub BigUfoShake
		cball.velx = 4
		cball.vely = -18
		DOF 111, DOFPulse
	End Sub

	Sub UfoShaker_Timer
		Dim a, b, c
		a = (ckicker.y - cball.y) / 5
		b = (ckicker.y - cball.y) / 10
		c = (cball.x - ckicker.x) / 5

		PTSS.rotx = a:PTSS.transz = b:PTSS.rotz = c
		'LED4.rotx = a:LED4.transz = b:LED4.rotz = c
	End Sub

	Sub tmrUFOSpin_Timer 
		tmrUFOSpin.UserValue=tmrUFOSpin.UserValue+1
		PTSS.roty=PTSS.roty+5
		if tmrUFOSpin.UserValue > 360/5 then 
			tmrUFOSpin.Enabled = False 
			PTSS.roty=0
		End if 
	End Sub 

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Keys & Flippers
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Sub Table1_KeyDown(ByVal keycode)
		If Keycode = AddCreditKey Then
			Credits = Credits + 1
			DOF 140, DOFOn
			If(Tilted = False) Then
				'DMD "black.png", "CREDITS " &credits, "PRESS START",  2000
				PlaySound "insertcoin"
				If NOT bGameInPlay Then ShowTableInfo
			End If
		End If

		If keycode = RightMagnaSave Then
			if modepickin=1 Then
				startSpecialMode()
			end if
		end if 

		If keycode = PlungerKey Then
			PlaySound "fx_plungerpull"
			Plunger.Pullback
			if modepickin=1 Then
				startSpecialMode()
			end if
		End If

		If keycode = LeftFlipperKey Then 		
		LFPress = 1
		If bAttractMode = True Then
			dmdintroloop:introtime=0
debug.print "attract left flipper" 
		End If
		end If
		If keycode = RightFlipperKey Then 
		If bAttractMode = True Then
			dmdintroloop:introtime=0
debug.print "attract left flipper" 
		End If
			RFPress = 1	
		end if 


		If hsbModeActive Then
			EnterHighScoreKey(keycode)
			Exit Sub
		End If
if (keycode = 17) then ' w key
	'HighScoreEntryInit
	BonusScene


End If
		If bGameInPlay Then

			' Debug mode 
			If (keycode = LeftMagnaSave or keycode = RightMagnaSave) then 
				HandleDebugDown(keycode)
			End If

			if bBallInPlungerLane and keycode = StartGameKey then 	' Start a timer to reset game 
				tmrHoldKey.Enabled = False
				tmrHoldKey.Enabled = True
			End If


			If NOT Tilted Then
				If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25:CheckTilt
				If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25:CheckTilt
				If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25:CheckTilt

if (keycode = 17) then ' w key

BeamFlash.opacity = 100	

'	PlayerState(CurrentPlayer).Specialties( 0) = 1
'	PlayerState(CurrentPlayer).Specialties( 1) = 1
'	PlayerState(CurrentPlayer).Specialties( 2) = 1
'	PlayerState(CurrentPlayer).Specialties( 3) = 1
'	PlayerState(CurrentPlayer).Specialties( 4) = 1
'	PlayerState(CurrentPlayer).Specialties( 5) = 1
'	PlayerState(CurrentPlayer).Specialties( 6) = 1

PlayerState(CurrentPlayer).SModePercent(0)=100
PlayerState(CurrentPlayer).SModePercent(1)=100
PlayerState(CurrentPlayer).SModePercent(2)=100
PlayerState(CurrentPlayer).SModePercent(3)=100



' Back Ramp
'	KickerTest.CreateBall
'	KickerTest.Kick 70, 50

' Right Drain
'	KickerTest.CreateBall
'	KickerTest.Kick 110, 50

'	tmrUFOSpin.UserValue=0
'	tmrUFOSpin.Enabled = True

'	StartExtraBall

'	bWizardModeSupremeReady = True
'	ufolocklight.state =2
'	pizzaorder.state = 2
'	modelight.state = 2

'	bWizardModeIllumReady = True

'	StartWizardAte

'	FlasherSequence.Enabled = True
End if 
				If keycode = LeftFlipperKey Then 
					if PuPGameRunning Then
						PuPGameInfo= PuPlayer.GameUpdate("PUPShooter", 1 , 83 , "")  'w
					Else
						if modepickin=1 Then
							if mcycle=0 Then
								mcycle=5
							else
								mcycle = mcycle -1
							end If
							modepick
						end if
						SolLFlipper 1
						SolULFlipper 1
						StartInstantInfo(keycode)
'						If DisableInstantInfo = false then
'							InstantInfoTimer.Enabled = True
'						End If
					End If 
				End If
				If keycode = RightFlipperKey Then
					if PuPGameRunning Then
						PuPGameInfo= PuPlayer.GameUpdate("PUPShooter", 1 , 87 , "")  's
					Else
						if modepickin=1 Then
							if mcycle=5 Then
								mcycle=0
							else
								mcycle = mcycle +1
							end If
							modepick
						end if
						SolRFlipper 1
						StartInstantInfo(keycode)
'						If DisableInstantInfo = false then
'							InstantInfoTimer.Enabled = True
'						End If
					End If 
				End If

				If keycode = StartGameKey Then
					If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then
						If(bFreePlay = True) Then
							PlayersPlayingGame = PlayersPlayingGame + 1
							TotalGamesPlayed = TotalGamesPlayed + 1
							'DMD "black.png", " ", PlayersPlayingGame & " PLAYERS",  500
							PlaySound "so_fanfare1"
						Else
							If(Credits> 0) then
								PlayersPlayingGame = PlayersPlayingGame + 1
								TotalGamesPlayed = TotalGamesPlayed + 1
								Credits = Credits - 1
								If Credits < 1 Then DOF 140, DOFOff
								'DMD "black.png", " ", PlayersPlayingGame & " PLAYERS",  500
								PlaySound "so_fanfare1"
							Else
								' Not Enough Credits to start a game.
								'DMD "black.png", "CREDITS " &credits, "INSERT COIN",  500
								PlaySound "so_nocredits"
							End If
						End If
						UpdateNumberPlayers	' Update the screen layout on pup for multiple players
					End If
				End If
			End If
		Else
		'game not in play yet
			If NOT Tilted Then
				If keycode = LeftFlipperKey Then SolLFlipper 0:SolULFlipper 0
				If keycode = RightFlipperKey Then SolRFlipper 0
				If keycode = StartGameKey Then
					If(bFreePlay = True) Then
						If(BallsOnPlayfield = 0) Then
							ResetForNewGame()
						End If
					Else
						If(Credits> 0) Then
							If(BallsOnPlayfield = 0) Then
								Credits = Credits - 1
								If Credits < 1 Then DOF 140, DOFOff
								ResetForNewGame()
							End If
						Else
							' Not Enough Credits to start a game.
							'DMD "black.png", "CREDITS " &credits, "INSERT COIN",  500
							ShowTableInfo
						End If
					End If
				End If
			End If
		End If

'****************
' Testing Keys  
'****************
'if keycode = "3" then
'StartPizzaTime
'End If


	End Sub


	Sub Table1_KeyUp(ByVal keycode)
		tmrHoldKey.Enabled = False
		If keycode = PlungerKey Then
			PlaySound "fx_plunger"
			Plunger.Fire
		End If

		If hsbModeActive Then
			Exit Sub
		End If

		If keycode = LeftFlipperKey or keycode = RightFlipperKey Then
			EndFlipperStatus(keycode)
		End If 

		if LFPress=1 and RFPress = 1 then	' Pressed both at the same time (Skip Scenes if we are in one)
			QueueSkip
		End If 


		If keycode = LeftFlipperKey Then LFPress = False 
		If keycode = RightFlipperKey Then RFPress = False 

		If bGameInPlay Then

			If (keycode = LeftMagnaSave or keycode = RightMagnaSave) then 
				HandleDebugUp(keycode)
			End If

			If keycode = LeftFlipperKey Then
				if PuPGameRunning Then
					PuPGameInfo= PuPlayer.GameUpdate("PUPShooter", 2 , 83 , "")  'w
				else
					SolLFlipper 0
					SolULFlipper 0
'					If DisableInstantInfo = false Then
'						InstantInfoTimer.Enabled = False
'					End If
'					If bInstantInfo Then
'						'DMDScoreNow
'						bInstantInfo = False
'					End If
				End If 
			End If
			If keycode = RightFlipperKey Then
				if PuPGameRunning Then
					PuPGameInfo= PuPlayer.GameUpdate("PUPShooter", 2 , 87 , "")  's
				Else
					SolRFlipper 0
'					If DisableInstantInfo = false Then
'						InstantInfoTimer.Enabled = False
'					End If
'					If bInstantInfo Then
'						'DMDScoreNow
'						bInstantInfo = False
'					End If
				End If 
			End If
		Else
			If keycode = LeftFlipperKey Then SolLFlipper 0:SolULFlipper 0
			If keycode = RightFlipperKey Then SolRFlipper 0
		End If
	End Sub

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Flippers Subs
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Sub SolLFlipper(Enabled)
		If DisableFlippers = False Then
			If Enabled Then
				PlaySound SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), 0, 1, -0.05, 0.15
				LeftFlipper.RotateToEnd
				If bSkillshotReady = False Then
					RotateLaneLightsLeft
				End If
			Else
				PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, -0.05, 0.15
				LeftFlipper.RotateToStart
			End If
		End If
	End Sub


	Sub SolULFlipper(Enabled)
		If DisableFlippers = False Then
			If Enabled Then
				PlaySound SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), 0, 1, -0.05, 0.15
				LeftFlipper2.RotateToEnd
			Else
				PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, -0.05, 0.15
				LeftFlipper2.RotateToStart
			End If
		End If
	End Sub

	Sub SolRFlipper(Enabled)
		If DisableFlippers = False Then
			If Enabled Then
				PlaySound SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), 0, 1, 0.05, 0.15
				RightFlipper.RotateToEnd
				If bSkillshotReady = False Then
					RotateLaneLightsRight
				End If
			Else
				PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), 0, 1, 0.05, 0.15
				RightFlipper.RotateToStart
			End If
		End If
	End Sub

	Sub UpdateNumberPlayers
		' Disable All
		P1ScoreColor="{'mt':2,'color':4079166"
		P2ScoreColor="{'mt':2,'color':4079166"
		P3ScoreColor="{'mt':2,'color':4079166"
		P4ScoreColor="{'mt':2,'color':4079166"

		' Make sure active ones are white
		if PlayersPlayingGame >=1 then P1ScoreColor="{'mt':2,'color':16777215"
		if PlayersPlayingGame >=2 then P2ScoreColor="{'mt':2,'color':16777215"
		if PlayersPlayingGame >=3 then P3ScoreColor="{'mt':2,'color':16777215"
		if PlayersPlayingGame >=4 then P4ScoreColor="{'mt':2,'color':16777215"
		
		' Make sure current player is Orange 
		If CurrentPlayer+1=1 then P1ScoreColor="{'mt':2,'color':33023"
		If CurrentPlayer+1=2 then P2ScoreColor="{'mt':2,'color':33023"
		If CurrentPlayer+1=3 then P3ScoreColor="{'mt':2,'color':33023"
		If CurrentPlayer+1=4 then P4ScoreColor="{'mt':2,'color':33023"

		pUpdateScores	' Force update
	End Sub

	Sub tmrHoldKey_Timer()		' Reset the game with Ball in lane
		tmrHoldKey.Enabled = False
		' TBD Destroy balls
		
		If bBallInPlungerLane and BallsOnPlayfield = 1 Then
			PlaySound "so_fanfare1"
			bResetCurrentGame=True
			if bUsePupDMD then 
				PuPlayer.PlayStop pOverVid						' Stop overlay if there is one
				PuPlayer.SetLoop pOverVid, 0
			End If
			ResetForNewGame()
		End If
	End Sub


'**************************************************
'******** Debug routines
Dim bDebugMode:bDebugMode = False		' Magna buttons perform debug functions
Dim debugEnableCount:debugEnableCount=0
Dim bDebugLeftMagnaDown:bDebugLeftMagnaDown=False
Dim bDebugRightMagnaDown:bDebugRightMagnaDown=False
Sub HandleDebugDown(keycode)
	exit sub 
	If KeyCode = LeftMagnaSave then bDebugLeftMagnaDown = True
	If KeyCode = RightMagnaSave then bDebugRightMagnaDown = True
debug.print "DEBUG L:" & bDebugLeftMagnaDown & " R:" & bDebugRightMagnaDown 
	if (bDebugMode = False) Then	' Turn on debug whne you pres magnas together three times 
		if (bDebugLeftMagnaDown and bDebugRightMagnaDown) then
			debugEnableCount=debugEnableCount+1
			if debugEnableCount = 3 then 
				PlaySoundVol "leftbones", VolDef
				bDebugMode = True
			End If 
		End If 
		Exit Sub
	End if 

	if (bDebugLeftMagnaDown and bDebugRightMagnaDown = False) then 
		vpmtimer.addtimer 200, "HandleDebugLeft '"
	Elseif (bDebugLeftMagnaDown = False and bDebugRightMagnaDown) then 
		vpmtimer.addtimer 200, "HandleDebugRight '"
	elseif (bDebugLeftMagnaDown and bDebugRightMagnaDown) then 	' Setup for Wizard modes 
		if bWizardModeSupreme=False and PlayerState(CurrentPlayer).SModePercent(5)<>100 then 
debug.print "Enable: Supreme"
					playmedia "","audio-supremelit",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "supreme-litt.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			pupDMDDisplay "-", "Supreme^Wizard Mode", "" ,3, 0, 10		'3 seconds
			'PlaySoundVol "leftbones", VolDef
			PlayerState(CurrentPlayer).SModePercent(0)=100 
			PlayerState(CurrentPlayer).SModePercent(1)=100 
			PlayerState(CurrentPlayer).SModePercent(2)=100 
			PlayerState(CurrentPlayer).SModePercent(3)=100 
			PlayerState(CurrentPlayer).SModePercent(4)=100 
			PlayerState(CurrentPlayer).SModePercent(5)=100 
			L34.state = 1
			L35.state = 1
			L39.state = 1
			L40.state = 1
			L41.state = 1
			L43.state = 1

			bWizardModeSupremeReady = True
			ufolocklight.state =2
			pizzaorder.state = 2
			modelight.state = 2

		elseif pizzasize < 4 then '  Belly Buster Ready
debug.print "Enable: Belly"
			pupDMDDisplay "-", "Belly Buster", "" ,3, 0, 10		'3 seconds
			if bWizardModeSupremeReady then
				ufolocklight.state =0
				pizzaorder.state = 0
				modelight.state = 0
			End if

			StackState(kStack_Pri0).Disable				'Done with the mode 
			RestoreAllLights
			supreme.state = 1
			bWizardModeSupremeFinished=True				' Mark it as done
			bWizardModeSupremeReady = False 
				StopWizardSupreme
			'PlaySoundVol "leftbones", VolDef
			pizzasize=4
			pizzasizemodes
			pizzaorder.state = 2

		elseif pizzasize = 4 and pz4l16.state <> 2  then '  I Ate the Whole Thing
debug.print "Enable: I Ate the Whole Thing"
			pizzaorder.state = 0
			PlaySoundVol "leftbones", VolDef
			pz4l16.state = 2
			pz4l15.state = 2
			pz4l14.state = 2
			pz4l13.state = 2
			pz4l12.state = 2
			pz4l11.state = 2
			pz4l10.state = 2
			pz4l9.state = 2
			pz4l8.state = 2
			pz4l7.state = 2
			pz4l6.state = 2
			pz4l5.state = 2
			pz4l4.state = 2
			pz4l3.state = 2
			pz4l2.state = 2
			pz4l1.state = 2
			StartWizardAte
		End if
	End If 
End Sub
Sub HandleDebugUp(keycode)
'	exit sub 
	If KeyCode = LeftMagnaSave then bDebugLeftMagnaDown = False
	If KeyCode = RightMagnaSave then bDebugRightMagnaDown = False
End Sub

Sub HandleDebugLeft()
	Dim Ball
	if (bDebugLeftMagnaDown and bDebugRightMagnaDown = False) then 
		if BallsOnPlayfield = 1 then 
			For each Ball in GetBalls
				if Ball.x <> cBall.x and Ball.y <> cBall.y then 
					Ball.VelY = 0
					Ball.VelX = 0
					Ball.VelZ = 0
					Ball.x = 257.3144
					Ball.y = 1604.835
					Ball.z = 25.53022
				End If 
			Next
		End If 
	End If 
End Sub
Sub HandleDebugRight()
	Dim Ball
	if (bDebugLeftMagnaDown = False and bDebugRightMagnaDown) then
		if BallsOnPlayfield = 1 then 
			For each Ball in GetBalls
				if Ball.x <> cBall.x and Ball.y <> cBall.y then 
					Ball.VelY = 0
					Ball.VelX = 0
					Ball.VelZ = 0
					Ball.x = 599.3697
					Ball.y = 1602.655
					Ball.z = 25.53322
				End if 
			Next
		End If 
	End If 
End Sub


'*********
' Queue - This could be used for anything but I use it to queue  priority=1 items up with the option to have 1 Priority=2 item queued or running 
'				Thought here is Pri 1 items need to be shown, Pri 2 items can be shown if an item is running 
'
' 	NOTE - Since VPMtimer is limited to 20 concurrent timers you need a timer called tmrQueue to ensure items dont get dropped 
'	QueueScene
'		Command=vbscript command    ex:   "RunFunction ""Test"", 123  '"
'		Length=milliseconds before running the next item in the queue
'		Priority=Number, 0 being highest
'
'*********
const kQCmd = 0
const kQPri = 1
const kQLen = 2
dim PupQueue(26, 3)			' Size=20,  Fields are 0=Command, 1=Priority, 2=time
dim PupQueueEndPos			' Size of the queue (-1 = Empty)
dim QueueActive				' We are actively running something 
Dim QueueCurrentTime		' How much time is this one going to run (Just used for gtting the queue time)
QueueActive=False
PupQueueEndPos=-1

Sub QueueNoop()
End Sub 

Sub QueueFlush()
	PupQueueEndPos=-1
	QueueActive=False
End Sub
Function getQueueTime()		' Returns how much time left on queue
	Dim time,i 
	time = 0
debug.print "GetQueueTime:" & now 
debug.print "GetQueueTime:" & QueueCurrentTime & " " & QueueActive
	if QueueActive and QueueCurrentTime <> 0 then time = (DateDiff("s", now, QueueCurrentTime) * 1000)
debug.print "GetQueueTime Active:" & time

	for i = 0 to PupQueueEndPos
		time = time + PupQueue(i, kQLen) 
	Next
	getQueueTime = time
debug.print "GetQueueTime ret:" & time
End Function
Sub QueuePop()
	if PupQueueEndPos = -1 then exit sub 
	PupQueue(0, kQPri )=99
	SortPupQueue
	PupQueue(PupQueueEndPos,kQCmd )=""
	PupQueue(PupQueueEndPos,kQLen )=0
	PupQueueEndPos=PupQueueEndPos-1

Debug.print "--Q-Dump Pop---"
	Dim xx
	for xx = 0 to PupQueueEndPos
		debug.print xx & " " & PupQueue(xx, kQCmd) & " " & PupQueue(xx, kQPri) & " " & PupQueue(xx, kQLen) 
	Next 
Debug.print "--Q-Dump Pop---"


End Sub
 
Sub QueueScene(Command, msecLen, priority) 
debug.print "Queue Scene " & Command & " Len: " & msecLen
	if PupQueueEndPos < UBound(PupQueue, kQPri)-1 then 
		PupQueueEndPos=PupQueueEndPos+1
	Else
		debug.print "QUEUE OVERFLOW!!!!!!!!!"
	End if 

	' NOTE: If it is full we overwrite the lowest priority (Optionally we could make the queue bigger)
	' Queue a noop to wait 
	PupQueue(PupQueueEndPos, kQCmd )="QueueNoop() '"
	PupQueue(PupQueueEndPos, kQPri )=priority
	PupQueue(PupQueueEndPos, kQLen )=msecLen
	SortPupQueue

	' Queue the actual command 
	PupQueueEndPos=PupQueueEndPos+1
	PupQueue(PupQueueEndPos, kQCmd )=Command
	PupQueue(PupQueueEndPos, kQPri )=priority
	PupQueue(PupQueueEndPos, kQLen )=0
	SortPupQueue
	
Debug.print "--Q-Dump---"
	Dim xx
	for xx = 0 to PupQueueEndPos
		debug.print xx & " " & PupQueue(xx, kQCmd) & " " & PupQueue(xx, kQPri) & " " & PupQueue(xx, kQLen) 
	Next 
Debug.print "--Q-Dump---"

	RunQueue True
End Sub

Sub tmrQueue_Timer
	tmrQueue.Enabled = False 
	RunQueue False
End Sub 

Sub QueueSkip()						' Shortcycle the timer and move on
Debug.print "--Q Skip--"
	if tmrQueue.Enabled Then
		tmrQueue.Enabled = False
		RunQueue False
	End if
End Sub 

Sub RunQueue(bNewItem)
	dim qCmd, qTime
debug.print "Run Queue " & QueueActive & " " & bNewItem & " " & Now
	if QueueActive = False or bNewItem=False then 	' Nothing is running Or we just finished running something 
		if PupQueueEndPos <> -1 then
			QueueActive = True
			qCmd=PupQueue(0, kQCmd)
			qTime=PupQueue(0, kQLen)
debug.print "Exec " & qCmd
			Execute qCmd
debug.print "Timer " & qTime
			if qTime > 0 then 
				QueueCurrentTime = DateAdd("s",qTime/1000, now)
debug.print QueueCurrentTime
				tmrQueue.Interval = qTime
				tmrQueue.Enabled = True
				'vpmtimer.addtimer cInt(qTime), "RunQueue False '"
				QueuePop
			Else			' No timer just run the next item in the queue 
				QueueCurrentTime = 0
				QueuePop
				RunQueue False
			End If
		Else 
debug.print "Queue Empty Deacivated"
			QueueActive = False
		End If 
	End if
End Sub

Sub SortPupQueue
	dim a, j, temp1, temp2, temp3
	for a = PupQueueEndPos - 1 To 0 Step -1
		for j= 0 to a
			if PupQueue(j, kQPri)>PupQueue(j+1, kQPri) then
				temp1=PupQueue(j+1,kQCmd )
				temp2=PupQueue(j+1,kQPri )
				temp3=PupQueue(j+1,kQLen )
				PupQueue(j+1,kQCmd )=PupQueue(j,kQCmd )
				PupQueue(j+1,kQPri )=PupQueue(j,kQPri )
				PupQueue(j+1,kQLen )=PupQueue(j,kQLen )
				PupQueue(j, kQCmd )=temp1
				PupQueue(j, kQPri )=temp2
				PupQueue(j, kQLen )=temp3
			end if
		next
	next 

End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Starting and Ending Game
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	' Initialise the Table for a new Game
	'
	Sub ResetForNewGame()
		Dim i
		bGameInPLay = True
		'resets the score display, and turn off attrack mode
		StopAttractMode
		if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
			GiOn
		end if
		TotalGamesPlayed = TotalGamesPlayed + 1
		CurrentPlayer = 0
		PlayersPlayingGame = 1
		bOnTheFirstBall = True
		For i = 0 To MaxPlayers-1
			Score(i) = 0
			BonusPoints(i) = 0
			BonusHeldPoints(i) = 0
			BonusMultiplier(i) = 1
	BallsRemaining(i) = bpgcurrent
			ExtraBallsAwards(i) = 0
		Next
		Tilt = 0
		Game_Init()
		vpmtimer.addtimer 1500, "FirstBall '"
	End Sub

	' This is used to delay the start of a game to allow any attract sequence to
	' complete.  When it expires it creates a ball for the player to start playing with
	Sub FirstBall
		' reset the table for a new ball
		ResetForNewPlayerBall()
		' create a new ball in the shooters lane
		If bResetCurrentGame = False then 
debug.print "Reset Current Game"
			CreateNewBall()
		End If 
		bResetCurrentGame = False
	End Sub

	' (Re-)Initialise the Table for a new ball (either a new ball after the player has
	' lost one or we have moved onto the next player (if multiple are playing))
	Sub ResetForNewPlayerBall()
		ufocall=0
		PlayGeneralMusic
debug.print currentplayer		
		dim pnow:pnow=0
		if currentplayer = 4 Then
			pnow = 1
		Else
			pnow = currentplayer+1
		end if
		Select case pnow
			case 1	
			If(PlayersPlayingGame= 1) Then
				playmedia "", "video-player1", pBackglass, "", 3000, "", 1, 1
			else
				playmedia "", "crust-player1", pBackglass, "", 3000, "", 1, 1
			ShowMsg "Player 1 up!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			end if 
			case 2
			ShowMsg "Player 2 up!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			playmedia "", "crust-player2", pBackglass, "", 3000, "", 1, 3
			case 3	
			ShowMsg "Player 3 up!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			playmedia "", "crust-player3", pBackglass, "", 3000, "", 1, 3
			case 4	
			ShowMsg "Player 4 up!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			playmedia "", "crust-player4", pBackglass, "", 3000, "", 1, 3
		end select

		AddScore 0
		BonusPoints(CurrentPlayer) = 0
		bBonusHeld = False
		bExtraBallWonThisBall = False
		ResetNewBallLights()
		ResetNewBallVariables
		bBallSaverReady = True
		'RaiseTargets
		bSkillShotReady = True
	End Sub

	Sub CreateNewBall()
Debug.print "Create New Ball"
		' create a ball in the plunger lane kicker.
		BallRelease.CreateSizedball BallSize / 2

		' There is a (or another) ball on the playfield
		BallsOnPlayfield = BallsOnPlayfield + 1

		' kick it out..
		PlaySound SoundFXDOF("fx_Ballrel", 102, DOFPulse, DOFContactors), 0, 1, 0.1, 0.1
		BallRelease.Kick 90, 4

	' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
	' set the bAutoPlunger flag to kick the ball in play automatically
		If BallsOnPlayfield > 1 Then
			bMultiBallMode = True
				DOF 127, DOFPulse
			bAutoPlunger = True
		End If

	'new song for the mode
		'If barbMultiball = True Then
		'	PlaySong "m_barb"  'this last number is the volume, from 0 to 1
		'End If
	End Sub

	' Add extra balls to the table with autoplunger
	' Use it as AddMultiball 4 to add 4 extra balls to the table

	Sub AddMultiball(nballs)
debug.print "AddMultiball:" & nballs
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballTimer.Enabled = True
	End Sub

	' Eject the ball after the delay, AddMultiballDelay
	Sub CreateMultiballTimer_Timer()
		' wait if there is a ball in the plunger lane
		If bBallInPlungerLane Then
			Exit Sub
		Else
			If BallsOnPlayfield <MaxMultiballs Then
				CreateNewBall()
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
					Me.Enabled = False
				End If
			Else 'the max number of multiballs is reached, so stop the timer
				mBalls2Eject = 0
				Me.Enabled = False
			End If
		End If
	End Sub

	Sub EndOfBall()
		Dim AwardPoints, TotalBonus, ii
		AwardPoints = 0
		TotalBonus = 0
		' the first ball has been lost. From this point on no new players can join in
		bOnTheFirstBall = False

		If NOT Tilted Then
'			ShowMsg "Bonus x " & BonusMultiplier(CurrentPlayer), FormatScore(BonusPoints(CurrentPlayer))
'			AddScore BonusPoints(CurrentPlayer)
'			BonusPoints(CurrentPlayer)=0
'			BonusMultiplier(CurrentPlayer)=1
'			' add a bit of a delay to allow for the bonus points to be shown & added up
'			vpmtimer.addtimer 100, "EndOfBall2 '"
			
			BonusScene

		Else 'if tilted then only add a short delay
			vpmtimer.addtimer 100, "EndOfBall2 '"
		End If
	End Sub

	dim inbonus:inbonus=false
	Sub BonusScene()
		Dim i
		inbonus=true
		SetBGPizza(True)
		tmrBonus.UserValue = 0
		tmrBonus.Interval = 1000
		tmrBonus.Enabled = True
		ScoreBonusAdd=0

		PuPlayer.LabelShowPage pBackglass, 6,0,""
		for i = 0 to 5
			puPlayer.LabelSet pBackglass,"Bonus"&i,			""			,1,"{'mt':2,'size':2.5,'ypos':"& (42+(i*3.5)) &"}"
			puPlayer.LabelSet pBackglass,"BonusScore"&i,	""			,1,"{'mt':2,'size':2.5,'ypos':"& (42+(i*3.5)) &"}"
		Next
		playclear pBackglass
		PuPlayer.playlistplayex pBackglass,"PupBonus","bonus_bg.mp4", 1, 1
		PuPlayer.SetLoop pBackglass, 1

		puPlayer.LabelSet pBackglass,"Bonus6", "Subtotal"							,1,"{'mt':2,'size':2.5,'ypos':"& (42+(6*3.5)) &"}"
		puPlayer.LabelSet pBackglass,"BonusScore6", FormatScore(ScoreBonusAdd)		,1,"{'mt':2,'size':2.5,'ypos':"& (42+(6*3.5)) &"}"
		puPlayer.LabelSet pBackglass,"Bonus7", ""					,1,"{'mt':2,'size':2.5,'ypos':"& (42+(7*3.5)) &"}"
		puPlayer.LabelSet pBackglass,"BonusScore7", ""							,1,"{'mt':2,'size':2.5,'ypos':"& (42+(7*3.5)) &"}"
		puPlayer.LabelSet pBackglass,"BonusTotal",	FormatScore(ScoreBonusAdd)			,1,""

	End Sub 


	Dim ScoreBonusAdd
	Sub tmrBonus_Timer()
		dim Title:Title=""
		Dim ScoreStr
		Dim Score
		Dim i

Debug.print "Bonus Timer:" & tmrBonus.UserValue
		tmrBonus.Interval = 700 'was 1000
		Dim Idx:Idx=tmrBonus.UserValue
		Select case tmrBonus.UserValue
			case 0:  ' Spins
				Title="Spins"
				ScoreStr=BonusTotals(kBonus_Spins) &"x200"
				Score=BonusTotals(kBonus_Spins)*200				
			case 1:  ' Locks
				Title="Space Locks"
				ScoreStr=BonusTotals(kBonus_Locks) &"x10,000"
				Score=BonusTotals(kBonus_Locks)*10000			
			case 2:  ' Ramps Hit
				Title="Ramps Hit"
				ScoreStr=BonusTotals(kBonus_Ramps) &"x3000"
				Score=BonusTotals(kBonus_Ramps)*3000				
			case 3:  ' Slices Eaten
				Title="Slices Eaten"
				ScoreStr=BonusTotals(kBonus_SlicesEaten) &"x1000"
				Score=BonusTotals(kBonus_SlicesEaten)*1000				
			case 4:  ' Loops
				Title="Loop De Loops"
				ScoreStr=BonusTotals(kBonus_LoopDeLoop) &"x300"
				Score=BonusTotals(kBonus_LoopDeLoop)*300				
			case 5:  ' Bumpers
				Title="Bumpers Hit"
				ScoreStr=BonusTotals(kBonus_Bumpers) &"x100"
				Score=BonusTotals(kBonus_Bumpers)*100				
			case 6:  ' Subtotal
			case 7:  ' Bonus Multiplier
				If BonusMultiplier(CurrentPlayer) = 0 Then
					BonusMultiplier(CurrentPlayer)= BonusMultiplier(CurrentPlayer)+1
				end if
				PlaySoundVol "kaching", 100
				puPlayer.LabelSet pBackglass,"Bonus7", "Bonus Multiplier"							,1,"{'mt':2,'size':2.5,'ypos':"& (42+(idx*3.5)) &"}"
				puPlayer.LabelSet pBackglass,"BonusScore7", BonusMultiplier(CurrentPlayer)&"x"		,1,"{'mt':2,'size':2.5,'ypos':"& (42+(idx*3.5)) &"}"
				puPlayer.LabelSet pBackglass,"BonusTotal",	BonusMultiplier(CurrentPlayer)*FormatScore(ScoreBonusAdd)		,1,""
				AddScore FormatScore(ScoreBonusAdd)*(BonusMultiplier(CurrentPlayer)-1)  ' Subtract one since we already added score counting up total
			case 8:  ' Wait
			case 9:  ' End of Ball
				for i = 0 to 5
					BonusTotals(0)=0
				Next 
				playclear pBackglass
				SetBGPizza(False)
				PuPlayer.LabelShowPage pBackglass, 1,0,""
				tmrBonus.Enabled = False 
				vpmtimer.addtimer 100, "EndOfBall2 '"
				exit sub 
		End Select

		if Title <> "" then
			playsfx("hittarget")
			AddScore Score
			ScoreBonusAdd=ScoreBonusAdd+Score
			puPlayer.LabelSet pBackglass,"Bonus"&idx, Title					,1,"{'mt':2,'size':2.5,'ypos':"& (42+(idx*3.5)) &"}"
			puPlayer.LabelSet pBackglass,"BonusScore"&idx, Score			,1,"{'mt':2,'size':2.5,'ypos':"& (42+(idx*3.5)) &"}"

			puPlayer.LabelSet pBackglass,"Bonus6", "Subtotal"							,1,"{'mt':2,'size':2.5,'ypos':"& (42+(6*3.5)) &"}"
			puPlayer.LabelSet pBackglass,"BonusScore6", FormatScore(ScoreBonusAdd)		,1,"{'mt':2,'size':2.5,'ypos':"& (42+(6*3.5)) &"}"

			puPlayer.LabelSet pBackglass,"BonusTotal",	1*FormatScore(ScoreBonusAdd)		,1,""
		End if 
		tmrBonus.UserValue = tmrBonus.UserValue+1

		' Flipper Skip
		if LFPress and RFPress then		' Short Cycle
			tmrBonus.Enabled=False
			tmrBonus.Interval = 100
			tmrBonus.Enabled=True
		End if 

	End Sub 

	
	Sub EndOfBall2()
		inbonus=false
			BonusMultiplier(CurrentPlayer) = 0
		' if were tilted, reset the internal tilted flag (this will also
		' set TiltWarnings back to zero) which is useful if we are changing player LOL
		Tilted = False
		Tilt = 0
		DisableTable False 'enable again bumpers and slingshots
		tmrResetSuperSkillshot_Timer	' Cancel timer

		' has the player won an extra-ball ? (might be multiple outstanding)
		If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
			'debug.print "Extra Ball"

			' yep got to give it to them
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

			' if no more EB's then turn off any shoot again light
			If(ExtraBallsAwards(CurrentPlayer) = 0) Then
				LightShootAgain.State = 0
			Else
				LightShootAgain.State = 1
			End If
			bBallSaverReady = True
			bSkillShotReady = True
			canskillshot = True
			'playmedia "","audio-shootagain",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "","crust-shootagain",pBackglass,"",3000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

			' You may wish to do a bit of a song AND dance at this point
					LightSeqFlasher.UpdateInterval = 150
					LightSeqFlasher.Play SeqRandom, 10, , 2000
					'DMD "black.png", "SHOOT", "AGAIN",  2000

			' Create a new ball in the shooters lane
			CreateNewBall()
		Else ' no extra balls

			BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

			' was that the last ball ?
			If(BallsRemaining(CurrentPlayer) <= 0) Then
				'debug.print "No More Balls, High Score Entry"

				' Submit the currentplayers score to the High Score system
				CheckHighScore()
			' you may wish to play some music at this point

			Else

				' not the last ball (for that player)
				' if multiple players are playing then move onto the next one
				EndOfBallComplete()
			End If
		End If
	End Sub

	' This function is called when the end of bonus display
	' (or high score entry finished) AND it either end the game or
	' move onto the next player (or the next ball of the same player)
	'
	Sub EndOfBallComplete()
		Dim NextPlayer

		debug.print "EndOfBall - Complete"

		' are there multiple players playing this game ?
		If(PlayersPlayingGame> 1) Then
			' then move to the next player
			NextPlayer = CurrentPlayer + 1
			' are we going from the last player back to the first
			' (ie say from player 4 back to player 1)
			If(NextPlayer> PlayersPlayingGame-1) Then
				NextPlayer = 0
			End If
		Else
			NextPlayer = CurrentPlayer
		End If

		'debug.print "Next Player = " & NextPlayer

		' is it the end of the game ? (all balls been lost for all players)
		If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
			EndOfGame()
		Else

			CurrentPlayer = NextPlayer
			UpdateNumberPlayers
			AddScore 0
			currentpizza=0
			PlayerState(CurrentPlayer).Restore	' Restore states after
			ResetForNewPlayerBall()

			CreateNewBall()
			if tips>5 then 
				if gottips then
				else
				tiplock
				end If
			end If
			If PlayersPlayingGame> 1 Then
				'PlaySound "vo_player" &CurrentPlayer+1
				'DMD "black.png", " ", "PLAYER " &CurrentPlayer,  800
			End If

		End If
	End Sub


	Sub EndOfGame()
Debug.print "EndOfGame"
		playmedia "","crust-playagain",pBackglass,"cineon",3000,"StartAttractMode:pDMDStartUP",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "","video-playagain",pBackglass,"cineon",4000,"StartAttractMode",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		ShowMsg "Game Over", "=(" 'FormatScore(BonusPoints(CurrentPlayer))

		' Set the new HS
		puPlayer.LabelSet pBackglass,"CheckGP", TotalGamesPlayed, 1,""
		puPlayer.LabelSet pBackglass,"CheckTS", HighScoreName(0) & " " & HighScore(0), 1,""

		resetalllights
		introposition = 0
		ShowTableInfo
		bGameInPLay = False
		playclear pMusic
		If NOT bJustStarted Then
			PlaySong "m_end"
		End If
		bJustStarted = False
		SolLFlipper 0
		SolRFlipper 0
		SolULFlipper 0
		BallsInLock(CurrentPlayer) = 0
		'RaiseTargets
		'DMD "black.png", "Game Over", "",  2000
		Dim i
		If Score(1) Then
			'DMD "black.png", "PLAYER 1", Score(1), 3000
		End If
		If Score(2) Then
			'DMD "black.png", "PLAYER 2", Score(2), 3000
		End If
		If Score(3) Then
			'DMD "black.png", "PLAYER 3", Score(3), 3000
		End If
		If Score(4) Then
			'DMD "black.png", "PLAYER 4", Score(4), 3000
		End If
		GiOff
		'StartAttractMode
	End Sub

	Function Balls
		Dim tmp
		tmp = bpgcurrent - BallsRemaining(CurrentPlayer) + 1
		If tmp> bpgcurrent Then
			Balls = bpgcurrent
		Else
			Balls = tmp
		End If
	End Function


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Sound FX - General Table Sounds
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	Dim Song
	Song = ""

	Sub PlaySong(name)
		If bMusicOn Then
			If Song <> name Then
				StopSound Song
				Song = name
				If Song = "m_end" Then
					PlaySound Song, 0, 0.1  'this last number is the volume, from 0 to 1
				Else
					PlaySound Song, -1, 0.1 'this last number is the volume, from 0 to 1
				End If
			End If
		End If
	End Sub


	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Different Song Per Ball
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Dim changetrack
	Sub Whatsong
		changetrack = changetrack +1
	End Sub

	Sub CurrentSong
		Select Case changetrack
			Case 1
				PlaySong "m_end"
			Case 2
				PlaySong "m_end"
			Case 3
				PlaySong "m_end"
				changetrack = 0
			End Select
	End Sub


	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Supporting Ball Sound Functions
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
		Vol = Csng(BallVel(ball) ^2 / 2000)
	End Function

	Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
		Dim tmp
		tmp = ball.x * 2 / table1.width-1
		If tmp > 0 Then
			Pan = Csng(tmp ^10)
		Else
			Pan = Csng(-((- tmp) ^10) )
		End If
	End Function

	Function AudioFade(ball) 'only on VPX 10.4 and newer
		Dim tmp
		tmp = ball.y * 2 / Table1.height-1
		If tmp > 0 Then
			AudioFade = Csng(tmp ^10)
		Else
			AudioFade = Csng(-((- tmp) ^10))
		End If
	End Function

	Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
		Pitch = BallVel(ball) * 20
	End Function

	Function BallVel(ball) 'Calculates the ball speed
		BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
	End Function

	Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
		PlaySound soundname, 0, VolSfx, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
	End Sub

	Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
		PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	End Sub

	Sub PlaySoundAtVol(soundname, tableobj, Volume)
	  PlaySound soundname, 1, Volume, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
	End Sub

	Sub PlaySoundVol(soundname, Volume)
	  PlaySound soundname, 1, Volume
	End Sub

	Sub PlaySoundLoopVol(soundname, Volume)
	  PlaySound soundname, -1, Volume
	End Sub


	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'->  Ramp Sounds, Use as needed
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'shooter ramp
	Sub ShooterStart_Hit():StopSound "fx_launchball":If ActiveBall.VelY < 0 Then PlaySound "fx_launchball":End If:End Sub	'ball is going up
	Sub ShooterEnd_Hit:If ActiveBall.Z > 50  Then Me.TimerInterval=100:Me.TimerEnabled=1:End If:End Sub						'ball is flying
	Sub ShooterEnd_Timer(): Me.TimerEnabled=0 : PlaySound "fx_balldrop" : End Sub
	'center ramp
	Sub CREnter_Hit():ultracombo 3:If ActiveBall.VelY < 0 Then PlaySound "fx_lrenter":End If:End Sub			'ball is going up
	Sub CREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_metalrolling":StopSound "fx_lrenter":End If:End Sub		'ball is going down
	Sub CREnter1_Hit():If ActiveBall.VelY < 0 Then PlaySound "fx_lrenter":End If:End Sub			'ball is going up
	Sub CREnter1_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_metalrolling":StopSound "fx_lrenter":End If:End Sub		'ball is going down
	Sub CRExit_Hit:StopSound "fx_lrenter" : Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
	Sub CRExit_Timer(): Me.TimerEnabled=0 : PlaySound "fx_metalrolling" : End Sub


	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Real Time updates using the GameTimer
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Sub GameTimer_Timer
		RollingUpdate
	End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub 'there no extra balls on this table

	'debug.print lob & " " & UBound(BOT)

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
				ballvol = Vol(BOT(b) ) 
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
				ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
			ballvol = ballvol * 8
'debug.print "BallRol:" & "fx_ballrolling" & b  & " " & ballvol
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'->  Sound FX Groupings
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	' Make sure to make collections for each of these to fire the sound fxs

	Sub OnBallBallCollision(ball1, ball2, velocity)
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
	End Sub

	Sub Pins_Hit (idx)
		PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	End Sub

	Sub Targets_Hit (idx)
		PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
	End Sub

	Sub Metals_Thin_Hit (idx)
		PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Sub

	Sub Metals_Medium_Hit (idx)
		PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Sub

	Sub Metals2_Hit (idx)
		PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Sub

	Sub Gates_Hit (idx)
		PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Sub

	Sub Spinner_Spin
		PlaySound "fx_spinner",0,.25,0,0.25
	End Sub

	Sub Rubbers_Hit(idx)
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		If finalspeed > 20 then 
			PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		End if
		If finalspeed >= 6 AND finalspeed <= 20 then
			RandomSoundRubber()
		End If
	End Sub

	Sub Posts_Hit(idx)
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		If finalspeed > 16 then 
			PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		End if
		If finalspeed >= 6 AND finalspeed <= 16 then
			RandomSoundRubber()
		End If
	End Sub

	Sub LeftFlipper_Collide(parm)
		RandomSoundFlipper()
	End Sub

	Sub RightFlipper_Collide(parm)
		RandomSoundFlipper()
	End Sub

	Sub LeftFlipper2_Collide(parm)
		RandomSoundFlipper()
	End Sub


	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Sound Randomizers
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	Sub RandomSoundFlipper()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
			Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
			Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		End Select
	End Sub

	Sub RandomSoundRubber()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
			Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
			Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		End Select
	End Sub

	Sub RandomSoundHole()
		Select Case RndNum(1,3)
			Case 1 : PlaySound "fx_hole1"
			Case 2 : PlaySound "fx_hole2"
			Case 3 : PlaySound "fx_hole3"
		End Select
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Tilt
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  



	Sub CheckTilt                                    'Called when table is nudged
		Tilt = Tilt + TiltSensitivity                'Add to tilt count
		TiltDecreaseTimer.Enabled = True
		If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
			ShowMsg "Tilt Warning!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			'DMD "black.png", "CAREFUL!", "MOUTHBREATHER",  800
			DOF 131, DOFPulse
		End if
		If Tilt> 15 Then 'If more that 15 then TILT the table
			Tilted = True
			ShowMsg "Tilt!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			'display Tilt
			'DMD "black.png", " ", "TILT!",  99999
			DisableTable True
			TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
		End If
	End Sub

	Sub TiltDecreaseTimer_Timer
		If Tilt> 0 Then
			Tilt = Tilt - 0.1
		Else
			TiltDecreaseTimer.Enabled = False
		End If
	End Sub

	Sub DisableTable(Enabled)
		If Enabled Then
			GiOff
			LightSeqTilt.Play SeqAllOff
			LeftFlipper.RotateToStart
			LeftFlipper2.RotateToStart
			RightFlipper.RotateToStart
		Else
			GiOn
			LightSeqTilt.StopPlay
		End If
	End Sub

	Sub TiltRecoveryTimer_Timer()
		If(BallsOnPlayfield = 0) Then
			EndOfBall()
			TiltRecoveryTimer.Enabled = False
		End If
	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Stack Mode lights 
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'
'  Mode stacking allows the light colors to represent what mode you are in and then the state can be set/queried per mode 
'	so the light keeps the status of the upper most mode until it goes off and then it shows the next mode down and so On
'	until no modes have the light set and it actually goes off
'
' kStackSize - Set this to the number of modes that can be run at the same time 
' kTargetSize - Set this to the number of targets in the stack (usually this is all the ramps, orbits and lanes)
'				- Typically these lights are white so the color can be changed based on the mode
'
' StackState_Init - Add this to your table1_init
'
' StackState(kStack_PriX).Enable - Do this when you start the mode that can be stacked 
' StackState(kStack_PriX).Disable - Do this when you end the mode so lower stacks are shown
' StackState(kStack_PriX).TargetState(light) - Set or get the state for a light  
' StackState(kStack_PriX).TargetState(light) - Set or get the state for a light  
'
' SSetLightColor(stackIndex, light, color, state) - Just like SetLightcolor except you have a mode with it.
' SetLightColorRestore(light, lIndex) - Restores the light based on current modes. Use this to force a mode update
'										lIndex should be -1 unless you already know the ModeIndex
'
'*****************************************
'      Structure to save/restore all player data before moving to a new ones
' ***************************************

Const kTargetSize = 46		' How many targets are in the stack
Const kStackSize  = 4		' How Many Modes can be stacked  - Higher priority wins
Const kStack_Pri0 = 0		' Base Modes								 
Const kStack_Pri1 = 1 		' Specialty modes/Multiball Modes
Const kStack_Pri2 = 2 		' Wizard Modes
Const kStack_Pri3 = 3		' Skillshot

Function GetModeIndex(lightTargetName)		' TBD you need to define this function to map your lights to an index
	GetModeIndex = -1
	Select Case lightTargetName
		Case "ecl2": 			' Extra Cheese
			GetModeIndex = 0
        Case "pepperonititle":  ' Pepperoni
			GetModeIndex = 1
        Case "pinetitle":  		' Pineapple
			GetModeIndex = 2
        Case "bacontitle":		' Bacon
			GetModeIndex = 3 
        Case "olivetitle": 	 	' Olives
			GetModeIndex = 4
        Case "mushtitle": 		' Mushroom
			GetModeIndex = 5
        Case "saustitle": 		' Sausage
			GetModeIndex = 6 
        Case "peptitle": 		' Pepper
			GetModeIndex = 7
		case "anchtitle":		' Anchovies
			GetModeIndex = 8
		case "jackpotl1":		' jackpot light
			GetModeIndex = 9
		case "skillarrow":		' skillarrow
			GetModeIndex = 10
		case "ufo4l":			' ufo4
			GetModeIndex = 11
		case "ufo3l":			' ufo3
			GetModeIndex = 12
		case "bumps1":			' mid bump light
			GetModeIndex = 13
		case "roni1":			' roni1
			GetModeIndex = 14
		case "roni2":			' roni2
			GetModeIndex = 15
		case "roni3":			' roni3
			GetModeIndex = 16
		case "bumps2":			' bumps2
			GetModeIndex = 17
		case "leftarrow":		' left arrow
			GetModeIndex = 18
		case "rightarrow":		' right arrow
			GetModeIndex = 19
		case "ropizza1":		' right arrow
			GetModeIndex = 20
		case "ropizza2":		' right arrow
			GetModeIndex = 21
		case "ropizza3":		' right arrow
			GetModeIndex = 22
		case "L13":		
			GetModeIndex = 23
		case "L26":		
			GetModeIndex = 24
		case "L28":		
			GetModeIndex = 25
		case "L27":		
			GetModeIndex = 26
		case "leftarrow1":		'----
			GetModeIndex = 27
		case "pinel1":		
			GetModeIndex = 28
		case "pinel2":		
			GetModeIndex = 29
		case "baconl1":		
			GetModeIndex = 30
		case "baconl2":		
			GetModeIndex = 31
		case "olivel1":		
			GetModeIndex = 32
		case "olivel2":		
			GetModeIndex = 33
		case "mushl1":		
			GetModeIndex = 34
		case "mushl2":		
			GetModeIndex = 35
		case "mushl3":		
			GetModeIndex = 36
		case "sausl1":		
			GetModeIndex = 37
		case "sausl2":		
			GetModeIndex = 38
		case "sausl3":		
			GetModeIndex = 39
		case "pepl1":		
			GetModeIndex = 40
		case "pepl2":		
			GetModeIndex = 41
		case "pepl3":		
			GetModeIndex = 42
		case "fish1":		
			GetModeIndex = 43
		case "fish2":		
			GetModeIndex = 44
		case "fish3":		
			GetModeIndex = 45

	End Select
end Function

Sub RestoreAllLights()
							
	SetLightColor roni1, "red", 0
	SetLightColor roni2, "red", 0
	SetLightColor roni3, "red", 0

	dim bulb
	For Each bulb in aTargetTitles
		SetLightColorRestore bulb, -1
	Next 
End Sub 


Class cStackLightState
	Public TargetColor
	Public TargetState
End Class
Class cStack
	Public bStackActive					' Is there a mode here actually stacked?
	Public sLightStates()				' What are the arrow states for this mode
	'This event is called when an instance of the class is instantiated
	Private Sub Class_Initialize(  )
		dim i
		ReDim sLightStates(kTargetSize)
		For i = 0 to kTargetSize-1
			set sLightStates(i) = New cStackLightState
		Next
	End Sub
	Public Sub Reset()
		dim i
		bStackActive = False
		For i = 0 to kTargetSize-1
			sLightStates(i).TargetColor = ""
			sLightStates(i).TargetState = 0
		Next 
	End Sub

	Public Property Get TargetState(light)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		TargetState = sLightStates(lIndex).TargetState
	End Property 
	Public Property Set TargetState(light, state)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		sLightStates(lIndex).TargetState = state
	End Property 

	Public Sub Enable () 
		bStackActive = True
	End Sub 

	Public Sub Disable() 
		Reset
	End Sub 
End Class
Public StackState()			' Stores which modes are actually in play (stacked)
Redim StackState(kStackSize)


Sub SetLightColorRestore(light, lIndex)
	Dim finalColor
	Dim finalState 
	Dim i
	if lIndex = -1 then 					' Got get it if they didnt already have it 
		lIndex = GetModeIndex(light.name)
	End if 

	finalState = 0
	finalColor = white					' Should never need this line but just in case 
	for i = 0 to kStackSize-1				' Set the real color and state based on the stack  (higher in the stack is higher prioroty)
		if StackState(i).bStackActive then 
			if StackState(i).sLightStates(lIndex).TargetState <> 0 then 
				finalState = StackState(i).sLightStates(lIndex).TargetState
				finalColor = StackState(i).sLightStates(lIndex).TargetColor
'debug.print "Stack: " & i & " lindex:" & lIndex & " state:" & finalState & " color:" & finalColor
			End If 
		End If  
	Next
'debug.print "SetLightColorRestore " & light.name & " state:" & finalState & " color:" & finalColor

	SetLightColor light, finalColor, finalState
End Sub 

Sub SSetLightColor(stackIndex, light, color, state)		' StackSetLightColor		
	dim lIndex
	lIndex = GetModeIndex(light.name)
'debug.print "SSetLightColor " & stackIndex & " idx:" & lIndex & " " & light.name & " state:" & state
	if lIndex <> -1 then  ' We should never hit this  
		StackState(stackIndex).sLightStates(lIndex).TargetColor = color
		StackState(stackIndex).sLightStates(lIndex).TargetState = state
		SetLightColorRestore light, lIndex
	else 
		MsgBox "Light Color Error:" & light.name & " idx:" & stackIndex & " " & lIndex
	End If 
End Sub

Sub StackState_Init()
	dim i 
	For i = 0 to kStackSize
		Set StackState(i) = New cStack
		StackState(i).Reset
	Next 
End Sub 





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Instant Info Setup
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Sub EndFlipperStatus(keycode)
		If bInstantInfo then 
			if keycode = InstantInfoTimer.UserValue then 	' If they let go of the key that started instant Info stop
				'tmrCheckHistory.Enabled=False 
				bInstantInfo=False
				ShowMsgOffset 0
			else 											' Otherwise cycle through Check
				PlaySoundVol "so_timer", VolDef
				ShowMsgOffset curOffset+1
			End if 
		elseif bGameInPlay=False Then
debug.print "EndInstantInfo check" & keycode & " " & InstantInfoTimer.UserValue
			if (keycode=InstantInfoTimer.UserValue) then 	' They let go of the key
debug.print "EndInstantInfo"
				InstantInfoTimer.Enabled = False
				bInstantInfo=False
				PuPlayer.LabelShowPage pBackglass,1,0,""
				pInAttract=false
				playclear pBackglass
				PuPlayer.LabelSet pBackglass,"OverMessage1","",1,""
				PuPlayer.LabelSet pBackglass,"OverMessage2","",1,""
				PuPlayer.LabelSet pBackglass,"OverMessage3","",1,""
'				RefreshPlayerMode
'				if bPlayerModeSelect Then
'					pDMDEvent(kDMD_PlayerSelect)
'				End If 
			Else ' They pressed the other flipper so cycle faster 
				PriorityReset=2000
				pAttractNext
			End If 
		Else
'debug.print "Stop Instant " & keycode
			InstantInfoTimer.Enabled = False
		End If
	End Sub

	Sub InstantInfoTimer_Timer
		PlaySoundVol "so_timer", VolDef
		InstantInfoTimer.Enabled = False
		bInstantInfo = True
	debug.print "Instant Info timer"
'		PuPlayer.LabelShowPage pOverVid, 1,0,""
'		PuPlayer.playlistplayex pOverVid,"DMDBackground","attract.mp4", 1, 1
'		PuPlayer.SetLoop pOverVid, 1
'		PuPlayer.SetBackground pOverVid, 1

	'	PuPlayer.LabelShowPage pOverVid, 1,0,""
		'PuPlayer.LabelShowPage pBackglass, 2,0,""
	'	if bPlayerModeSelect Then 
	'		pDMDEvent(kDMD_Attract)
	'	End If

		'tmrCheckHistory.Interval = 500
		'tmrCheckHistory.Enabled=True

		'pCurAttractPos=0
		'pInAttract=true
		'pAttractNext
	End Sub

'	Sub tmrCheckHistory_Timer()
'		ShowMsgOffset curOffset+1
'	End Sub 

	Sub InstantInfo
		If DisableInstantInfo = False Then
		'DMD "black.png", "", "INSTANT INFO",  500
		End If
	End Sub

	sub StartInstantInfo(keycode)
	debug.print "Start Instant " & keycode & " " & bInstantInfo
		if bInstantInfo = False Then ' I am already in instantinfo
			InstantInfoTimer.Enabled = False 
			InstantInfoTimer.Interval = 4000
			InstantInfoTimer.Enabled = True
			InstantInfoTimer.UserValue=keycode
		End If 
	End Sub 




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Score Entering & Recalling
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Dim bHSAttract:bHSAttract=False
	Dim hsOffset:hsOffset=0
	Dim whichHS:whichHS=0			' 0=Daily. 1=Weekly, 2=All Time
	Sub ScrollHS
		if bHSAttract Then
		puPlayer.LabelSet pBackglass,"hsRT", "#" ,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':42 ,'ypos':42}"
		puPlayer.LabelSet pBackglass,"hsST", "Score" ,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':54.5,'ypos':42}"
		puPlayer.LabelSet pBackglass,"hsNT", "INI" ,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':60 ,'ypos':42}"
		End if
		dim i
		dim ypos
		dim vis
		Dim score
		Dim Name
		if bHSAttract and hsOffset<=60.8 then
'debug.print hsOffset 
			hsOffset=hsOffset+(1/5)
			UpdateAttractHS
		elseif bHSAttract and hsOffset>120 then
			hsOffset=hsOffset+1
			if hsOffset > 600 then hsOffset=0
		Elseif bHSAttract and hsOffset>60.8 then 
			hsOffset=hsOffset+1
			hsOffset=0
			whichHS = (whichHS+1) mod 3
			select case whichHS
				case 0:puPlayer.LabelSet pBackglass,"hsTitle",	"Osb Daily"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':50  ,'ypos':36}"
				case 1:puPlayer.LabelSet pBackglass,"hsTitle",	"Osb Weekly"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':50  ,'ypos':36}"
				case 2:puPlayer.LabelSet pBackglass,"hsTitle",	"Osb AllTime"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':50  ,'ypos':36}"
			End Select
			UpdateAttractHS
			hsOffset=121
		End if 
	End Sub


	Sub clearhslabels
		dim i
			puPlayer.LabelSet pBackglass,"hsTitle",	""			,0,""
			puPlayer.LabelSet pBackglass,"hsRT",	""				,0,""
			puPlayer.LabelSet pBackglass,"hsST",	""			,0,""
			puPlayer.LabelSet pBackglass,"hsNT",	""			,0,""
		for i = 0 to 19
			puPlayer.LabelSet pBackglass,"hsR"&i,	""	,0,""
			puPlayer.LabelSet pBackglass,"hsS"&i,	""	    ,0,""
			puPlayer.LabelSet pBackglass,"hsN"&i,	""		,0,""
		next
	end Sub


	Sub UpdateAttractHS()
		dim i
		dim ypos
		dim vis
		Dim score
		Dim Name
		
		for i = 0 to 19
			ypos=47+(i*4)-hsOffset
			vis=1
			if ypos <= 48 then vis=0
			if ypos >= 67 then vis=0
			select case whichHS
				case 0:score=dailyvar(1,i):Name=dailyvar(0,i)
				case 1:score=weeklyvar(1,i):Name=weeklyvar(0,i)
				case 2:score=alltimevar(1,i):Name=alltimevar(0,i)
			End Select 
			if i=0 then ypos=47:vis=1
			puPlayer.LabelSet pBackglass,"hsR"&i,	"#" &i+1	,vis,"{'mt':2,'size':2.2,'xpos':42  ,'ypos':"& ypos &"}"
			puPlayer.LabelSet pBackglass,"hsS"&i,	score	    ,vis,"{'mt':2,'size':2.2,'xpos':54.5  ,'ypos':"& ypos &"}"
			puPlayer.LabelSet pBackglass,"hsN"&i,	name		,vis,"{'mt':2,'size':2.2,'xpos':60  ,'ypos':"& ypos &"}"
		Next 
	End Sub 

	'high score clearing
	If resetScores = True Then
		If areYouSure = "i am serious" Then
			SaveValue TableName, "HighScore1", 0
			SaveValue TableName, "HighScore1Name", "SBW"
			SaveValue TableName, "HighScore2", 0
			SaveValue TableName, "HighScore2Name", "SBW"
			SaveValue TableName, "HighScore3", 0
			SaveValue TableName, "HighScore3Name", "SBW"
			SaveValue TableName, "HighScore4", 0
			SaveValue TableName, "HighScore4Name", "SBW"
		end If
	end if




	Sub Loadhs
		Dim x
		GetScores

		x = LoadValue(TableName, "HighScore1")
		If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 1300000 End If

		x = LoadValue(TableName, "HighScore1Name")
		If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "piz" End If

		x = LoadValue(TableName, "HighScore2")
		If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1200000 End If

		x = LoadValue(TableName, "HighScore2Name")
		If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "za" End If

		x = LoadValue(TableName, "HighScore3")
		If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1100000 End If

		x = LoadValue(TableName, "HighScore3Name")
		If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "ti" End If

		x = LoadValue(TableName, "HighScore4")
		If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 1000000 End If

		x = LoadValue(TableName, "HighScore4Name")
		If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "me" End If


		x = LoadValue(TableName, "HighScore5")
		If(x <> "") then HighScore(4) = CDbl(x) Else HighScore(4) = 900000 End If

		x = LoadValue(TableName, "HighScore5Name")
		If(x <> "") then HighScoreName(4) = x Else HighScoreName(4) = "OOH" End If

		x = LoadValue(TableName, "Credits")
		If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

		'x = LoadValue(TableName, "Jackpot")
		'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
		x = LoadValue(TableName, "TotalGamesPlayed")
		If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
	End Sub

	Sub Savehs
		SaveValue TableName, "HighScore1", HighScore(0)
		SaveValue TableName, "HighScore1Name", HighScoreName(0)
		SaveValue TableName, "HighScore2", HighScore(1)
		SaveValue TableName, "HighScore2Name", HighScoreName(1)
		SaveValue TableName, "HighScore3", HighScore(2)
		SaveValue TableName, "HighScore3Name", HighScoreName(2)
		SaveValue TableName, "HighScore4", HighScore(3)
		SaveValue TableName, "HighScore4Name", HighScoreName(3)
		SaveValue TableName, "HighScore5", HighScore(4)
		SaveValue TableName, "HighScore5Name", HighScoreName(4)
		SaveValue TableName, "Credits", Credits
		SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
	End Sub


	Dim hsbModeActive
	Dim hsEnteredName
	Dim hsEnteredDigits(3)
	Dim hsCurrentDigit
	Dim hsValidLetters
	Dim hsCurrentLetter
	Dim hsLetterFlash

	Sub CheckHighscore()
		osbtempscore = Score(CurrentPlayer)

		Dim tmp
		tmp = Score(0)
		If Score(1)> tmp Then tmp = Score(1)
		If Score(2)> tmp Then tmp = Score(2)
		If Score(3)> tmp Then tmp = Score(3)

		If tmp> HighScore(0) Then 'add 1 credit for beating the highscore
			AwardSpecial
		End If
debug.print "Temp:" & tmp & " " & HighScore(3) 
debug.print "Scores: " & Score(0) & Score(1) & Score(2) & Score(3)

		If tmp> HighScore(4) Then
Debug.print "High Score"
			'vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
			HighScore(4) = tmp
			'enter player's name
			ShowMsg "You got a high score!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			'playmedia "","audio-highscore",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "","crust-highscore",pBackglass,"cineon",3000,"HighScoreEntryInit()",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			
		Else
Debug.print "Done"
			if osbactive <> 0 then 
				' Submit to Orbital
				osbtemp = osbdefinit
				SubmitOSBScore
			End if 

			EndOfBallComplete()
		End If
	End Sub

Sub HighScoreEntryPup()
Dim i, ypos, tmp_score, tmp_name, tmp_color

	puPlayer.LabelSet pBackglass,"EnterHS1","Player " & CurrentPlayer+1 ,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 0, 0)&",'xpos':50  ,'ypos':36}"
	puPlayer.LabelSet pBackglass,"EnterHS2","Enter Initials"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 0, 0)&",'xpos':50  ,'ypos':39}"
	puPlayer.LabelSet pBackglass,"EnterHST", "  Score   INI"			,1,"{'mt':2,'size':2.2,'color':"&RGB(0, 255, 255)&",'xpos':50  ,'ypos':42}"
	puPlayer.LabelSet pBackglass,"EnterScr", Score(CurrentPlayer)	,1,"{'mt':2,'size':2.2,'color':"&RGB(0, 255, 255)&",'xpos':50,  'ypos':45}"
	puPlayer.LabelSet pBackglass,"EnterIni", ">   <"				,1,"{'mt':2,'size':2.2,'color':"&RGB(0, 255, 255)&",'xpos':60  ,'ypos':45}"
	puPlayer.LabelSet pBackglass,"EnterHST2","Top 5"				,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 0, 0)&",'xpos':50  ,'ypos':49}"
	puPlayer.LabelSet pBackglass,"EnterRT",	"#"						,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':42  ,'ypos':52}"
	puPlayer.LabelSet pBackglass,"EnterST",	"Score"					,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':54.5,'ypos':52}"
	puPlayer.LabelSet pBackglass,"EnterNT",	"INI"					,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':60  ,'ypos':52}"

	for i = 0 to 4
		ypos=55+(i*3)
		tmp_color=RGB(0, 255, 255)
		if osbtempscore=HighScore(i) then tmp_color=RGB(255, 255, 0)		' Make current yellow
		tmp_score=HighScore(i):tmp_name=HighScoreName(i)
		puPlayer.LabelSet pBackglass,"EnterR"&i,	"#" &i+1	,1,"{'mt':2,'size':2.2,'color':"&tmp_color&",'xpos':42  ,'ypos':"& ypos &"}"
		puPlayer.LabelSet pBackglass,"EnterS"&i,	tmp_score   ,1,"{'mt':2,'size':2.2,'color':"&tmp_color&",'xpos':54.5  ,'ypos':"& ypos &"}"
		puPlayer.LabelSet pBackglass,"EnterN"&i,	tmp_name	,1,"{'mt':2,'size':2.2,'color':"&tmp_color&",'xpos':60  ,'ypos':"& ypos &"}"
	Next
End Sub 

Sub HighScoreEntryInit()
	

	' Show Yellow Page
	if bUsePUPDMD then 
		PuPlayer.LabelShowPage pBackglass, 3,0,""
	'	playclear pBackglass
	'	playmedia "attract.mp4", "DMDBackground", pBackglass, "", -1, "", 1, 1
	'	PuPlayer.playlistplayex pBackglass,"PupOverlays","clear.png", 1, 1
'		PuPlayer.SetLoop pOverVid, 1
'		PuPlayer.SetBackground pOverVid, 1
		HighScoreEntryPup()

	End If 
    hsbModeActive = True
   ' PlaySoundVol "vo-EnterYourInitials", VolDef
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789"    ' < is used to delete the last letter
    hsCurrentLetter = 1
	'DMDId "hsc", "Enter", "Your Name", 999999
    HighScoreDisplayName()

'    HighScoreFlashTimer.Interval = 250
'    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        PlaySoundVol "fx_Previous", VolDef
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        PlaySoundVol "fx_Next", VolDef
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            PlaySoundVol "fx_Enter", VolDef
			
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
			PlaySoundVol "fx_Esc", VolDef
			
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
Dim i, TempStr
Dim bugFIX

    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
	bugFIX=""
	'DMDMod "hsc", "YOUR NAME" & bugFIX, Mid(TempStr, 2, 5), 999999
	if bUsePUPDMD then puPlayer.LabelSet pBackglass,"EnterIni", TempStr,1,""
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(4) = hsEnteredName
    SortHighscore
	HighScoreEntryPup()

	' Submit to Orbital
	osbtemp = hsEnteredName
	SubmitOSBScore

	if bUsePUPDMD then 
		puPlayer.LabelSet pBackglass,"EnterHS1", "",1,""
		puPlayer.LabelSet pBackglass,"EnterHS2", "",1,""
		puPlayer.LabelSet pBackglass,"EnterHST", "",1,""
		puPlayer.LabelSet pBackglass,"EnterScr", "",1,""
		puPlayer.LabelSet pBackglass,"EnterIni", "",1,""
	End If 
	pBGGamePlay
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 4
        For j = 0 to 3
            If HighScore(j) < HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Lighting
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Global Illumination
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Dim OldGiState
	OldGiState = -1   'start witht the Gi off



	Sub GIUpdateTimer_Timer
		Dim tmp, obj
		tmp = Getballs
		If UBound(tmp) <> OldGiState Then
			OldGiState = Ubound(tmp)
			If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
				'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
			Else
				'Gion
			End If
		End If
	End Sub

	Sub GiOn
		prgion.visible=True
		prgioff.visible=false
		backwallon.visible=True
		backwalloff.visible=false
				psgion.visible=true
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
		DOF 126, DOFOn
		Dim bulb
		Spot2.visible=true
		For each bulb in aGiLights
			bulb.State = 1
		Next

	End Sub

	Sub GiLowerOn
		Dim bulb
		For each bulb in lowergi
			bulb.State = 1
		Next

	End Sub

	Sub GiLowerOff
		Dim bulb
		For each bulb in lowergi
			bulb.State = 0
		Next

	End Sub

	Sub GiOff
		prgion.visible=False
		prgioff.visible=True
		backwallon.visible=False
		backwalloff.visible=True
				psgion.visible=false
				psgioff.visible=True
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
		Spot2.visible=false
		Dim bulb
		For each bulb in aGiLights
			bulb.State = 0
		Next

	End Sub

	' GI & light sequence effects

	Sub GiEffect(n)
		Select Case n
			Case 0 'all off
				LightSeqGi.Play SeqAlloff
			Case 1 'all blink
				LightSeqGi.UpdateInterval = 4
				LightSeqGi.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqGi.UpdateInterval = 10
				LightSeqGi.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqGi.UpdateInterval = 4
				LightSeqGi.Play SeqUpOn, 5, 1
			Case 4 ' left-right-left
				LightSeqGi.UpdateInterval = 5
				LightSeqGi.Play SeqLeftOn, 10, 1
				LightSeqGi.UpdateInterval = 5
				LightSeqGi.Play SeqRightOn, 10, 1
		End Select
	End Sub

	Sub LightEffect(n)
		Select Case n
			Case 0 ' all off
				LightSeqInserts.Play SeqAlloff
			Case 1 'all blink
				LightSeqInserts.UpdateInterval = 4
				LightSeqInserts.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqInserts.UpdateInterval = 10
				LightSeqInserts.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqInserts.UpdateInterval = 4
				LightSeqInserts.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				LightSeqInserts.UpdateInterval = 5
				LightSeqInserts.Play SeqLeftOn, 10, 1
				LightSeqInserts.UpdateInterval = 5
				LightSeqInserts.Play SeqRightOn, 10, 1
			Case 5 'random
				LightSeqbumper.UpdateInterval = 4
				LightSeqbumper.Play SeqBlinking, , 5, 10
			Case 6 'random
				LightSeqRSling.UpdateInterval = 4
				LightSeqRSling.Play SeqBlinking, , 5, 6
			Case 7 'random
				LightSeqLSling.UpdateInterval = 4
				LightSeqLSling.Play SeqBlinking, , 5, 6
			Case 8 'random
				LightSeqBack.UpdateInterval = 4
				LightSeqBack.Play SeqBlinking, , 5, 6
		End Select
	End Sub

	Dim FEStep, FEffect
	FEStep = 0
	FEffect = 0

	Sub FlashEffect(n)
		Select case n
			Case 0 ' all off
				LightSeqFlasher.Play SeqAlloff
			Case 1 'all blink
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqFlasher.UpdateInterval = 10
				LightSeqFlasher.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqLeftOn, 10, 1
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqRightOn, 10, 1
		End Select
	End Sub


	'lrflashtime.enabled = 0
	Dim letsflash
	Sub lrflashnow
		'lrflashtime.enabled = 1
	End Sub

	Sub lrflashtime_Timer 
		letsflash = letsflash + 1
		Select Case letsflash
		Case 0
			'FlasherLeftRed.opacity = 0
		Case 1
			'FlasherLeftRed.opacity = 100
		Case 2
			'FlasherLeftRed.opacity = 0
		Case 3
			'FlasherLeftRed.opacity = 100
		Case 4
			'FlasherLeftRed.opacity = 0
		Case 5
			'FlasherLeftRed.opacity = 100
		Case 6
			'FlasherLeftRed.opacity = 0
			'lrflashtime.Enabled = False
			letsflash = 0
		End Select
	End Sub

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
	'-> When TotalPeriod done, light or flasher will be set to FinalState value where
	'-> Final State values are:   0=Off, 1=On, 2=Return to previous State
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->


	Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)

		If TypeName(MyLight) = "Light" Then

			If FinalState = 2 Then
				FinalState = MyLight.State 'Keep the current light state
			End If
			MyLight.BlinkInterval = BlinkPeriod
			MyLight.Duration 2, TotalPeriod, FinalState
		ElseIf TypeName(MyLight) = "Flasher" Then

			Dim steps

			' Store all blink information
			steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
			If FinalState = 2 Then                      'Keep the current flasher state
				FinalState = ABS(MyLight.Visible)
			End If
			MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

			' Start blink timer and create timer subroutine
			MyLight.TimerInterval = BlinkPeriod
			MyLight.TimerEnabled = 0
			MyLight.TimerEnabled = 1
			ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
		End If
	End Sub


	'******************************************
	' Change light color - simulate color leds
	' changes the light color and state
	' 10 colors: red, orange, amber, yellow...
	'******************************************
	' in this table this colors are use to keep track of the progress during the acts and battles

	'colors
	Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base

	red = 10
	orange = 9
	amber = 8
	yellow = 7
	darkgreen = 6
	green = 5
	blue = 4
	darkblue = 3
	purple = 2
	white = 1
	base = 11


	Sub gicolor(n)
		dim a
		backwallon.visible=False
		backwalloff.visible=True
		Spot2.visible=false
		Select Case n
			Case red
				psgion.visible=false
				psgioff.visible=false
				pred.visible=true
				For each a in aGiLights
					a.color = RGB(18, 0, 0)
					a.colorfull = RGB(255, 0, 0)
					a.State = 1
				next
			Case orange
				psgion.visible=false
				psgioff.visible=false
				pred.visible=False
				porange.visible=true
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
				For each a in aGiLights
					a.color = RGB(18, 3, 0)
					a.colorfull = RGB(255, 64, 0)
					a.State = 1
				next
			Case amber
				psgion.visible=True
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(193, 49, 0)
				a.colorfull = RGB(255, 153, 0)
					a.State = 1
				next
			Case yellow
				psgion.visible=True
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(18, 18, 0)
				a.colorfull = RGB(255, 255, 0)
					a.State = 1
				next
			Case darkgreen
				psgion.visible=false
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=True
				pblue.visible=false
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(0, 8, 0)
				a.colorfull = RGB(0, 64, 0)
					a.State = 1
				next
			Case green
				psgion.visible=false
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=True
				pblue.visible=false
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(0, 18, 0)
				a.colorfull = RGB(0, 255, 0)
					a.State = 1
				next
			Case blue
				psgion.visible=false
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=True
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(0, 18, 18)
				a.colorfull = RGB(0, 255, 255)
					a.State = 1
				next
			Case darkblue
				psgion.visible=false
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=True
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(0, 8, 8)
				a.colorfull = RGB(0, 64, 64)
					a.State = 1
				next
			Case purple
				psgion.visible=false
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=True
				For each a in aGiLights
				a.color = RGB(128, 0, 128)
				a.colorfull = RGB(255, 0, 255)
					a.State = 1
				next
			Case white
				psgion.visible=True
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
		backwallon.visible=True
		backwalloff.visible=False
		Spot2.visible=true
				For each a in aGiLights
				a.color = RGB(253, 181, 47)
				a.colorfull = RGB(255, 255, 255)
					a.State = 1
				next
			Case base
		backwallon.visible=True
		backwalloff.visible=False
		Spot2.visible=true
				psgion.visible=true
				psgioff.visible=false
				pred.visible=False
				porange.visible=False
				pgreen.visible=False
				pblue.visible=false
				ppurple.visible=False
				For each a in aGiLights
				a.color = RGB(253, 181, 47)
				a.colorfull = RGB(255, 255, 255)
					a.State = 1
				next
		End Select
	End Sub

	Sub SetLightColor(n, col, stat)
		Select Case col
			Case red
				n.color = RGB(18, 0, 0)
				n.colorfull = RGB(255, 0, 0)
			Case orange
				n.color = RGB(18, 3, 0)
				n.colorfull = RGB(255, 64, 0)
			Case amber
				n.color = RGB(193, 49, 0)
				n.colorfull = RGB(255, 153, 0)
			Case yellow
				n.color = RGB(18, 18, 0)
				n.colorfull = RGB(255, 255, 0)
			Case darkgreen
				n.color = RGB(0, 8, 0)
				n.colorfull = RGB(0, 64, 0)
			Case green
				n.color = RGB(0, 18, 0)
				n.colorfull = RGB(0, 255, 0)
			Case blue
				n.color = RGB(0, 18, 18)
				n.colorfull = RGB(0, 255, 255)
			Case darkblue
				n.color = RGB(0, 8, 8)
				n.colorfull = RGB(0, 64, 64)
			Case purple
				n.color = RGB(128, 0, 128)
				n.colorfull = RGB(255, 0, 255)
			Case white
				n.color = RGB(255, 255, 0)
				n.colorfull = RGB(255, 255, 255)
			Case base
				n.color = RGB(255, 197, 143)
				n.colorfull = RGB(255, 255, 236)
		End Select
		If stat <> -1 Then
			n.State = 0
			n.State = stat
		End If
	End Sub

	Sub ResetAllLightsColor ' Called at a new game
				dim a
				For each a in aLights
				a.intensity = 7
				a.color = RGB(255, 255, 0)
				a.colorfull = RGB(255, 255, 255)
				next
	End Sub

	Sub UpdateBonusColors
	End Sub

	'*************************
	' Rainbow Changing Lights
	'*************************

	Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

	Sub StartRainbow(n)
		set RainbowLights = n
		RGBStep = 0
		RGBFactor = 5
		rRed = 255
		rGreen = 0
		rBlue = 0
		RainbowTimer.Enabled = 1
	End Sub

	Dim RGBStep2, RGBFactor2, rRed2, rGreen2, rBlue2, RainbowLights2
	Sub StartRainbow2(n)
		set RainbowLights2 = n
		RGBStep2 = 0
		RGBFactor2 = 5
		rRed2 = 255
		rGreen2 = 0
		rBlue2 = 0
		RainbowTimer1.Enabled = 1
	End Sub

	Sub StopRainbow(n)
		Dim obj
		RainbowTimer.Enabled = 0
		RainbowTimer.Enabled = 0
			For each obj in RainbowLights
				SetLightColor obj, "white", 0
			Next
	End Sub

	Sub StopRainbow2(n)
		Dim obj
		RainbowTimer1.Enabled = 0
			For each obj in RainbowLights2
				SetLightColor obj, "white", 0
				obj.state = 1
				obj.Intensity = 7
			Next
	End Sub

	Sub RainbowTimer_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep
			Case 0 'Green
				rGreen = rGreen + RGBFactor
				If rGreen > 255 then
					rGreen = 255
					RGBStep = 1
				End If
			Case 1 'Red
				rRed = rRed - RGBFactor
				If rRed < 0 then
					rRed = 0
					RGBStep = 2
				End If
			Case 2 'Blue
				rBlue = rBlue + RGBFactor
				If rBlue > 255 then
					rBlue = 255
					RGBStep = 3
				End If
			Case 3 'Green
				rGreen = rGreen - RGBFactor
				If rGreen < 0 then
					rGreen = 0
					RGBStep = 4
				End If
			Case 4 'Red
				rRed = rRed + RGBFactor
				If rRed > 255 then
					rRed = 255
					RGBStep = 5
				End If
			Case 5 'Blue
				rBlue = rBlue - RGBFactor
				If rBlue < 0 then
					rBlue = 0
					RGBStep = 0
				End If
		End Select
			For each obj in RainbowLights
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
	End Sub

	Sub RainbowTimer1_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep2
			Case 0 'Green
				rGreen2 = rGreen2 + RGBFactor2
				If rGreen2 > 255 then
					rGreen2 = 255
					RGBStep2 = 1
				End If
			Case 1 'Red
				rRed2 = rRed2 - RGBFactor2
				If rRed2 < 0 then
					rRed2 = 0
					RGBStep2 = 2
				End If
			Case 2 'Blue
				rBlue2 = rBlue2 + RGBFactor2
				If rBlue2 > 255 then
					rBlue2 = 255
					RGBStep2 = 3
				End If
			Case 3 'Green
				rGreen2 = rGreen2 - RGBFactor2
				If rGreen2 < 0 then
					rGreen2 = 0
					RGBStep2 = 4
				End If
			Case 4 'Red
				rRed2 = rRed2 + RGBFactor2
				If rRed2 > 255 then
					rRed2 = 255
					RGBStep2 = 5
				End If
			Case 5 'Blue
				rBlue2 = rBlue2 - RGBFactor2
				If rBlue2 < 0 then
					rBlue2 = 0
					RGBStep2 = 0
				End If
		End Select
		'	For each obj in RainbowLights2
		'		obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
		'		obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
		'	Next
	End Sub


'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Light Runs
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\

	dim runninglights: runninglights=0
	dim arcbld:arcbld=1
	dim arcblu:arcblu=2
	dim arcbrd:arcbrd=3
	dim arcbru:arcbru=4
	dim arctld:arctld=5
	dim arctlu:arctlu=6
	dim arctrd:arctrd=7
	dim arctru:arctru=8
	dim circlein:circlein=9
	dim circleout:circleout=10
	dim clockleft:clockleft=11
	dim clockright:clockright=12
	dim diagdl:diagdl=13
	dim diagdr:diagdr=14
	dim diagul:diagul=15
	dim diagur:diagur=16
	dim down:down=17
	dim fanld:fanld=18
	dim fanlu:fanlu=19
	dim fanrd:fanrd=20
	dim fanru:fanru=21
	dim hatch1h:hatch1h=22
	dim hatch1v:hatch1v=23
	dim hatch2h:hatch2h=24
	dim hatch2v:hatch2v=25
	'dim left:left=26
	dim middleih:middleih=27
	dim middleiv:middleiv=28
	dim middleoh:middleoh=29
	dim middleov:middleov=30
	dim radarl:radarl=31
	dim radarr:radarr=32
	dim randoms:randoms=33
	'dim right:right=34
	dim screwl:screwl=35
	dim screwr:screwr=36
	dim stripe1h:stripe1h=37
	dim stripe1v:stripe1v=38
	dim stripe2h:stripe2h=39
	dim stripe2v:stripe2v=40
	dim up:up=41
	dim wiperl:wiperl=42
	dim wiperr:wiperr=43

	'***** LIGHT RUNS *****
	'(CHAOS)randoms
	'(DIRECTIONS)up|down|||diagdl|diagdr|diagul|diagur
	'(SWIPES)middleih|middleiv|middleoh|middleov|stripe1h|hatch1h|hatch1v|hatch2h|hatch2v|stripe1v|stripe2h|stripe2v
	'(SPINS)circlein|circleout|clockleft|clockright|screwl|screwr
	'(CURVES)arcbld|arcblu|arcbrd|arcbru|arctld|arctlu|arctrd|arctru|fanld|fanlu|fanrd|fanru|radarl|radarr|wiperl|wiperr
	'(EXAMPLE)lightrun red,circleout,1  (SYNTAX)lightrun color,direction,times to run    lightrun orange,radarl,2
	Sub lightrun(colorduring,direction,timenum)
		On Error Resume Next
		dim timefornext
		' color setting  
		dim a
		for each a in aLights
			SetLightColor a, colorduring, -1
		next
	
		if WeakComputer = false then
			spot2.visible = false
			if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
				GiOff
			end if
		end if

		select case direction
			Case arcbld
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomLeftDownOn, 90,timenum,0
				lrseq.Play SeqArcBottomLeftDownOff, 90,timenum,0
			Case arcblu
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomLeftUpOn, 90,timenum,0
				lrseq.Play SeqArcBottomLeftUpOff, 90,timenum,0
			Case arcbrd
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomRightDownOn, 90,timenum,0
				lrseq.Play SeqArcBottomRightDownOff, 90,timenum,0
			Case arcbru
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomRightUpOn, 90,timenum,0
				lrseq.Play SeqArcBottomRightUpOff, 90,timenum,0
			Case arctld
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopLeftDownOn, 90,timenum,0
				lrseq.Play SeqArcTopLeftDownOff, 90,timenum,0
			Case arctlu
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopLeftUpOn, 90,timenum,0
				lrseq.Play SeqArcTopLeftUpOff, 90,timenum,0
			Case arctrd
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopRightDownOn, 90,timenum,0
				lrseq.Play SeqArcTopRightDownOff, 90,timenum,0
			Case arctru
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopRightUpOn, 90,timenum,0
				lrseq.Play SeqArcTopRightUpOff, 90,timenum,0
			Case circlein
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqCircleInOn,50,timenum,0
				lrseq.Play SeqCircleInOff,50,timenum,0
			Case circleout
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqCircleOutOn,50,timenum,0
				lrseq.Play SeqCircleOutOff,50,timenum,0
			Case clockleft
				lrseq.UpdateInterval = 2
				timefornext = timenum*800
				lrseq.Play SeqClockLeftOn, 45,timenum,0
				lrseq.Play SeqClockLeftOff, 45,timenum,0
			Case clockright
				lrseq.UpdateInterval = 2
				timefornext = timenum*800
				lrseq.Play SeqClockRightOn,45,timenum,0
				lrseq.Play SeqClockRightOff,45,timenum,0
			Case diagdl
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagDownLeftOn, 25,timenum,0
				lrseq.Play SeqDiagDownLeftOff, 25,timenum,0
			Case diagdr
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagDownRightOn, 25,timenum,0
				lrseq.Play SeqDiagDownRightOff, 25,timenum,0
			Case diagul
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagUpLeftOn, 25,timenum,0
				lrseq.Play SeqDiagUpLeftOff, 25,timenum,0
			Case diagur
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagUpRightOn, 25,timenum,0
				lrseq.Play SeqDiagUpRightOff, 25,timenum,0
			Case down
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqDownOn, 15,timenum,0
				lrseq.Play SeqDownOff, 15,timenum,0
			Case fanld
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanLeftDownOn, 30,timenum,0
				lrseq.Play SeqFanLeftDownOff, 30,timenum,0
			Case fanlu
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanLeftUpOn, 30,timenum,0
				lrseq.Play SeqFanLeftUpOff, 30,timenum,0
			Case fanrd
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanRightDownOn, 30,timenum,0
				lrseq.Play SeqFanRightDownOff, 30,timenum,0
			Case fanru
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanRightUpOn, 30,timenum,0
				lrseq.Play SeqFanRightUpOff, 30,timenum,0
			Case hatch1h
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch1HorizOn, 25,timenum,0
				lrseq.Play SeqHatch1HorizOff, 25,timenum,0
			Case hatch1v
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch1VertOn, 75,timenum,0
				lrseq.Play SeqHatch1VertOff, 75,timenum,0
			Case hatch2h
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch2HorizOn, 25,timenum,0
				lrseq.Play SeqHatch2HorizOff, 25,timenum,0
			Case hatch2v
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch2VertOn, 75,timenum,0
				lrseq.Play SeqHatch2VertOff, 75,timenum,0
			'Case left
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqLeftOn, 50,timenum,0
				lrseq.Play SeqLeftOff, 50,timenum,0
			Case middleih
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleInHorizOn, 50,timenum,0
				lrseq.Play SeqMiddleInHorizOff, 50,timenum,0
			Case middleiv
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleInVertOn, 50,timenum,0
				lrseq.Play SeqMiddleInVertOff, 50,timenum,0
			Case middleoh
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleOutHorizOn, 50,timenum,0
				lrseq.Play SeqMiddleOutHorizOff, 50,timenum,0
			Case middleov
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleOutVertOn, 50,timenum,0
				lrseq.Play SeqMiddleOutVertOff, 50,timenum,0
			Case radarl
				lrseq.UpdateInterval = 4
				timefornext = timenum*700
				lrseq.Play SeqRadarLeftOn, 45,timenum,0
				lrseq.Play SeqRadarLeftOff, 45,timenum,0
			Case radarr
				lrseq.UpdateInterval = 4
				timefornext = timenum*700
				lrseq.Play SeqRadarRightOn, 45,timenum,0
				lrseq.Play SeqRadarRightOff, 45,timenum,0
			Case randoms
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqRandom,40,,timefornext
			'Case right
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqRightOn, 50,timenum,0
				lrseq.Play SeqRightOff, 50,timenum,0
			Case screwl
				lrseq.UpdateInterval = 2
				timefornext = timenum*500
				lrseq.Play SeqScrewLeftOn, 25,timenum,0
				lrseq.Play SeqScrewLeftOff, 25,timenum,0
			Case screwr
				lrseq.UpdateInterval = 2
				timefornext = timenum*500
				lrseq.Play SeqScrewRightOn, 25,timenum,0
				lrseq.Play SeqScrewRightOff, 25,timenum,0
			Case stripe1h
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe1HorizOn, 25,timenum,0
				lrseq.Play SeqStripe1HorizOff, 25,timenum,0
			Case stripe1v
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe1VertOn, 50,timenum,0
				lrseq.Play SeqStripe1VertOff, 50,timenum,0
			Case stripe2h
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe2HorizOn, 25,timenum,0
				lrseq.Play SeqStripe2HorizOff, 25,timenum,0
			Case stripe2v
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe2VertOn, 25,timenum,0
				lrseq.Play SeqStripe2VertOff, 25,timenum,0
			Case up
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqUpOn, 15,timenum,0
				lrseq.Play SeqUpOff, 15,timenum,0
			Case wiperl
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqWiperLeftOn, 45,timenum,0
				lrseq.Play SeqWiperLeftOff, 45,timenum,0
			Case wiperr
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqWiperRightOn, 45,timenum,0
				lrseq.Play SeqWiperRightOff, 45,timenum,0
		end Select


		runninglights = 1
		vpmtimer.addtimer timefornext, "nolongerrun '"

	end Sub

	Sub nolongerrun
		if WeakComputer = false then
			spot2.visible=true
			if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
				GiOn
			end if
		end if
		dim a
				For each a in aLights
				a.intensity = 7
				a.color = RGB(255, 252, 0)
				a.colorfull = RGB(255, 255, 255)
				next

		runninglights = 0
		lrseq.StopPlay
		'if bMultiBallMode = false Then
		'	relighttable
		'end if 
	end Sub

	dim ufogi:ufogi=0 
	dim ptgi:ptgi=0 
	dim modegi:modegi=0 
	dim illgi:illgi=0 
	dim supgi:supgi=0 
	dim ategi:ategi=0 


	'****************************
	' Pulsing pizza lights
	'****************************

	'pulsetim.enabled=False

	dim pint:pint=0
	dim pdir:pdir=0

	sub pizzastrobe(num)
		if num = 1 Then
			pulse1.state=1
			pulse2.state=1
			pulsetim.enabled=1
			Flasherbase007.image = "domeredbase"
			Flasherbase006.image = "domeredbase"
		Else
			pulse1.state=0
			pulse2.state=0
			pulsetim.enabled=0
			Flasherbase007.image = "domewhitebase"
			Flasherbase006.image = "domewhitebase"
		end if
	end Sub
	sub pulsetim_timer
		If pint=25 Then
			pdir=1
		end If
		if pint=0 Then
			pdir=0
		end If

		if pdir=0 Then
			pint=pint+1
		else
			pint=pint-1
		end if

		pulse1.intensity=pint
		pulse2.intensity=pint
	end Sub







	'****************************
	' Backwall Lamps
	'****************************
	'backlamp "run"
	sub backlamp(x)
		if WeakComputer = false then
			if x = "run" Then
				backwallrun.enabled=1
			end If
			if x = "flash" Then
				backwallflash.enabled=1
			end If
		end if
	end Sub



	sub flash1_hit 
		strip1.visible = 1

		vpmtimer.addtimer 200, "f1off '"
	End Sub
	sub f1off
		strip1.visible = 0

	end Sub

	sub flash001_hit
		flash1_hit
	end Sub

	sub flash2_hit 
		strip3.visible = 1


		vpmtimer.addtimer 200, "f2off '"
	End Sub
	sub f2off
		strip3.visible = 0
	end Sub

	sub flash002_hit
		flash2_hit
	end Sub

	sub flash3_hit 
		strip5.visible = 1

		vpmtimer.addtimer 200, "f3off '"

	End Sub
	sub f3off
		Strip5.visible = 0

	end Sub

	sub flash4_hit 
		Strip7.visible = 1

		vpmtimer.addtimer 200, "f4off '"
	End Sub
	sub f4off
		Strip7.visible = 0
	end Sub

	sub flash003_hit
		flash4_hit
	end Sub

	sub flash5_hit 
		strip9.visible = 1

		vpmtimer.addtimer 200, "f5off '"
	End Sub
	sub f5off
		strip9.visible = 0
	end Sub

	sub bwallon
		strip1.visible = 1

		Strip3.visible = 1

		Strip5.visible = 1
		Strip7.visible = 1
		Strip9.visible = 1
	end Sub

	sub bwalloff
		strip1.visible = 0

		Strip3.visible = 0

		Strip5.visible = 0
		Strip7.visible = 0
		Strip9.visible = 0
	end sub

	dim bwflash:bwflash=0
	sub backwallflash_timer
		bwflash=bwflash+1
		select case bwflash
			case 1:bwallon
			case 3:bwalloff
			case 5:bwallon
			case 7:bwalloff
			case 9:bwallon
			case 11:bwalloff
			case 13:bwallon
			case 15:bwalloff
			case 17:bwallon
			case 19:bwalloff
			case 21:bwallon
			case 23:bwalloff:backwallflash.enabled=0:bwflash=0
		end Select
	end Sub


	dim bwrun:bwrun=0
	sub backwallrun_timer
		bwrun=bwrun+1
		select case bwrun
			case 1:flash1_hit 
			case 2:flash2_hit 
			case 3:flash3_hit 
			case 4:flash4_hit 
			case 5:flash5_hit 
			case 6:flash1_hit 
			case 7:flash2_hit 
			case 8:flash3_hit 
			case 9:flash4_hit 
			case 10:flash5_hit 
			case 13:bwallon
			case 15:bwalloff
			case 17:bwallon
			case 19:bwalloff
			case 21:bwallon
			case 23:bwalloff:backwallrun.enabled=0:bwrun=0
		end Select
	end Sub



	'****************************
	' Flashers - Thanks Flupper
	'****************************

	Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6, FlashLevel7, FlashLevel8
	Flasherlight004.IntensityScale = 0
	Flasherlight005.IntensityScale = 0
	Flasherlight006.IntensityScale = 0
	Flasherlight007.IntensityScale = 0
	Flasherlight008.IntensityScale = 0

	'*** lower right rbg flasher ***
	Sub Flasherflash004_Timer()
	if WeakComputer = false then
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash004.TimerEnabled Then 
			Flasherflash004.TimerEnabled = True
			Flasherflash004.visible = 1
			Flasherlit004.visible = 1
		End If
		flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
		Flasherflash004.opacity = 1500 * flashx3
		Flasherlit004.BlendDisableLighting = 10 * flashx3
		Flasherbase004.BlendDisableLighting =  flashx3
		Flasherlight004.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel4)
		Flasherlit004.material = "domelit" & matdim
		FlashLevel4 = FlashLevel4 * 0.9 - 0.01
		If FlashLevel4 < 0.15 Then
			Flasherlit004.visible = 0
		Else
			Flasherlit004.visible = 1
			DOF 333, DOFPulse 'MX-Flash1
			DOF 433, DOFPulse 'rgbflash1
		end If
		If FlashLevel4 < 0 Then
			Flasherflash004.TimerEnabled = False
			Flasherflash004.visible = 0
		End If
	end if
	End Sub'

	'*** middle right rbg flasher ***
	Sub Flasherflash005_Timer()
		if WeakComputer = false then
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash005.TimerEnabled Then 
			Flasherflash005.TimerEnabled = True
			Flasherflash005.visible = 1
			Flasherlit005.visible = 1
			DOF 334, DOFPulse 'MX-Flash2
			DOF 434, DOFPulse 'rgbflash2
		End If
		flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
		Flasherflash005.opacity = 1500 * flashx3
		Flasherlit005.BlendDisableLighting = 10 * flashx3
		Flasherbase005.BlendDisableLighting =  flashx3
		Flasherlight005.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel5)
		Flasherlit005.material = "domelit" & matdim
		FlashLevel5 = FlashLevel5 * 0.9 - 0.01
		If FlashLevel5 < 0.15 Then
			Flasherlit005.visible = 0
		Else
			Flasherlit005.visible = 1
		end If
		If FlashLevel5 < 0 Then
			Flasherflash005.TimerEnabled = False
			Flasherflash005.visible = 0
		End If
	end if
	End Sub

	'*** left kicker rbg flasher ***
	Sub Flasherflash006_Timer()
		if WeakComputer = false then
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash006.TimerEnabled Then 
			Flasherflash006.TimerEnabled = True
			Flasherflash006.visible = 1
			Flasherlit006.visible = 1
		End If
		flashx3 = FlashLevel6 * FlashLevel6 * FlashLevel6
		Flasherflash006.opacity = 1500 * flashx3
		Flasherlit006.BlendDisableLighting = 10 * flashx3
		Flasherbase006.BlendDisableLighting =  flashx3
		Flasherlight006.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel6)
		Flasherlit006.material = "domelit" & matdim
		FlashLevel6 = FlashLevel6 * 0.9 - 0.01
		If FlashLevel6 < 0.15 Then
			Flasherlit006.visible = 0
		Else
			Flasherlit006.visible = 1
			DOF 335, DOFPulse 'MX-Flash3
			DOF 435, DOFPulse 'rgbflash3
		end If
		If FlashLevel6 < 0 Then
			Flasherflash006.TimerEnabled = False
			Flasherflash006.visible = 0
		End If
	end if
	End Sub

	'*** center rbg flasher ***
	Sub Flasherflash007_Timer()
		if WeakComputer = false then
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash007.TimerEnabled Then 
			Flasherflash007.TimerEnabled = True
			Flasherflash007.visible = 1
			Flasherlit007.visible = 1
		End If
		flashx3 = FlashLevel7 * FlashLevel7 * FlashLevel7
		Flasherflash007.opacity = 1500 * flashx3
		Flasherlit007.BlendDisableLighting = 10 * flashx3
		Flasherbase007.BlendDisableLighting =  flashx3
		Flasherlight007.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel7)
		Flasherlit007.material = "domelit" & matdim
		FlashLevel7 = FlashLevel7 * 0.9 - 0.01
		If FlashLevel7 < 0.15 Then
			Flasherlit007.visible = 0
		Else
			Flasherlit007.visible = 1
			DOF 336, DOFPulse 'MX-Flash4
			DOF 436, DOFPulse 'rgbflash4
		end If
		If FlashLevel7 < 0 Then
			Flasherflash007.TimerEnabled = False
			Flasherflash007.visible = 0
		End If
	end if
	End Sub

	'*** top right rbg flasher ***
	Sub Flasherflash008_Timer()
		if WeakComputer = false then
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash008.TimerEnabled Then 
			Flasherflash008.TimerEnabled = True
			Flasherflash008.visible = 1
			Flasherlit008.visible = 1
		End If
		flashx3 = FlashLevel8 * FlashLevel8 * FlashLevel8
		Flasherflash008.opacity = 1500 * flashx3
		Flasherlit008.BlendDisableLighting = 10 * flashx3
		Flasherbase008.BlendDisableLighting =  flashx3
		Flasherlight008.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel8)
		Flasherlit008.material = "domelit" & matdim
		FlashLevel8 = FlashLevel8 * 0.9 - 0.01
		If FlashLevel8 < 0.15 Then
			Flasherlit008.visible = 0
		Else
			Flasherlit008.visible = 1
			DOF 337, DOFPulse 'MX-Flash5
			DOF 437, DOFPulse 'rgbflash5
		end If
		If FlashLevel8 < 0 Then
			Flasherflash008.TimerEnabled = False
			Flasherflash008.visible = 0
		End If
	end if
	End Sub





	flasherseq.enabled = 1
	dim flasherpos:flasherpos = 0
	dim flasherdir:flasherdir = "left"


	'flasherspop orange,"right" 'left,right,bottom,rightkick,top,crazy

	Sub flasherspop(n,x)
		On Error Resume Next
		flasherpos = 0
		dim a
		flasherdir = x
		flasherseq.enabled = 1
		for each a in aFlashers
			SetLightColor a, n, -1
		Next
	End Sub

	Sub flasherseq_timer
		On Error Resume Next
		flasherpos = flasherpos + 1
		If flasherdir = "left" Then
			select case flasherpos
				case 1
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 2
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 3
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 4
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 5
					FlashLevel7 = 1 : Flasherflash007_Timer
				case 6
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 7
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 8
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 9
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 10
					FlashLevel7 = 1 : Flasherflash007_Timer
				case 11
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 12
					FlashLevel5 = 1 : Flasherflash005_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "right" Then
			select case flasherpos
				case 1
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 2
					FlashLevel7 = 1 : Flasherflash007_Timer
				case 3
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 4
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 5
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 6
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 7
					FlashLevel7 = 1 : Flasherflash007_Timer
				case 8
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 9
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 10
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 11
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 11
					FlashLevel7 = 1 : Flasherflash007_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "bottom" Then
			select case flasherpos
				case 1
					FlashLevel8 = 1 : Flasherflash008_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer

				case 4
					FlashLevel8 = 1 : Flasherflash008_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer

				case 7
					FlashLevel8 = 1 : Flasherflash008_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer

				case 10
					FlashLevel5 = 1 : Flasherflash005_Timer
					FlashLevel8 = 1 : Flasherflash008_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "leftkick" Then
			select case flasherpos
				case 1
					'FlashLevel3 = 1 : Flasherflash3_Timer

				case 4
					FlashLevel3 = 1 : Flasherflash3_Timer

				case 7
					FlashLevel3 = 1 : Flasherflash3_Timer

				case 10
					FlashLevel3 = 1 : Flasherflash3_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "rightkick" Then
			select case flasherpos
				case 1
					'FlashLevel3 = 1 : Flasherflash3_Timer

				case 4
					FlashLevel5 = 1 : Flasherflash005_Timer

				case 7
					FlashLevel5 = 1 : Flasherflash005_Timer

				case 10
					FlashLevel5 = 1 : Flasherflash005_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "top" Then
			select case flasherpos
				case 1
					FlashLevel6 = 1 : Flasherflash006_Timer
					FlashLevel7 = 1 : Flasherflash007_Timer

				case 4
					FlashLevel6 = 1 : Flasherflash006_Timer
					FlashLevel7 = 1 : Flasherflash007_Timer

				case 7
					FlashLevel6 = 1 : Flasherflash006_Timer
					FlashLevel7 = 1 : Flasherflash007_Timer

				case 10
					FlashLevel6 = 1 : Flasherflash006_Timer
					FlashLevel7 = 1 : Flasherflash007_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "crazy" Then
			select case flasherpos
				case 1
					FlashLevel4 = 1 : Flasherflash004_Timer
					FlashLevel7 = 1 : Flasherflash007_Timer
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 2
					FlashLevel8 = 1 : Flasherflash008_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 3
					FlashLevel5 = 1 : Flasherflash005_Timer
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 4
					FlashLevel4 = 1 : Flasherflash004_Timer
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 5
					FlashLevel7 = 1 : Flasherflash007_Timer
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 6
					FlashLevel4 = 1 : Flasherflash004_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 7
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 8
					FlashLevel5 = 1 : Flasherflash005_Timer
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 9
					FlashLevel4 = 1 : Flasherflash004_Timer
					FlashLevel8 = 1 : Flasherflash008_Timer
				case 10
					FlashLevel7 = 1 : Flasherflash007_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer
				case 11
					FlashLevel4 = 1 : Flasherflash004_Timer
					FlashLevel7 = 1 : Flasherflash007_Timer
				case 12
					FlashLevel8 = 1 : Flasherflash008_Timer
					FlashLevel5 = 1 : Flasherflash005_Timer
					FlashLevel6 = 1 : Flasherflash006_Timer
				case 13
					FlashLevel5 = 1 : Flasherflash005_Timer
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 14
					FlashLevel4 = 1 : Flasherflash004_Timer
				case 15
					FlashLevel7 = 1 : Flasherflash007_Timer
					FlashLevel8 = 1 : Flasherflash008_Timer
					FlashLevel6 = 1 : Flasherflash006_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If
	end Sub
		












'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Attract Mode
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Sub ShowTableInfo
		dmdintroloop:introtime=0
	End Sub

	Dim introposition
	introposition = 0


	sub hstime_timer
		ScrollHS
	end sub

	Sub dmdintroloop
		playclear pBackglass
		introtime=0
		introposition = introposition + 1
		Dim waittime
		Select Case introposition
		Case 1
				clearhslabels
				PuPlayer.LabelShowPage pBackglass, 1,0,""
				BHSAttract=False
				hstime.enabled=false
				pausemedia pMusic
				'playmedia "intro.mp4","AttractMode",pBackglass,"",46000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Case 2
				clearhslabels
				PuPlayer.LabelShowPage pBackglass, 1,0,""
				BHSAttract=False
				hstime.enabled=false
				pausemedia pMusic
				playmedia "intro.mp4","AttractMode",pBackglass,"",46000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

		Case 3
				hstime.enabled=false
				playmedia "crust.mp4","AttractMode",pBackglass,"",14000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

		Case 4
				playmedia "hsbg.mp4","video-scenes",pBackglass,"",46000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

				puPlayer.LabelSet pBackglass,"hsTitle",	"High Scores"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':50  ,'ypos':36}"
				if bMediaPaused(pMusic) then 
					resumemedia pMusic
				else 
					PlayGeneralMusic
				end if
				localHS

		Case 5
				playmedia "hsbg.mp4","video-scenes",pBackglass,"",46000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

				puPlayer.LabelSet pBackglass,"hsTitle",	"Osb Daily"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':50  ,'ypos':36}"
				bHSAttract=true
				'PuPlayer.LabelShowPage pBackglass, 4,0,""
				hstime.enabled=true

		Case 6
				clearhslabels
				PuPlayer.LabelShowPage pBackglass, 1,0,""
				BHSAttract=False
				hstime.enabled=false
				playmedia "psa.mp4","AttractMode",pBackglass,"",7000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Case 7
				pDMDSplashBig introposition, 2, 8388863
			introposition = 1
		Case 8
				pDMDSplashBig introposition, 2, 8388863
			introposition = 1
		End Select
	End Sub


	Sub localHS()
		dim i
		dim ypos
		dim vis
		Dim score
		Dim Name
		puPlayer.LabelSet pBackglass,"hsRT",	"#"				,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':42  ,'ypos':42}"
		puPlayer.LabelSet pBackglass,"hsST",	"Score"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':54.5,'ypos':42}"
		puPlayer.LabelSet pBackglass,"hsNT",	"INI"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':60  ,'ypos':42}"
		for i = 0 to 4
			ypos=47+(i*4)
			puPlayer.LabelSet pBackglass,"hsR"&i,	"#" &i+1	,1,"{'mt':2,'size':2.2,'xpos':42  ,'ypos':"& ypos &"}"
			puPlayer.LabelSet pBackglass,"hsS"&i,	HighScore(i)	    ,1,"{'mt':2,'size':2.2,'xpos':54.5  ,'ypos':"& ypos &"}"
			puPlayer.LabelSet pBackglass,"hsN"&i,	HighScoreName(i)		,1,"{'mt':2,'size':2.2,'xpos':60  ,'ypos':"& ypos &"}"
		Next 
	End Sub 


	Dim introtime
	introtime = 0

	Sub intromover_timer
		introtime = introtime + 1
		If introposition = 1 Then
			If introtime = 1 Then
				DMDintroloop
			End If
		End If
		If introposition = 2 Then
			If introtime = 46 Then
				DMDintroloop
			End If
		End If
		If introposition = 3 Then
			If introtime = 14 Then
				DMDintroloop
			End If
		End If
		If introposition = 4 Then 
			If introtime = 10 Then
				DMDintroloop
			End If
		End If
		If introposition = 5 Then 
			If introtime = 41 Then
				DMDintroloop
			End If
		End If
		If introposition = 6 Then
			If introtime = 8 Then
				introposition = 0
				DMDintroloop
			End If
		End If
		If introposition = 7 Then
			If introtime = 0 Then
				introposition = 0
				DMDintroloop
			End If
		End If
		If introposition = 0 Then
				DMDintroloop
		End If
	End Sub

	Sub StartAttractMode()
				clearhslabels
		intromover.enabled = true
		StartRainbow alights
		bAttractMode = True
		DOF 310, DOFOn 'MX-Attract
		DOF 311, DOFOn 'MX-Undercab1
		StartLightSeq
		'vpmtimer.addtimer 500, "dmdintroloop'"
		
	End Sub

	Sub StopAttractMode()
				clearhslabels
		intromover.enabled = false
		bAttractMode = False
		DOF 310, DOFOff 'MX-Attract
		LightSeqAttract.StopPlay
		LightSeqFlasher.StopPlay
		ResetAllLightsColor
		pDMDStartGame				' Turn off Pup Attract
		StopRainbow alights
	End Sub

	Sub StartLightSeq()
		on error resume next
		'lights sequences
		'LightSeqFlasher.UpdateInterval = 150
		'LightSeqFlasher.Play SeqRandom, 10, , 50000
		'LightSeqAttract.UpdateInterval = 25
		'LightSeqAttract.Play SeqBlinking, , 5, 150
		'LightSeqAttract.Play SeqRandom, 40, , 4000
		'LightSeqAttract.Play SeqAllOff
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqCircleOutOn, 15, 3
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqRightOn, 50, 1
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqLeftOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 40, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 40, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqRightOn, 30, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqLeftOn, 30, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqCircleOutOn, 15, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqStripe1VertOn, 50, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe1VertOn, 50, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe2VertOn, 50, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe1VertOn, 25, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe2VertOn, 25, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
	End Sub

	Sub LightSeqAttract_PlayDone()
		StartLightSeq()
	End Sub

	Sub LightSeqTilt_PlayDone()
		LightSeqTilt.Play SeqAllOff
	End Sub

	Sub LightSeqSkillshot_PlayDone()
		LightSeqSkillshot.Play SeqAllOff
	End Sub




' END PLATFORM SCRIPT
'
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'
' START TABLE THEME SCRIPTS



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Scoring
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\	
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
on error resume next
    dim i
    dim NumString

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i) & "," & right(NumString, Len(NumString)-i)
        end if
    Next
    FormatScore = NumString
End function

	Sub AddScore(points)
		If triplepoints Then	
			AddScore points*3

		Elseif doublepoints Then 
			AddScore points*2

		Else 
			AddScore points 
		End If
	End Sub 

	Sub AddScore(points)
		If(Tilted = False) Then
			' add the points to the current players score variable
			Score(CurrentPlayer) = Score(CurrentPlayer) + points
			If pspec(currentplayer) = 0 Then
				If Score(CurrentPlayer) > 5000000 Then
					AwardSpecial
					pspec(CurrentPlayer) = 1
				end if 
			end If
			' update the score displays
			'DMDScore
			pUpdateScores
		End if
	End Sub

	Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
		If(Tilted = False) Then
			' add the bonus to the current players bonus variable
			BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
			' update the score displays
			'DMDScore
		End if
	End Sub

	Sub AwardExtraBall(bKick)
		if miniend=0 then
		if bKick then
			playmedia "","crust-extraball",pBackglass,"cineon",3000,"ebkickit",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Else
			playmedia "","crust-extraball",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End if 
		end If
		ShowMsg "Extra ball awarded", "" 'FormatScore(BonusPoints(CurrentPlayer))
		LightShootAgain.State = 1
		'LightSeqFlasher.UpdateInterval = 150
		'LightSeqFlasher.Play SeqRandom, 10, , 10000
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	End Sub

		dim pspec(4)


	Sub AwardSpecial()
		Credits = Credits + 1
		DOF 140, DOFOn
		PlaySound SoundFXDOF("knocker",136,DOFPulse,DOFKnocker)		
		DOF 115, DOFPulse
		GiEffect 1
		LightEffect 1
			playmedia "","audio-replay",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "replay.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End Sub

	Sub extrasmallpoints
		If doublepoints = true Then
			AddScore 20
		End If
		If triplepoints = true Then	
			AddScore 30
		End If
		If doublepoints = false and triplepoints = false Then	
			AddScore 10
		End If
	End Sub

	Sub Smallpoints
		If doublepoints = true Then
			AddScore 220
		End If
		If triplepoints = true Then	
			AddScore 330
		End If
		If doublepoints = false and triplepoints = false Then	
			AddScore 110
		End If
	End Sub

	Sub Mediumpoints
		If doublepoints = true Then
			AddScore 2000
		End If
		If triplepoints = true Then	
			AddScore 3000
		End If
		If doublepoints = false and triplepoints = false Then	
			AddScore 1000
		End If
	End Sub

	Sub largepoints
		If doublepoints = true Then
			AddScore 20000
		End If
		If triplepoints = true Then	
			AddScore 30000
		End If
		If doublepoints = false and triplepoints = false Then	
			AddScore 10000
		End If
	End Sub


	Sub extralargepoints
		If doublepoints = true Then
			AddScore 200000
		End If
		If triplepoints = true Then	
			AddScore 300000
		End If
		If doublepoints = false and triplepoints = false Then	
			AddScore 100000
		End If
	End Sub


	Sub superpoints
		If doublepoints = true Then
			AddScore 800000
		End If
		If triplepoints = true Then	
			AddScore 1200000
		End If
		If doublepoints = false and triplepoints = false Then	
			AddScore 400000
		End If
	End Sub

	Sub looppointscore
		Select Case loopbonus
			Case 0
				AddScore 200
			Case 1
				AddScore 200
			Case 2
				AddScore 400
			Case 3
				AddScore 600
			Case 4
				AddScore 800
			Case 5
				AddScore 1000
		End Select
	End Sub

	Sub targetpointscore
		Select Case targetbonuses
			Case 0
				AddScore 100
			Case 1
				AddScore 200
			Case 2
				AddScore 400
			Case 3
				AddScore 600
			Case 4
				AddScore 800
			Case 5
				AddScore 1000
		End Select
	End Sub

	Sub JackpotScore
		dim JPScore:JPScore=0
		Select Case pizzasize
			case 1:
				JPScore=80000
			case 2:
				JPScore=130000
			case 3:
				JPScore=200000
			case 4:
				JPScore=300000
		End Select
					playmedia "","audio-jackpot",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "","video-jackpot",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		lightrun orange,circleout,1
		ShowMsg "Jackpot", FormatScore(JPScore)
		AddScore JPScore
	End Sub



' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Drain & Plunger Functions
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Drain Function, yeah it's beastly
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Dim Saves
	Dim Drains
	Dim BIP
	BIP = 0

	Sub Drain_Hit()
		startB2S(7)
debug.print "Drain Hit"
		Drain.DestroyBall
		BallsOnPlayfield = BallsOnPlayfield - 1
		PlaySound "fx_drain"
		'if Tilted the end Ball Mode
		If Tilted Then
			StopEndOfBallMode
		End If
		If(bGameInPLay = True) AND(Tilted = False) and DisableFlippers=False Then
			If(bBallSaverActive = True) Then
				AddMultiball 1
				bAutoPlunger = True
				If bMultiBallMode = False Then
					Ballsaved
				End If
			Else
				' cancel any multiball if on last ball (ie. lost all other balls)
				If(BallsOnPlayfield = 1) Then
					' AND in a multi-ball??
					If(bMultiBallMode = True) then
						' not in multiball mode any more
						If pizzamulti = True Then EndPizza(pizzaTimeFinished)
						If ufomulti = true Then EndUfo
						bMultiBallMode = False	
					End If

					if videoready then StartVideoMode 		' Now we can do video mode since we are down to 1 ball
					' you may wish to change any music over at this point and
					' turn off any multiball specific lights	
		
					gicolor white
					'PlayGeneralMusic
					'CurrentSong
				End If

				' was that the last ball on the playfield
				If(BallsOnPlayfield = 0) Then
					' Stop Music
					DOF 341, DOFPulse 'MX-Balllost
					DOF 338, DOFOff 'MX UFo off
					DOF 441, DOFPulse ' RGb
					DOF 342, DOFOff
					DOF 343, DOFOff
					DOF 344, DOFOff
					DOF 345, DOFOff
					DOF 346, DOFOff
					bMultiBallMode = False			' Just in case 
					PlaySong "m_wait"
					gioff
					StopWizardSupreme
					StopSpecialMode
					StopBeerFrenzy
					Balldrained
					PlayerState(CurrentPlayer).Save		' Save states before we change
					'playmedia "", "audio-drain", pCallouts, "", 1000, "", 1, 1
					dim dn:dn=RndNum(1,8)
					select case dn
						case 1:playmedia "d1.mp4", "crust-drain", pBackglass, "cineon", 1700, "EndOfBall", 1, 3
						case 2:playmedia "d2.mp4", "crust-drain", pBackglass, "cineon", 1200, "EndOfBall", 1, 3
						case 3:playmedia "d3.mp4", "crust-drain", pBackglass, "cineon", 2400, "EndOfBall", 1, 3
						case 4:playmedia "d4.mp4", "crust-drain", pBackglass, "cineon", 2500, "EndOfBall", 1, 3
						case 5:playmedia "d5.mp4", "crust-drain", pBackglass, "cineon", 3200, "EndOfBall", 1, 3
						case 6:playmedia "d6.mp4", "crust-drain", pBackglass, "cineon", 2500, "EndOfBall", 1, 3
						case 7:playmedia "d7.mp4", "crust-drain", pBackglass, "cineon", 2400, "EndOfBall", 1, 3
						case 8:playmedia "d8.mp4", "crust-drain", pBackglass, "cineon", 2200, "EndOfBall", 1, 3
					end Select
					'caboff
					lightrun blue,down,4
			ShowMsg "Drain, sad.", "=(" 'FormatScore(BonusPoints(CurrentPlayer))
					StopEndOfBallMode
				End If
			End If
		End If
	End Sub

	Sub Balldrained
		Drains = Drains + 1
		Select Case Drains
			'Case 1 DMD "black.png", "drain", "sad", 4000
			Case 1 pDMDSplashBig  "Drain sad", 2, 8388863
			'Case 2 DMD "black.png", "wow", "you suck", 3000
			'Case 3 DMD "black.png", "Are Your", "eyes open?", 5000:Drains = 0
		End Select
	End Sub

	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Ball Saver Functions
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	Sub Ballsaved
		playmedia "", "crust-ballsaved", pBackglass, "", 4000, "", 1, 1
		'playmedia "", "audio-ballsaved", pCallouts, "", 1500, "", 1, 1
			ShowMsg "Ball saved", "" 'FormatScore(BonusPoints(CurrentPlayer))
		Saves = Saves + 1
		Select Case Saves
			'Case 1 DMD "black.png", "balls", "shaved", 3000
			'Case 2 DMD "black.png", "balls", "shaved", 3000:Saves = 0
		End Select
	End Sub

	Sub ballsavestarttrigger_hit
debug.print "Bal'Save Trigger"
		If(bBallSaverReady = True) AND(bstcurrent <> 0) And(bBallSaverActive = False) Then
			EnableBallSaver bstcurrent
		End If
	End Sub


	Sub ShooterEnd_Hit
		If canskillshot = True Then
			skillarrow.state = 2
		End If
	End Sub

	Sub skillt_hit
		ultracombo 1
		If skillarrow.state = 2 Then
			skillvalues = skillvalues + 1
			Select Case skillvalues
				Case 0
				Case 1
					largepoints
					'DMD "black.png", "Skill", "Shot", 1000
				Case 2
					largepoints
					'DMD "black.png", "Skill", "Shot", 1000
				Case 3
					extralargepoints
					'DMD "black.png", "Super", "Skillshot", 1000
				Case 4
					extralargepoints
					'DMD "black.png", "Super", "Skillshot", 1000
				Case 5
					extralargepoints
					'DMD "black.png", "Super", "Skillshot", 1000
				Case 6
					extralargepoints
					'DMD "black.png", "Super", "Skillshot", 1000
				Case 7
					extralargepoints
					'DMD "black.png", "Super", "Skillshot", 1000
				Case 8
					extralargepoints
					'DMD "black.png", "OK sir", "You've Had Enough", 1000
					skillarrow.state = 0
			End Select
		End If
	End Sub

	Sub EnableBallSaver(seconds)
		bBallSaverActive = True
		bBallSaverReady = False
		' start the timer
		BallSaverTimerExpired.Interval = 1000 * seconds
		BallSaverTimerExpired.Enabled = True
		BallSaverTimerExpiredlag.Interval = 1000 * seconds + 4000
		BallSaverTimerExpiredlag.Enabled = True
		BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
		BallSaverSpeedUpTimer.Enabled = True
		' if you have a ball saver light you might want to turn it on at this point (or make it flash)
		LightShootAgain.BlinkInterval = 160
		LightShootAgain.State = 2
	End Sub

	Sub BallSaverTimerExpired_Timer()
		BallSaverTimerExpired.Enabled = False
			If(ExtraBallsAwards(CurrentPlayer) = 0) Then
				LightShootAgain.State = 0
			Else
				LightShootAgain.State = 1
			End If
	End Sub

	Sub BallSaverTimerExpiredlag_Timer()
		BallSaverTimerExpiredlag.Enabled = False
		bBallSaverActive = False
		skillarrow.state = 0
			ShowMsg "ball save over", "" 'FormatScore(BonusPoints(CurrentPlayer))
	End Sub

	Sub BallSaverSpeedUpTimer_Timer()
		BallSaverSpeedUpTimer.Enabled = False
		LightShootAgain.BlinkInterval = 80
		LightShootAgain.State = 2
	End Sub



	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Plunger Functions
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	Sub Plunger_Init()
		PlaySound "ballrelease",0,1,0,0.25
		'Plunger.CreateBall
		'BallRelease.CreateBall
		'BallRelease.Kick 90, 7
		BIP = BIP +1
	End Sub

	Sub swPlungerRest_Hit()
		PlaySound "fx_sensor", 0, 1, 0.15, 0.25
		bSuperSkillShotEnabled = False 
		bBallInPlungerLane = True
		'Update the Scoreboard
		'DMDScoreNow
		' kick the ball in play if the bAutoPlunger flag is on
		If bAutoPlunger Then
			if ufomulti = True Then
			PlungerIM.Strength = 30
			else
			PlungerIM.Strength = 45
			end If
			PlungerIM.AutoFire
			PlungerIM.Strength = Plunger.MechStrength
			DOF 114, DOFPulse		
			DOF 115, DOFPulse
			bAutoPlunger = False
			bAutoPlunged=True
		End If
		DOF 317, DOFOn 	'MX, Launch Ball - Ball Ready to Shoot
		If bSkillShotReady Then
			swPlungerRest.TimerEnabled = 1
			UpdateSkillshot()
		If NOT bMultiballMode Then
			Whatsong
			CurrentSong
		End If
		End If
		LastSwitchHit = "swPlungerRest"
	End Sub

	Sub swPlungerRest_UnHit()

		bBallInPlungerLane = False
		swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
		'DMDScoreNow
		DOF 317, DOFOff 	'MX, Launch Ball - Ball Ready to Shoot
		DOF 318, DOFPulse 	'DOF MX - Ball Launched (AutoPlunger)
		If bSkillShotReady Then
			ResetSkillShotTimer.Enabled = True
		End If

		if bAutoPlunged = False and PlayerState(CurrentPlayer).SMode=-1 then 
			'PlayGeneralMusic		' Start some music unless we are already in a mode or MB
			if LFPress then 
debug.print "Super Skillshot Enabled"
				bSuperSkillShotEnabled=True												' Start super skillshot 
				Gate7.Open = True
				tmrResetSuperSkillshot.Interval = 8000
				tmrResetSuperSkillshot.Enabled = True
			End If 
		End If 

		bAutoPlunged = False 
	End Sub

	' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

	Sub swPlungerRest_Timer
		playmedia "", "crust-delay", pBackglass, "", 3000, "", 1, 3
		swPlungerRest.TimerEnabled = 0
	End Sub

	Sub PlayModeMusic(strModeSong)
		pausemedia pMusic

		If strModeSong = "Sonny & The Sunsets - Green Blood.mp3" Then
			if houseband = 1 Then
				playmedia "Pizza Don't Cry.mp3", "Musicipfree", pAudio, "", -1, "", 1, 1	
			else
				playmedia strModeSong, MusicDir, pAudio, "", -1, "", 1, 1
			end if
		ElseIf strModeSong = "Pizza Time - Pizza Time.mp3" Then
			playmedia strModeSong, MusicDir, pAudio, "", -1, "", 1, 1

		elseIf strModeSong = "The Misfits-I Turned Into A Martian.mp3" or strModeSong = "Sham 69 - Hurry Up Harry.mp3" or strModeSong = "Ty Segall - Every 1's a Winner.mp3"Then
			if houseband = 1 Then
				playmedia "I turned into a margherita.mp3", "Music", pAudio, "", -1, "", 1, 1	
			else
				playmedia strModeSong, MusicDir, pAudio, "", -1, "", 1, 1
			end if

		Else
			if houseband = 1 Then
				playmedia "Pulled Up Cheese.mp3", "Music", pAudio, "", -1, "", 1, 1	
			else
				playmedia strModeSong, MusicDir, pAudio, "", -1, "", 1, 1
			end if
		end If
	End Sub 

	Sub StopModeMusic()
		playclear pAudio
		if bMediaPaused(pMusic) then 
			resumemedia pMusic
		else 
			PlayGeneralMusic
		End If 
	End Sub

	Sub PlayGeneralMusic()
		If houseband=1 Then
			playmedia "", "Musicipfree", pMusic, "", -1, "", 1, 1
		Else
		dim index
		index = RndNum(0,11)
		Select case index
			case 0:
				playmedia "TheMekons-WhereWereYou.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "The Mekons", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 1:
				playmedia "Gang of Four - Damaged Goods.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "Gang of Four", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 2:
				playmedia "king tuff - animal.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "King Tuff", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 3:
				playmedia "New York Dolls - Personality Crisis.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "new york dolls", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 4:
				playmedia "Ramones - Blitzkrieg Bop.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "the ramones", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 5:
				playmedia "Sex Pistols - Anarchy in the U.K..mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "sex pistols", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 6:
				playmedia "Stiff Little Fingers - Alternative Ulster.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "stiff little fingers", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 7:
				playmedia "The Adicts - Too Young.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "the adicts", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 8:
				playmedia "The Buzzcocks - Why Can't I Touch It.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "the buzzcocks", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 9:
				playmedia "The Modern Lovers - Roadrunner.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "the modern lovers", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 10:
				playmedia "The Only Ones - Another Girl Another Planet.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "the only ones", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
			case 11:
				playmedia "The Replacements - Bastards of Young.mp3", MusicDir, pMusic, "", -1, "", 1, 1

				ShowMsg "the replacements", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Now Playing:", "" 'FormatScore(BonusPoints(CurrentPlayer))
		End Select 
		End if

'		if pizzamulti = False and bWizardModeSupreme=False then 	' Dont stop music when in pizza time or Supreme Wizard
'			playclear pMusic
'			if PlayerState(CurrentPlayer).SMode <> -1 and strModeSong <> "" then 	' We are in a mode start mode music back up
'				playmedia strModeSong, MusicDir, pMusic, "", -1, "", 1, 1
'			else 
'				dim index
'				index = RndNum(0,11)
'				Select case index
'					case 0:
'						playmedia "TheMekons-WhereWereYou.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 1:
'						playmedia "Gang of Four - Damaged Goods.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 2:
'						playmedia "king tuff - animal.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 3:
'						playmedia "New York Dolls - Personality Crisis.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 4:
'						playmedia "Ramones - Blitzkrieg Bop.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 5:
'						playmedia "Sex Pistols - Anarchy in the U.K..mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 6:
'						playmedia "Stiff Little Fingers - Alternative Ulster.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 7:
'						playmedia "The Adicts - Too Young.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 8:
'						playmedia "The Buzzcocks - Why Can't I Touch It.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 9:
'						playmedia "The Modern Lovers - Roadrunner.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 10:
'						playmedia "The Only Ones - Another Girl Another Planet.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'					case 11:
'						playmedia "The Replacements - Bastards of Young.mp3", MusicDir, pMusic, "", -1, "", 1, 1
'				End Select 
'			End if 
'		End if 
	End Sub 

' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  B2S Lighting Functions
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  



	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	 '-> B2S Light Show
	 '-> cause i mean everyone loves a good light show
	 '-> 1 = 
	 '-> 2 =  
	 '-> 3 =  
	 '-> 4 = 
	 '-> 5 = 
	 '-> 6 = 
	 '-> 7 = 
	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	Dim b2sstep
	b2sstep = 0
	b2sflash.enabled = 0
	Dim b2satm

	Sub startB2S(aB2S)
		b2sflash.enabled = 1
		b2satm = ab2s
	End Sub

	Sub b2sflash_timer
		If B2SOn Then
			b2sstep = b2sstep + 1
			Select Case b2sstep
				Case 0
				Controller.B2SSetData b2satm, 0
				Case 1
				Controller.B2SSetData b2satm, 1
				Case 2
				Controller.B2SSetData b2satm, 0
				Case 3
				Controller.B2SSetData b2satm, 1
				Case 4
				Controller.B2SSetData b2satm, 0
				Case 5
				Controller.B2SSetData b2satm, 1
				Case 6
				Controller.B2SSetData b2satm, 0
				Case 7
				Controller.B2SSetData b2satm, 1
				Case 8
				Controller.B2SSetData b2satm, 0
				b2sstep = 0
				b2sflash.enabled = 0
			End Select
		End If
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Minigame
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


		'*************************
		' PUP MINI GAME (videomode)
		' create a timer (disable default) PuPGameTimer (interval 300)
		' when you want to start game call PuPGameStartMiniGame
		' look at PuPMiniGameEnd to do something when gameover.
		' game music be called PuPMiniGame.exe inside ofr MiniGame folder of puppack!
		' see sample of key_down and key_up in table script!
		'*************************

			Sub AlwaysOnTop(appName, regExpTitle, setOnTop)
			' @description: Makes a window always on top if setOnTop is true, else makes it normal again. Will wait up to 10 seconds for window to load.
			' @author: Jeremy England (SimplyCoded)
			  If (setOnTop) Then setOnTop = "-1" Else setOnTop = "-2"
			  CreateObject("wscript.shell").Run "powershell -Command """ & _    
				"$Code = Add-Type -MemberDefinition '" & vbcrlf & _
				"  [DllImport(\""user32.dll\"")] public static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X,int Y, int cx, int cy, uint uFlags);" & vbcrlf & _
				"  [DllImport(\""user32.dll\"")] public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);" & vbcrlf & _
				"  public static void AlwaysOnTop (IntPtr fHandle, int insertAfter) {" & vbcrlf & _
				"    if (insertAfter == -1) { ShowWindow(fHandle, 4); }" & vbcrlf & _
				"    SetWindowPos(fHandle, new IntPtr(insertAfter), 0, 0, 0, 0, 3);" & vbcrlf & _
				"  }' -Name PS -PassThru" & vbcrlf & _
				"for ($s=0;$s -le 9; $s++){$hWnd = (GPS " & appName & " -EA 0 | ? {$_.MainWindowTitle -Match '" & regExpTitle & "'}).MainWindowHandle;Start-Sleep 1;if ($hWnd){break}}" & vbcrlf & _  
				"$Code::AlwaysOnTop($hWnd, " & setOnTop & ")""", 0, True
			End Sub


		DIM PuPGameRunning:PuPGameRunning=false
		DIM PuPGameTimeout
		DIM PuPGameInfo
		DIM PuPGameScore
		Dim inminigame

		Dim tmrModeCountdownSave
		Dim tmrBeerFrenzySave 

		sub startminigame
		playmedia "minigameintro.mp4","video-minigame",pBackglass,"cineon",4000,"delayedstart",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
		end sub

		sub delayedstart
		vpmtimer.addtimer 2000, "PuPGameStartMiniGame '"
		end Sub

		Sub PuPGameStartMiniGame
			if PuPGameRunning Then Exit Sub


			'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 1 }"      'this will showsuccess overlay if applicable
			ShowMsg "minigame start!", "" 'FormatScore(BonusPoints(CurrentPlayer))

			tmrModeCountdownSave=False 
			tmrBeerFrenzySave=False 
			if tmrModeCountdown.Enabled then 
				tmrModeCountdownSave=True
				tmrModeCountdown.Enabled=False 
			End if 
			if tmrBeerFrenzy.Enabled then 
				tmrBeerFrenzySave=True
				tmrBeerFrenzy.Enabled=False 
			End if 
			if bWizardModeAte then tmrModeCountdown.Enabled = False 

			PlayModeMusic "Sonny & The Sunsets - Green Blood.mp3"

			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":16, ""EX"": ""MiniGame\\PUPShooter1.exe"", ""WT"": ""PUPShooter"", ""RS"":1 , ""TO"":15 , ""WZ"":0 , ""SH"": 1 , ""FT"":""Visual Pinball Player"" }" 
			AlwaysOnTop "PUPShooter1", ".", True
			'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
			PuPGameTimeout=-3    'check for timeout  every 500 ms
			PuPGameRunning=true	
			PuPGameTimer.enabled=true
			PuPlayer.playpause pBackglass
		End Sub


		dim miniend:miniend=0
		Sub PuPMiniGameEnd(gamescore)
			miniend=1
			if PuPGameRunning Then Exit Sub
			inminigame = 0
			StopModeMusic
			PuPlayer.playresume pBackglass
			AddScore gamescore * 100
			'pupDMDDisplay "-", "Video Score^" & FormatNumber(gamescore,0), "" ,3, 0, 10		'3 seconds
				ShowMsg "minigame score", FormatScore(gamescore * 120)
			if gamescore > 4800 then
				if bEB_Video = False then
					bEB_Video = True
					AwardExtraBall(False)
				End if 
			playmedia "minieb.mp4","video-minigame",pBackglass,"cineon",5000,"completemini",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
				Else
			playmedia "minigameend.mp4","video-minigame",pBackglass,"cineon",5000,"completemini",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
			End if 



' Restore active timers
			if tmrModeCountdownSave then 
				tmrModeCountdownSave=False 
				tmrModeCountdown.Enabled=True 
			End if 
			if tmrBeerFrenzySave then 
				tmrBeerFrenzySave=False
				tmrBeerFrenzy.Enabled=True 
			End if 
			if bWizardModeAte then tmrModeCountdown.Enabled = True 


			'PuPlayer.LabelSet pBackglass,"mgscore",FormatNumber(PuPGameScore,0),1,""
			'playmedia "defensescore.mp4","videoscenes",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'msgbox "mini score "&PuPGameScore

			'vpmtimer.addtimer 4000, "clearmgscore '"
			'vpmtimer.addtimer 4000, "checkk1lock '"

			' Increase video mode 
			if PlayerState(CurrentPlayer).VideoModeCount = 0 then				' just in case they dont hit the scoop?
				PlayerState(CurrentPlayer).Specialties( kSpecial_Ramps) = 9
			elseif PlayerState(CurrentPlayer).VideoModeCount= 1 then 
				PlayerState(CurrentPlayer).Specialties( kSpecial_Ramps) = 20
			elseif PlayerState(CurrentPlayer).VideoModeCount = 2 then 
				PlayerState(CurrentPlayer).Specialties( kSpecial_Ramps) = 40
			End if 			
			puPlayer.LabelSet pBackglass,"CollectVal3",	PlayerState(CurrentPlayer).Specialties(kSpecial_Ramps)	,1,""


		End Sub

		sub completemini
			PlaySound "fx_kicker"
			miniend=0
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 1 }"      'this will showsuccess overlay if applicable
			KickerVideoMode.Kick 90, 4
			GiOn
			'resetalllights
			vpmTimer.addtimer 500, "StopVideoMode '"
		end Sub

		sub clearmgscore
			PuPlayer.LabelSet pBackglass,"mgscore","",1,""
		end Sub

		Sub PuPGameTimer_Timer()

			PuPGameTimeout=PuPGameTimeout+1
			if PuPGameTimeout < 10 then 
				'WshShell.AppActivate "PUPShooter"
				WshShell.AppActivate "Visual Pinball Player"
			End if
			PuPGameInfo= PuPlayer.GameUpdate("PUPShooter", 0 , 0 , "")   '0=game over, 1=game running
			'CHECK GAME OVER
			if PuPGameInfo=0 AND PuPGameTimeOut>12 Then  'gameover if more than 5 seconds passed
			   PuPGameTimer.enabled=false 
debug.print "GAME STOPPED " & PuPGameInfo & " " & PuPGameTimeOut
 			   PupGameRunning=False
			   PuPGameScore= PuPlayer.GameUpdate("PUPShooter", 3 , 0 , "\PUPShooter1\gameover.txt")   'grab score from minigame   3=gms 6=godot           
			   'msgbox PuPGameScore  'DO something with the score if its over 0!!!
			   PuPMiniGameEnd(PuPGameScore)  
			End If 
		End Sub






' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Table Init / Objects
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 
 

	' tables variables and Mode init
	Dim LaneBonus
	Dim TargetBonus
	Dim RampBonus
	Dim OrbitBonus
	Dim spinvalue
	Dim PizzaSize
	Dim cheesevalue
	Dim totalronis
	Dim spinCount
	Dim addaballmade
	Dim extraballgiven
	Dim currentpizza
	Dim pzlnum
	Dim pzlnum2
	Dim ptready
	Dim videoready
	Dim ronibumps
	Dim ufolock
	Dim gottips
	Dim tips
	Dim ufoPizzasCollected
	Dim glorycount

	Dim ufolightlock
	Dim doublepoints
	Dim triplepoints
	Dim targetbonuses
	Dim loopbonus
	Dim jps
	Dim canskillshot
	Dim skillvalues
	dim surferComboState
	Dim bResetCurrentGame

	Sub Game_Init() 'called at the start of a new game
		Dim i,j 
		For i = 0 to 4
			pspec(i) = 0
		Next
		TableState_Init(0)
		TableState_Init(1)
		TableState_Init(2)
		TableState_Init(3)

		PuPlayer.LabelShowPage pDMD,1,0,""
		pUpdateScores
		PuPlayer.playlistplayex pDMD,"bgs","background.jpg",0,1  'should be an attract background (no text is displayed)
		ShowMsg "Have a Great Game! ", "-"
		PuPlayer.SetBackground pDMD,1
		bExtraBallWonThisBall = False
		bEB_Video=False
		bEB_Modes=False
		bEB_Eat = False
		EBQueue=0
		CurrentSong
		BeamFlash.opacity = 0
	'	ebflash.opacity = 0
	'	mysteryflash.opacity = 0
		doublepoints = False
		triplepoints = False
		Magnet.MagnetON = False
		addaballgate.open = False
		multimulti.enabled = 0
		loopbonus = 0

		L34.UserValue = 1
		L35.UserValue = 0
		L39.UserValue = 0
		L40.UserValue = 0
		L41.UserValue = 0
		L43.UserValue = 0


		jps = 0
		targetbonuses = 0

		for i = 0 to kCheckSize - 1 
			CheckArray(i,0)=""
			CheckArray(i,1)=""
		Next

		

		iateitall.state = 0
		illuminati.state = 0
		supreme.state = 0


' Switch to PlayerStates
'		For i = 0 to 4
'			SkillshotValue(i) = 100000 ' increases by 100000 each time it is collected
'			pzlnum(i) = 0
'			ronibumps(i) = 0
'			totalronis(i) = 0
'			ufolock(i) = 0
'			ufoPizzasCollected(i) = 0
'			extraballgiven(i) = false
'			ResetPlayerIngredStates i		'' Reset ingredient states for this player
'			ufo1lon(i) = False
'			ufo2lon(i) = False
'			ufo3lon(i) = False
'			ufo4lon(i) = False
'			ufo5lon(i) = False
'			addaballmade(i) = False
'			ptready(i)= False
'			pz1l1on(i) = 0
'			pz1l2on(i) = 0
'			pz1l3on(i) = 0
'			pz1l4on(i) = 0
'			pz2l1on(i) = 0
'			pz2l2on(i) = 0
'			pz2l3on(i) = 0
'			pz2l4on(i) = 0
'			pz2l5on(i) = 0
'			pz2l6on(i) = 0
'			pz2l7on(i) = 0
'			pz2l8on(i) = 0
'			pz3l1on(i) = 0
'			pz3l2on(i) = 0
'			pz3l3on(i) = 0
'			pz3l4on(i) = 0
'			pz3l5on(i) = 0
'			pz3l6on(i) = 0
'			pz3l7on(i) = 0
'			pz3l8on(i) = 0
'			pz3l9on(i) = 0
'			pz3l10on(i) = 0
'			pz3l11on(i) = 0
'			pz3l12on(i) = 0
'			pz4l1on(i) = 0
'			pz4l2on(i) = 0
'			pz4l3on(i) = 0
'			pz4l4on(i) = 0
'			pz4l5on(i) = 0
'			pz4l6on(i) = 0
'			pz4l7on(i) = 0
'			pz4l8on(i) = 0
'			pz4l9on(i) = 0
'			pz4l10on(i) = 0
'			pz4l11on(i) = 0
'			pz4l12on(i) = 0
'			pz4l13on(i) = 0
'			pz4l14on(i) = 0
'			pz4l15on(i) = 0
'			pz4l16on(i) = 0
'			tipjartlon(i) = 0
'			tipjarilon(i) = 0
'			tipjarplon(i) = 0
'			tipjarjlon(i) = 0
'			tipjaralon(i) = 0
'			tipjarrlon(i) = 0
'			tipped1on(i) = false
'			tipped2on(i) = false
'			tipped3on(i) = false
'			tipped4on(i) = false
'			gottips(i) = False
'			Specialties(i,0) = 72	' spins
'			Specialties(i,1) = 2	' Locks
'			Specialties(i,2) = 9	' Ramps
'			Specialties(i,3) = 6	' Multipliers
'			Specialties(i,4) = 8	' slices Eaten
'			Specialties(i,5) = 20	' Loop-de-Loops
'			Specialties(i,6) = 6	' Tip Bumper
'
'			puPlayer.LabelSet pBackglass,"CollectVal1",	Specialties(CurrentPlayer,0)	,1,""
'			puPlayer.LabelSet pBackglass,"CollectVal2",	Specialties(CurrentPlayer,1)	,1,""
'			puPlayer.LabelSet pBackglass,"CollectVal3",	Specialties(CurrentPlayer,2)	,1,""
'			puPlayer.LabelSet pBackglass,"CollectVal4",	Specialties(CurrentPlayer,3)	,1,""
'			puPlayer.LabelSet pBackglass,"CollectVal5",	Specialties(CurrentPlayer,4)	,1,""
'			puPlayer.LabelSet pBackglass,"CollectVal6",	Specialties(CurrentPlayer,5)	,1,""
'			puPlayer.LabelSet pBackglass,"CollectVal7",	Specialties(CurrentPlayer,6)	,1,""
'
'			SMode(i) = -1			' CurrentMode - Default to No mode
'			VideoModeCount(i) = 0	' Number of video mode starts 
'			For j = 0 to 5 
'				SModePercent(i, j) = 0
'				SModeProgress(i, j) = 0
'			next 
'			for j = 0 to 8
'				pizzaPartyProgress(i, j) = 0
'			next 
'			ecwheel1on(i) = False
'			ecwheel2on(i) = False
'			ecwheel3on(i) = False
'			ecwheel4on(i) = False
'			aablighton(i) = False
'			ebnowlit(i) = False
'			ismysteryon(i) = False
'			currentpizza(i) = 0
'			tips(i) = 0
'			pizzasize(i) = 1
'			glorycount(i) = 0
'			cheesevalue(i) = 0
'		Next
		glorycount=0
		ufoPizzasCollected=0
		gottips=False
		tips=0
		ufolock=0
		ronibumps=0
		ptready=False 
		videoready=False 
		pzlnum = 0
		pzlnum2 = 0
		extraballgiven = False 
		addaballmade = False
		currentpizza =0
		pizzasize = 1
		cheesevalue = 0
		totalronis = 0
		spinCount = 0

		puPlayer.LabelSet pBackglass,"CollectVal1", PlayerState(CurrentPlayer).Specialties(0)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal2",	PlayerState(CurrentPlayer).Specialties(1)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal3",	PlayerState(CurrentPlayer).Specialties(2)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal4",	PlayerState(CurrentPlayer).Specialties(3)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal5",	PlayerState(CurrentPlayer).Specialties(4)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal6",	PlayerState(CurrentPlayer).Specialties(5)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal7",	PlayerState(CurrentPlayer).Specialties(6)	,1,""

		For i = 0 to 5 
			BonusTotals(i)=0
		Next 

		' Blank Lights 
		Dim bulb
		For each bulb in aLights
			bulb.State = 0
		Next
		'playerlights

		cheese4l.opacity = 0
		cheese3l.opacity = 0
		cheese2ll.opacity = 0
		cheese1l.opacity = 0
		

		ufolightlock = false
		bWizardModeSupreme = False 
		bWizardModeSupremeReady = False 
		bWizardModeSupremeFinished = False 

		bWizardModeIllum = False 
		bWizardModeIllumReady = False 
		bWizardModeIllumFinished = False 

		bWizardModeAte = False 
		bWizardModeAteReady = False 
		bWizardModeAteFinished = False 

		' Reset all the player states
		PlayerState(0).Reset
		PlayerState(1).Reset
		PlayerState(2).Reset
		PlayerState(3).Reset
		PlayerState(0).Save	' Save new state 
		PlayerState(1).Save	' Save new state 
		PlayerState(2).Save	' Save new state 
		PlayerState(3).Save	' Save new state 

		UpdateNumberPlayers
		' Test Hish score entry
		'Score(0)=10000
		'CheckHighscore
		
		ShowRule(kRule_PizzaTime)
		'ShowMsg "Superjackpot " & FormatScore(500000), "More Info", 2000
		'pupDMDDisplay "-", "BONUS " & " x " & 4 & "^"  & FormatScore(2003404), "" ,3, 0, 10		'3 seconds

	End Sub

	Sub StopEndOfBallMode()              'this sub is called after the last ball is drained
		ResetSkillShotTimer_Timer
	End Sub

	Sub ResetNewBallVariables()          'reset variables for a new ball or player

	End Sub

	Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
		if ufolightlock = False  then	' Targets dont carry over but mkaming the lock carry over 
			BeamFlash.opacity = 0
			ufolocklight.state = 0
		Else
			ufo1l.state = 1
			ufo2l.state = 1
			ufo3l.state = 1
			ufo4l.state = 1
			ufo5l.state = 1
			checkufos
		End if 

		If pizzaorder.state = 2 Then
			pizzaorder.state = 0
		End If
	'	mysteryflash.opacity = 0

		' Reset playfield states besed on this player 

debug.print "PizzaState:" & pizzaisopen & " " & ptready
		If pizzaisopen and ptready=False Then pizzaopen														' Close the Pizza
		If pizzaisopen=False and ptready=True Then pizzaopen														' Close the Pizza

debug.print "videoready: " & videoready
		if KickerVideoMode.Enabled = False and videoready then StartVideoMode		' Open Video mode if it is ready
		if KickerVideoMode.Enabled = True and videoready=false then StopVideoMode	' Open Video mode if it is ready
		reseteveryballlights
		if currentpizza=0 then 		' Setup Pizza time if 
			pizzaspin
		else
			pizzaspinnerSet			' Just restore based on saved values 
		End If 
		pizzatype
		updatePizzaHitsLeft
		tipoff
		pizzasizemodes


	End Sub

	Sub TurnOffPlayfieldLights()
		Dim a
		For each a in aLights
			a.State = 0
		Next
	End Sub

	Sub UpdateSkillShot() 
	'	LightSeqSkillshot.Play SeqAllOff
	End Sub

	Sub SkillshotOff_Hit 
debug.print "Skillshot-Trigger"
		If bSkillShotReady Then
			ResetSkillShotTimer_Timer
		End If
		canskillshot = False
		skillarrow.state = 0
	End Sub

	Sub tmrResetSuperSkillshot_Timer 
debug.print "SuperSkill Stop"
		bSuperSkillShotEnabled=False 
		LightSeqSuperSkill.StopPlay
		Gate7.Open=False 
		tmrResetSuperSkillshot.Enabled = False 
	End Sub 

	Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
		pizzatype
		ResetSkillShotTimer.Enabled = False
		bSkillShotReady = False
		'LightSeqSkillshot.StopPlay
	End Sub

	Sub resetalllights
		Dim bulb
		For each bulb in aIngredLights		' Clear ingredient lights before we set them back up
			bulb.State = 0
		Next
		For each bulb in aTargetTitles
			bulb.State = 0
		Next
	End Sub

	Sub reseteveryballlights
		Dim bulb
		'ebflash.opacity = 0
		If pizzasize = 3 Then
			If extraballgiven= False Then
				'extraballlight.state = 2
				ShowMsg "Extra Ball", ""
				'pupDMDDisplay "-", "ExtraBall", "" ,3, 0, 10		'3 seconds
				'ebflash.opacity = 2000
			End If
		End If
		addaballgate.open = False
		'addflash.opacity = 0
		
		lolights = 0
		rolights = 0
		bonuses = 0
		loopbonus = 0
		skillvalues = 0
		canskillshot = True
		targetbonuses = 0
		roready = False
		For each bulb in reseteveryball			' Reset all target Bonus and loops 
			bulb.State = 0
		Next
	End Sub

	Sub resetingredlights
		ResetPlayerIngredStates CurrentPlayer
		Dim bulb
		For each bulb in aIngredLights
			bulb.State = 0
		Next
	End Sub 

	Sub StartCountdown(numSeconds)
		bCallout1=False
		bCallout2=False 
		ModeTimerInc = 32/numSeconds
		puPlayer.LabelSet pBackglass,"CheckV", numSeconds, 1,"{'mt':2,'color':33023}"
		tmrModeCountdown.Interval=1000
		tmrModeCountdown.UserValue = 32
		tmrModeCountdown.Enabled = True
	End Sub 

	Sub StopCountdown()
		puPlayer.LabelSet pBackglass,"CheckV", myVersion, 1,"{'mt':2,'color':0}"
		'playmedia "","audio-bummer",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		puPlayer.LabelSet pBackglass,"ModeTmr", "PuPOverlays\\clear.png", 0,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"
		tmrModeCountdown.Enabled = False 
		bCallout1=False
		bCallout2=False 
	End Sub 

	Sub ResetPlayerIngredStates(index)
debug.print "Resettingredlights"
		PlayerState(currentplayer).pinel1on = False
		PlayerState(currentplayer).pinel2on = False
		PlayerState(currentplayer).olivel1on = False
		PlayerState(currentplayer).olivel2on = False
		PlayerState(currentplayer).mushl1on = False
		PlayerState(currentplayer).mushl2on = False
		PlayerState(currentplayer).mushl3on = False
		PlayerState(currentplayer).sausl1on = False
		PlayerState(currentplayer).sausl2on = False
		PlayerState(currentplayer).sausl3on = False
		PlayerState(currentplayer).pepl1on = False
		PlayerState(currentplayer).pepl2on = False
		PlayerState(currentplayer).pepl3on = False
		PlayerState(currentplayer).baconl1on = False
		PlayerState(currentplayer).baconl2on = False
		PlayerState(currentplayer).baconl3on = False
		PlayerState(currentplayer).fish1on = False
		PlayerState(currentplayer).fish2on = False
		PlayerState(currentplayer).fish3on = False
		PlayerState(currentplayer).roni1on = False
		PlayerState(currentplayer).roni2on = False
		PlayerState(currentplayer).roni3on = False
		cheesevalue = 0
		totalronis = 0
	End Sub

	Sub tmrRovingShot_Timer 
		tmrRovingShot.UserValue = tmrRovingShot.UserValue+1
		select case tmrRovingShot.UserValue 
			case 1: 
				SSetLightColor kStack_Pri1, peptitle, modeColor, 0
				SSetLightColor kStack_Pri1, pepl1, modeColor, 0
				SSetLightColor kStack_Pri1, pepl2, modeColor, 0
				SSetLightColor kStack_Pri1, pepl3, modeColor, 0
				SSetLightColor kStack_Pri1, pinetitle, modeColor, 2
				SSetLightColor kStack_Pri1, pinel1, modeColor, 2
				SSetLightColor kStack_Pri1, pinel2, modeColor, 2
			case 2: 
				SSetLightColor kStack_Pri1, pinel1, modeColor, 0
				SSetLightColor kStack_Pri1, pinel2, modeColor, 0
				SSetLightColor kStack_Pri1, pinetitle, modeColor, 0
				SSetLightColor kStack_Pri1, bacontitle, modeColor, 2
				SSetLightColor kStack_Pri1, baconl1, modeColor, 2
				SSetLightColor kStack_Pri1, baconl2, modeColor, 2
			case 3: 
				SSetLightColor kStack_Pri1, baconl1, modeColor, 0
				SSetLightColor kStack_Pri1, baconl2, modeColor, 0
				SSetLightColor kStack_Pri1, bacontitle, modeColor, 0
				SSetLightColor kStack_Pri1, olivetitle, modeColor, 2
				SSetLightColor kStack_Pri1, olivel1, modeColor, 2
				SSetLightColor kStack_Pri1, olivel2, modeColor, 2
			case 4: 
				SSetLightColor kStack_Pri1, olivel1, modeColor, 0
				SSetLightColor kStack_Pri1, olivel2, modeColor, 0
				SSetLightColor kStack_Pri1, olivetitle, modeColor, 0
				SSetLightColor kStack_Pri1, mushtitle, modeColor, 2
				SSetLightColor kStack_Pri1, mushl1, modeColor, 2
				SSetLightColor kStack_Pri1, mushl2, modeColor, 2
				SSetLightColor kStack_Pri1, mushl3, modeColor, 2
			case 5: 
				SSetLightColor kStack_Pri1, mushl1, modeColor, 0
				SSetLightColor kStack_Pri1, mushl2, modeColor, 0
				SSetLightColor kStack_Pri1, mushl3, modeColor, 0
				SSetLightColor kStack_Pri1, mushtitle, modeColor, 0
				SSetLightColor kStack_Pri1, saustitle, modeColor, 2
				SSetLightColor kStack_Pri1, sausl1, modeColor, 2
				SSetLightColor kStack_Pri1, sausl2, modeColor, 2
				SSetLightColor kStack_Pri1, sausl3, modeColor, 2
			case 6: 
				SSetLightColor kStack_Pri1, sausl1, modeColor, 0
				SSetLightColor kStack_Pri1, sausl2, modeColor, 0
				SSetLightColor kStack_Pri1, sausl3, modeColor, 0
				SSetLightColor kStack_Pri1, saustitle, modeColor, 0
				SSetLightColor kStack_Pri1, peptitle, modeColor, 2
				SSetLightColor kStack_Pri1, pepl1, modeColor, 2
				SSetLightColor kStack_Pri1, pepl2, modeColor, 2
				SSetLightColor kStack_Pri1, pepl3, modeColor, 2
				tmrRovingShot.UserValue=0
		End Select 
	End Sub 

	Dim bCallout1
	Dim bCallout2
	bCallout1=False
	bCallout2=False 

	sub modelightstime_timer
		keepmodelights(PlayerState(CurrentPlayer).SMode)
		debug.print "relight " & PlayerState(CurrentPlayer).SMode
	end sub



	Sub tmrModeCountdown_Timer()


Debug.print "UserVal:" & tmrModeCountdown.UserValue & " " & ModeTimerInc & " " & (33 - CInt(tmrModeCountdown.UserValue))
		tmrModeCountdown.UserValue = tmrModeCountdown.UserValue-ModeTimerInc

		if tmrModeCountdown.UserValue >=0 then 
			puPlayer.LabelSet pBackglass,"CheckV", CInt(tmrModeCountdown.UserValue/ModeTimerInc), 1,"{'mt':2,'color':33023}"
			'puPlayer.LabelSet pBackglass,"ModeTmr", "Mode:"	& tmrModeCountdown.UserValue	,1,""
			puPlayer.LabelSet pBackglass,"ModeTmr", "PuPOverlays\\" & 33 - CInt(tmrModeCountdown.UserValue) & ".png", 1,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"
		End If 

		if tmrModeCountdown.UserValue/ModeTimerInc < 15 and bCallout1=False then
			bCallout1=True
			playmedia "","audio-hurryup",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'PlaySound "so_timer"
		End if 


		if tmrModeCountdown.UserValue/ModeTimerInc < 5 and bCallout2=False then
			bCallout2=True
			playmedia "","audio-countdown",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'PlaySound "so_timer"
		End if 

		if tmrModeCountdown.UserValue/ModeTimerInc <= -2 then 	' 2 second buffer
			StopCountdown
			if bWizardModeAte then 
debug.print "Ate Timer Expired"
				StopWizardAte
			elseif bWizardModeIllum then 
debug.print "Illum Timer Expired"
				StopWizardIllum
			else
				StopSpecialMode
			End if 
		End if 
	End Sub 


	sub modeintro
		if bMultiBallMode=true Then
			startSpecialMode
		Else
			playmedia "intro.mp3","audio-modepick",pCallouts,"",4000,"",1,1 
			playmedia "select.jpg","modes",pBackglass,"cineon",2500,"modepick",1,2  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			modepicktime.enabled=true
		end If
	end Sub

	dim rst:rst=0
	dim mcycle:mcycle=0
	dim modepickin:modepickin=0
	sub modepick
		DisableFlippers=True
		modepickin=1
		select Case mcycle
			case 0
				playmedia "pizzaparty.jpg","modes",pBackglass,"",25000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				NextMode = 0
				l43.state=rst
				rst = L34.state
				L34.state=2
			case 1
				playmedia "timebomb.jpg","modes",pBackglass,"",25000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				NextMode = 1
				L34.state=rst
				rst=L35.state
				L35.state=2
			case 2
				playmedia "deathorglory.jpg","modes",pBackglass,"",25000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				NextMode = 2
				L35.state=rst
				rst=l39.state
				L39.state=2
			case 3
				playmedia "threepizza.jpg","modes",pBackglass,"",25000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				NextMode = 3
				L39.state=rst
				rst=l40.state
				L40.state=2
			case 4
				playmedia "badtown.jpg","modes",pBackglass,"",25000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				NextMode = 4
				L40.state=rst
				rst=l41.state
				L41.state=2
			case 5
				playmedia "pizzalove.jpg","modes",pBackglass,"",25000,"",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				NextMode = 5
				L41.state=rst
				rst=L43.state
				L43.state=2
		end Select		
	end Sub



	dim mtim:mtim=0
	sub modepicktime_timer
		mtim=mtim+1
		select case mtim
			case 15
				playmedia "pick.mp3","audio-modepick",pCallouts,"",2000,"",1,1 
			case 20
				playmedia "","audio-countdown",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			case 25
				startSpecialMode()
				modepicktime.enabled=False
				mtim=0
		end Select
	end Sub

	dim inamode:inamode=0

	dim modeColor
	Sub startSpecialMode()
		inamode=1
		modepicktime.enabled=False
		mtim=0
		DisableFlippers=False
		modepickin=0
		if ptgi + ufogi=0 Then
			gicolor purple
		end if
		modegi=1
		strModeSong=""
		PlayerState(CurrentPlayer).SMode = NextMode			' Grab the current cycled mode 
		modelightstime.enabled = true
		Select case PlayerState(CurrentPlayer).SMode
			case 0:		' Pizza Party
				keepmodelights(0)
				SSetLightColor kStack_Pri1, ecl2, purple, 2
				SSetLightColor kStack_Pri1, leftarrow1, purple, 2
				SSetLightColor kStack_Pri1, pepperonititle, purple, 2
				SSetLightColor kStack_Pri1, leftarrow, purple, 2
				SSetLightColor kStack_Pri1, pinetitle, purple, 2
				SSetLightColor kStack_Pri1, pinel1, purple, 2
				SSetLightColor kStack_Pri1, pinel2, purple, 2
				SSetLightColor kStack_Pri1, bacontitle, purple, 2
				SSetLightColor kStack_Pri1, baconl1, purple, 2
				SSetLightColor kStack_Pri1, baconl2, purple, 2
				SSetLightColor kStack_Pri1, olivetitle, purple, 2
				SSetLightColor kStack_Pri1, olivel1, purple, 2
				SSetLightColor kStack_Pri1, olivel2, purple, 2
				SSetLightColor kStack_Pri1, mushtitle, purple, 2
				SSetLightColor kStack_Pri1, mushl1, purple, 2
				SSetLightColor kStack_Pri1, mushl2, purple, 2
				SSetLightColor kStack_Pri1, mushl3, purple, 2
				SSetLightColor kStack_Pri1, saustitle, purple, 2
				SSetLightColor kStack_Pri1, sausl1, purple, 2
				SSetLightColor kStack_Pri1, sausl2, purple, 2
				SSetLightColor kStack_Pri1, sausl3, purple, 2
				SSetLightColor kStack_Pri1, peptitle, purple, 2
				SSetLightColor kStack_Pri1, pepl1, purple, 2
				SSetLightColor kStack_Pri1, pepl2, purple, 2
				SSetLightColor kStack_Pri1, pepl3, purple, 2
				SSetLightColor kStack_Pri1, anchtitle, purple, 2
				SSetLightColor kStack_Pri1, fish1, purple, 2
				SSetLightColor kStack_Pri1, fish2, purple, 2
				SSetLightColor kStack_Pri1, fish3, purple, 2
				ShowRule(kRule_PizzaParty)
				playmedia "","crust-pizzaparty",pBackglass,"cineon",3000,"startkick",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				ShowMsg "clear table shots", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "mode:pizza party", "" 'FormatScore(BonusPoints(CurrentPlayer))
				StartCountdown 90					' 40 second timer
				strModeSong = "King Khan & BBQ Show - Animal Party.mp3"
				backlamp "flash"

			case 1:		' Time Bomb
				keepmodelights(1)
				ShowRule(kRule_TimeBomb)
				L35.state = 2
				StackState(kStack_Pri1).Enable()
				SSetLightColor kStack_Pri1, pinetitle, modeColor, 2
				SSetLightColor kStack_Pri1, pinel1, modeColor, 2
				SSetLightColor kStack_Pri1, pinel2, modeColor, 2
				modeColor = purple
				tmrRovingShot.UserValue = 0
				tmrRovingShot.Interval = 2500
				tmrRovingShot.Enabled = True 
					playmedia "","crust-timebomb",pBackglass,"cineon",1500,"startkick",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "mode-complete.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				DOF 342, DOFOn 'MX-Timebomb
				DOF 442, DOFPulse 'RGB
				ShowMsg "hit roving shot", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "mode:time bomb", "" 'FormatScore(BonusPoints(CurrentPlayer))
				strModeSong = "Rancid - Time Bomb.mp3"

				backlamp "flash"
			case 2:		' Death or Glory 
				StackState(kStack_Pri1).Enable()
				keepmodelights(2)
				ShowRule(kRule_DeathOrGlory)
				L39.state = 2
					playmedia "","crust-deathorglory",pBackglass,"cineon",4300,"startkick",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "mode-complete.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				DOF 343, DOFOn 'MX-deathorglory
				DOF 442, DOFPulse 'RGB
				ShowMsg "hit death or glory", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "mode:death or glory", "" 'FormatScore(BonusPoints(CurrentPlayer))
				strModeSong = "The Clash - Death or Glory .mp3"

				backlamp "flash"
			case 3:		' 3 Pizza Rhumba
				StackState(kStack_Pri1).Enable()
				keepmodelights(3)
				SSetLightColor kStack_Pri1, anchtitle, purple, 2
				SSetLightColor kStack_Pri1, rightarrow, purple, 2
				SSetLightColor kStack_Pri1, ropizza1, purple, 2
				SSetLightColor kStack_Pri1, ropizza2, purple, 2
				SSetLightColor kStack_Pri1, ropizza3, purple, 2
				SSetLightColor kStack_Pri1, skillarrow, purple, 2
				SSetLightColor kStack_Pri1, jackpotl1, purple, 2
				rhumbatime.enabled=True
				ShowRule(kRule_Rhumba)
				L40.state = 2
				surferComboState=0
				SSetLightColor kStack_Pri1, anchtitle, purple, 2
				StartCountdown 90					' 40 second timer
					playmedia "","crust-threepizza",pBackglass,"cineon",2200,"startkick",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "mode-complete.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				DOF 344, DOFOn 'MX-Pizzarhumba
				DOF 444, DOFPulse 'RGB
				ShowMsg "then hit ramp", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "hit right orbit", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "mode:3 pizza rhumba", "" 'FormatScore(BonusPoints(CurrentPlayer))			
				strModeSong = "Wire - Three Girl Rhumba.mp3"
				backlamp "flash"

			case 4:		' Bad Town
				keepmodelights(4)
				ShowRule(kRule_BadTown)
				L41.state = 2
				Light109.State = 2
				roni1.state = 2
				roni2.state = 2
				roni3.state = 2
				StackState(kStack_Pri1).Enable()
					playmedia "","crust-badtown",pBackglass,"cineon",2500,"startkick",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "","video-jackpot",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				DOF 345, DOFOn 'MX-Badtown
				DOF 445, DOFPulse 'RGB
				StartCountdown 90					' 60 second timer
				ShowMsg "smash shop", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "hit bumpers to", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "mode:bad town", "" 'FormatScore(BonusPoints(CurrentPlayer))
				strModeSong = "Bad Town - OPERATION IVY.mp3"
				backlamp "flash"

			case 5:		' Pizza your love 
				keepmodelights(5)
				ShowRule(kRule_PizzaLove)
				StackState(kStack_Pri1).Enable()
				L43.state = 2
				SSetLightColor kStack_Pri1, jackpotl1, purple, 2
				SSetLightColor kStack_Pri1, skillarrow, purple, 2
				SSetLightColor kStack_Pri1, ufo3l, purple, 2
				SSetLightColor kStack_Pri1, ufo4l, purple, 2
				StartCountdown 90					' 40 second timer
					playmedia "","crust-pizzayourlove",pBackglass,"cineon",2000,"startkick",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "mode-complete.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				DOF 346, DOFOn 'MX-Pizzayourlove
				DOF 446, DOFPulse 'RGB
				ShowMsg "hit upper ramp", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "mode: pizza your love", "" 'FormatScore(BonusPoints(CurrentPlayer))
				strModeSong = "Ramones - I Wanna Be Your Boyfriend.mp3"
				backlamp "flash"
		End Select 

		if pizzamulti = False and strModeSong <> "" then 	' In pizza time dont kill music 
			if PlayerState(CurrentPlayer).SMode <> -1 and strModeSong <> "" then 	' We are in a mode play mode music
				PlayModeMusic strModeSong
			End if 
		End if

		modelight.state = 0 
	End Sub 

	dim rhum:rhum=0
	sub rhumbatime_timer
		rhum=rhum+1
		select case rhum
			case 1
				skillarrow.state = 0	
				jackpotl1.state = 0	
				anchtitle.state = 1
			case 2
				anchtitle.state = 1	
				rightarrow.state = 1
			case 3
				anchtitle.state = 0
				rightarrow.state = 1
				ropizza1.state = 1		
			case 4
				rightarrow.state = 0
				ropizza1.state = 1	
				ropizza2.state = 1	
			case 5
				ropizza1.state = 0
				ropizza2.state = 1	
				ropizza3.state = 1
			case 6
				ropizza2.state = 0
				ropizza3.state = 1	
				skillarrow.state = 1
			case 7
				ropizza3.state = 0
				skillarrow.state = 1	
				jackpotl1.state = 1	
			case 8
				skillarrow.state = 0	
				jackpotl1.state = 0
			case 9
				skillarrow.state = 1	
				jackpotl1.state = 1	
			case 10
				skillarrow.state = 0	
				jackpotl1.state = 0	
			case 11
				skillarrow.state = 1	
				jackpotl1.state = 1
				rhum=0		
		end select
	end Sub



	Sub keepmodelights(modenum)
		if ptgi + ufogi=0 Then
			gicolor purple
		end if
		select case modenum
			case 0:		' Pizza Party 
				L34.state = 2
				StackState(kStack_Pri1).Enable()

			case 1:		' Time Bomb 
				L35.state = 2
				StackState(kStack_Pri1).Enable()
				modeColor = purple
			case 2:		' Death or Glory 
				L39.state = 2
				StackState(kStack_Pri1).Enable()
				SSetLightColor kStack_Pri1, L13, purple, 2
				SSetLightColor kStack_Pri1, L26, purple, 2
				SSetLightColor kStack_Pri1, L27, purple, 2
				SSetLightColor kStack_Pri1, L28, purple, 2
			case 3:		' 3 Pizza Rhumba
				L40.state = 2
				StackState(kStack_Pri1).Enable()

			case 4:		' Bad Town
				StackState(kStack_Pri1).Enable()
				L41.state = 2
				Light109.State = 2
				SSetLightColor kStack_Pri1, bumps1, purple, 2
				SSetLightColor kStack_Pri1, bumps2, purple, 2
				SSetLightColor kStack_Pri1, roni1, purple, 2
				SSetLightColor kStack_Pri1, roni2, purple, 2
				SSetLightColor kStack_Pri1, roni3, purple, 2
				SSetLightColor kStack_Pri1, pepperonititle, purple, 2
				SSetLightColor kStack_Pri1, anchtitle, purple, 2
				SSetLightColor kStack_Pri1, leftarrow, purple, 2
				SSetLightColor kStack_Pri1, rightarrow, purple, 2

			case 5:		' Pizza your love 
				StackState(kStack_Pri1).Enable()
				L43.state = 2
				SSetLightColor kStack_Pri1, jackpotl1, purple, 2
				SSetLightColor kStack_Pri1, skillarrow, purple, 2
				SSetLightColor kStack_Pri1, ufo3l, purple, 2
				SSetLightColor kStack_Pri1, ufo4l, purple, 2
		end Select
	end Sub

	Sub clearmodelights(modenum)
		if ptgi + ufogi=0 Then
			gicolor purple
		end if
		select case modenum
			case 0:		' Pizza Party 

			case 1:		' Time Bomb 
			case 2:		' Death or Glory 
				SSetLightColor kStack_Pri1, L13, purple, 0
				SSetLightColor kStack_Pri1, L26, purple, 0
				SSetLightColor kStack_Pri1, L27, purple, 0
				SSetLightColor kStack_Pri1, L28, purple, 0
			case 3:		' 3 Pizza Rhumba

			case 4:		' Bad Town
				Light109.State = 0
				SSetLightColor kStack_Pri1, bumps1, purple, 0
				SSetLightColor kStack_Pri1, bumps2, purple, 0
				SSetLightColor kStack_Pri1, roni1, purple, 0
				SSetLightColor kStack_Pri1, roni2, purple, 0
				SSetLightColor kStack_Pri1, roni3, purple, 0
				SSetLightColor kStack_Pri1, pepperonititle, purple, 0
				SSetLightColor kStack_Pri1, anchtitle, purple, 0
				SSetLightColor kStack_Pri1, leftarrow, purple, 0
				SSetLightColor kStack_Pri1, rightarrow, purple, 0

			case 5:		' Pizza your love 
				SSetLightColor kStack_Pri1, jackpotl1, purple, 0
				SSetLightColor kStack_Pri1, skillarrow, purple, 0
				SSetLightColor kStack_Pri1, ufo3l, purple, 0
				SSetLightColor kStack_Pri1, ufo4l, purple, 0
		end Select
	end Sub


	Sub checkModeProgress(trigger_name)
		Dim i, a
		Dim bValidHit
		Dim ModesComplete
		Dim bModeComplete:bModeComplete=False
		Dim CurrentMode
		dim triggerIndex:triggerIndex = -1
		dim saveCnt


		bValidHit=False
		CurrentMode = PlayerState(CurrentPlayer).SMode
debug.print "CheckModeProgress " & trigger_name & " " & CurrentMode

		if bWizardModeSupreme then
			For each a in aTargetTitles				' Need to hit flashing targets
				if a.name = trigger_name and StackState(kStack_Pri2).TargetState(a) <> 0 then 
					select case trigger_name
						case "ecl2"	: triggerIndex=0
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 3 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "pepperonititle": triggerIndex=1
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 3 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "pinetitle": triggerIndex=2
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 2 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "bacontitle"	: triggerIndex=3
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 2 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "olivetitle"	: triggerIndex=4
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 2 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "mushtitle"	: triggerIndex=5
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 3 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "saustitle"	: triggerIndex=6
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 3 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "peptitle"		: triggerIndex=7
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 3 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
						case "anchtitle"	: triggerIndex=8
							WizardModeSupremeProgress(triggerIndex)=WizardModeSupremeProgress(triggerIndex)+1
							if WizardModeSupremeProgress(triggerIndex) = 3 then 
								WizardModeSupremeProgress(triggerIndex)=99	' Done
								SSetLightColor kStack_Pri2, a, green, 0
								AddScore 80000
								backlamp "run"
								lightrun white,down,1
								flasherspop white,"top" 'left,right,bottom,rightkick,top,crazy
							End If 
					End Select
					saveCnt = 0
					bValidHit = True		' See if we are done 
					For i = 0 to 8
						If WizardModeSupremeProgress(i)<>99 then 
							bValidHit=False	
						else 
							saveCnt=SaveCnt+1
						End If 
					Next 
					if saveCnt>=4 and bWizardModeSupremeMB1=False then 
						bWizardModeSupremeMB1=True 
						AddMultiball 1
					End if 
					if saveCnt>=8 and bWizardModeSupremeMB2=False then 
						bWizardModeSupremeMB2=True 
						AddMultiball 1
					End if 
					if bValidHit then 	' We are Done 
Debug.print "Wizard Supreme Complete"
						AddScore 5000000
								backlamp "flash"
								lightrun white,randoms,4
								flasherspop white,"crazy" 'left,right,bottom,rightkick,top,crazy
						ShowMsg "Supreme Finished", FormatScore(5000000)
						'pupDMDDisplay "-", "SUPREME FINISHED", "" ,3, 0, 10		'3 seconds
						bWizardModeSupremeFinished = True
						StopWizardSupreme
						PlaySound "bell"
					End If 
				End if 
			Next 
		End if 

	
		Select case CurrentMode
			case 0:	' Pizza Party 
				For each a in aTargetTitles				' Need to hit flashing targets
					if a.name = trigger_name and StackState(kStack_Pri1).TargetState(a) <> 0 then 
						select case trigger_name
							case "ecl2"				: triggerIndex=0
							case "pepperonititle"	: triggerIndex=1
							case "pinetitle"		: triggerIndex=2
							case "bacontitle"		: triggerIndex=3
							case "olivetitle"		: triggerIndex=4
							case "mushtitle"		: triggerIndex=5
							case "saustitle"		: triggerIndex=6
							case "peptitle"			: triggerIndex=7
							case "anchtitle"		: triggerIndex=8
						'	case "jackpotl1"	: triggerIndex=9
						'	case "skillarrow"	: triggerIndex=10
						'	case "ufo4l"		: triggerIndex=11
						'	case "ufo3l"		: triggerIndex=12
						'	case "bumps1"		: triggerIndex=13
						'	case "roni1"		: triggerIndex=14
						'	case "roni2"		: triggerIndex=15
						'	case "roni3"		: triggerIndex=16
						'	case "bumps2"		: triggerIndex=17
						'	case "leftarrow"	: triggerIndex=18
						'	case "rightarrow"	: triggerIndex=19
						'	case "ropizza1"		: triggerIndex=20
						'	case "ropizza2"		: triggerIndex=21
						'	case "ropizza3"		: triggerIndex=22
						'	case "L13"		: triggerIndex=23
						'	case "L26"		: triggerIndex=24
						'	case "L27"		: triggerIndex=25
						'	case "L28"		: triggerIndex=26
						'	case "leftarrow1"			: triggerIndex=27
						'	case "pinel1"			: triggerIndex=28
						'	case "pinel2"			: triggerIndex=29
						'	case "baconl1"			: triggerIndex=30
						'	case "baconl2"			: triggerIndex=31
						'	case "olivel1"			: triggerIndex=32
						'	case "olivel2"			: triggerIndex=33
						'	case "mushl1"			: triggerIndex=34
						'	case "mushl2"			: triggerIndex=35
						'	case "mushl3"			: triggerIndex=36
						'	case "sausl1"			: triggerIndex=37
						'	case "sausl2"			: triggerIndex=38
						'	case "sausl3"			: triggerIndex=39
						'	case "pepl1"			: triggerIndex=40
						'	case "pepl2"			: triggerIndex=41
						'	case "pepl3"			: triggerIndex=42
						'	case "fish1"			: triggerIndex=43
						'	case "fish2"			: triggerIndex=44
						'	case "fish3"			: triggerIndex=45

						End Select 
						if triggerIndex <> -1 Then
							if PlayerState(CurrentPlayer).pizzaPartyProgress(triggerIndex) = 0 then
								SSetLightColor kStack_Pri1, a, purple, 0
								select case trigger_name
									case "ecl2"
										SSetLightColor kStack_Pri1, ecl2, purple, 0
										SSetLightColor kStack_Pri1, leftarrow1, purple, 0
									case "pepperonititle"
										SSetLightColor kStack_Pri1, pepperonititle, purple, 0
										SSetLightColor kStack_Pri1, leftarrow, purple, 0
									case "pinetitle"
										SSetLightColor kStack_Pri1, pinetitle, purple, 0
										SSetLightColor kStack_Pri1, pinel1, purple, 0
										SSetLightColor kStack_Pri1, pinel2, purple, 0
									case "bacontitle"
										SSetLightColor kStack_Pri1, bacontitle, purple, 0
										SSetLightColor kStack_Pri1, baconl1, purple, 0
										SSetLightColor kStack_Pri1, baconl2, purple, 0
									case "olivetitle"
										SSetLightColor kStack_Pri1, olivetitle, purple, 0
										SSetLightColor kStack_Pri1, olivel1, purple, 0
										SSetLightColor kStack_Pri1, olivel2, purple, 0
									case "mushtitle"
										SSetLightColor kStack_Pri1, mushtitle, purple, 0
										SSetLightColor kStack_Pri1, mushl1, purple, 0
										SSetLightColor kStack_Pri1, mushl2, purple, 0
										SSetLightColor kStack_Pri1, mushl3, purple, 0
									case "saustitle"
										SSetLightColor kStack_Pri1, saustitle, purple, 0
										SSetLightColor kStack_Pri1, sausl1, purple, 0
										SSetLightColor kStack_Pri1, sausl2, purple, 0
										SSetLightColor kStack_Pri1, sausl3, purple, 0
									case "peptitle"
										SSetLightColor kStack_Pri1, peptitle, purple, 0
										SSetLightColor kStack_Pri1, pepl1, purple, 0
										SSetLightColor kStack_Pri1, pepl2, purple, 0
										SSetLightColor kStack_Pri1, pepl3, purple, 0
									case "anchtitle"
										SSetLightColor kStack_Pri1, anchtitle, purple, 0
										SSetLightColor kStack_Pri1, fish1, purple, 0
										SSetLightColor kStack_Pri1, fish2, purple, 0
										SSetLightColor kStack_Pri1, fish3, purple, 0
								End Select 
								PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
								PlayerState(CurrentPlayer).SModePercent(CurrentMode) = CINT((PlayerState(CurrentPlayer).SModeProgress(CurrentMode)  / 9) * 100)
								PlayerState(CurrentPlayer).pizzaPartyProgress(triggerIndex) = 1
								'PlaySound "ro2"
								AddScore 50000
								backlamp "run"
								lightrun purple,hatch2h,1
								flasherspop purple,"left" 'left,right,bottom,rightkick,top,crazy
								playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
								'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
								ShowMsg "mode jackpot", "50,000" 'FormatScore(BonusPoints(CurrentPlayer))
								if PlayerState(CurrentPlayer).SModeProgress(CurrentMode) >= 9 then 
									bModeComplete=True
									'tmrModeCountdown.Enabled = False 
									L34.state = 1
								End if 
							End If 
						else
							debug.print trigger_name
						End If 
					End If 
				next 
			case 1:	' Time Bomb 
				For each a in aTargetTitles
					if a.name = trigger_name and StackState(kStack_Pri1).TargetState(a)=2 then 
						PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
						PlayerState(CurrentPlayer).SModePercent(CurrentMode) = CINT((PlayerState(CurrentPlayer).SModeProgress(CurrentMode)  / 15) * 100)
						AddScore 60000
						backlamp "run"
						lightrun purple,hatch2h,1
						flasherspop purple,"left" 'left,right,bottom,rightkick,top,crazy
						playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						ShowMsg "mode jackpot", "60,000" 'FormatScore(BonusPoints(CurrentPlayer))
						if PlayerState(CurrentPlayer).SModeProgress(CurrentMode) >= 10 then 
							bModeComplete=True
							L35.state = 1
							tmrRovingShot.Enabled = False 
						End if 					
					End If 
				Next
			case 2:	' Death or Glory 
				if trigger_name = "glory" or trigger_name="deathsave" then
					if trigger_name="deathsave" then 	' Extra point for death save 
						PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
					End if 
					PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
					PlayerState(CurrentPlayer).SModePercent(CurrentMode) = CINT((PlayerState(CurrentPlayer).SModeProgress(CurrentMode)  / 5) * 100)
					'PlaySound "ro2"
					AddScore 70000
					backlamp "run"
					lightrun purple,hatch2h,1
					flasherspop purple,"left" 'left,right,bottom,rightkick,top,crazy
					playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					ShowMsg "mode jackpot", "70,000" 'FormatScore(BonusPoints(CurrentPlayer))
					if PlayerState(CurrentPlayer).SModeProgress(CurrentMode) >= 5 then 
						bModeComplete=True
						L39.state = 1
						tmrRovingShot.Enabled = False 
					End if 		
				End if 
			case 3:	' 3 Pizza Rhumba 
				if "rightloop" = trigger_name then 
					surferComboState=1
				elseif "ramp_jp1" = trigger_name and surferComboState=1 then
					PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
					PlayerState(CurrentPlayer).SModePercent(CurrentMode) = CINT((PlayerState(CurrentPlayer).SModeProgress(CurrentMode)  / 3) * 100)
					'PlaySound "ro2"
					AddScore 151111
								backlamp "run"
								lightrun purple,hatch2h,1
								flasherspop purple,"left" 'left,right,bottom,rightkick,top,crazy
					playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				ShowMsg "mode jackpot", "151,111" 'FormatScore(BonusPoints(CurrentPlayer))
					if PlayerState(CurrentPlayer).SModeProgress(CurrentMode) >= 3 then 
						bModeComplete=True
						L40.state = 1
						'tmrModeCountdown.Enabled = False 
					End if 
				else
					surferComboState=0
				End If 
			case 4:	' Bad Town 
				if "bumper" = trigger_name then 
					PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
					PlayerState(CurrentPlayer).SModePercent(CurrentMode) = CINT((PlayerState(CurrentPlayer).SModeProgress(CurrentMode)  / 100) * 100)
					'PlaySound "ro2"
					AddScore 6000
								backlamp "flash"
								flasherspop purple,"rightkick" 'left,right,bottom,rightkick,top,crazy
					'playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				'ShowMsg "mode jackpot", "4,000" 'FormatScore(BonusPoints(CurrentPlayer))
					Select case PlayerState(CurrentPlayer).SModeProgress(CurrentMode)
						case 10
							ShowMsg "10 Bumps", "60,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 20
							ShowMsg "20 Bumps", "120,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 30
							ShowMsg "30 Bumps", "180,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 40
							ShowMsg "40 Bumps", "240,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 50
							ShowMsg "50 Bumps", "300,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 60
							ShowMsg "60 Bumps", "360,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 70
							ShowMsg "70 Bumps", "420,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 80
							ShowMsg "80 Bumps", "480,000" 'FormatScore(BonusPoints(CurrentPlayer))
						case 90
							ShowMsg "90 Bumps", "540,000" 'FormatScore(BonusPoints(CurrentPlayer))	
					end select


					if PlayerState(CurrentPlayer).SModeProgress(CurrentMode) >= 100 then 
						bModeComplete=True
						L41.state = 1
						'tmrModeCountdown.Enabled = False 
					End if
				End if 
			case 5:	' Pizza your love - 10 skillshots 
				if "ramp_jp1" = trigger_name then 
					PlayerState(CurrentPlayer).SModeProgress(CurrentMode) = PlayerState(CurrentPlayer).SModeProgress(CurrentMode)+1
					PlayerState(CurrentPlayer).SModePercent(CurrentMode) = CINT((PlayerState(CurrentPlayer).SModeProgress(CurrentMode)  / 10) * 100)
					'PlaySound "ro2"
					AddScore 80000
								backlamp "run"
								lightrun purple,hatch2h,1
								flasherspop purple,"left" 'left,right,bottom,rightkick,top,crazy
					playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				ShowMsg "mode jackpot", "80,000" 'FormatScore(BonusPoints(CurrentPlayer))
					if PlayerState(CurrentPlayer).SModeProgress(CurrentMode) >= 7 then 
						bModeComplete=True
						L43.state = 1
						'tmrModeCountdown.Enabled = False 
					End if
					
				End If 
		End Select 

		if bModeComplete then 
			clearmodelights(PlayerState(CurrentPlayer).SMode)
debug.print "Mode Complete"
			ShowMsg "mode complete", "500,000" 'FormatScore(BonusPoints(CurrentPlayer))
			playmedia "","audio-celebration",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "mode-complete.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'PlaySound "bell"
			StopSpecialMode
			AddScore 500000
			backlamp "flash"
			lightrun purple,hatch2v,2
			flasherspop purple,"crazy" 'left,right,bottom,rightkick,top,crazy
			ModesComplete=0
			for i = 0 to 6
				if PlayerState(CurrentPlayer).SModePercent(CurrentMode)  >= 100 then ModesComplete = ModesComplete+1
			Next 
			if ModesComplete = 4 then 
				if bEB_Modes = False then 
					StartExtraBall		' Extra Ball scoop opens after 4 modes 
					bEB_Modes = True
				End if 
			End If 
			if ModesComplete = 6 and bWizardModeSupremeFinished=False then 	' Setup Wizard Mode Supreme
				bWizardModeSupremeReady = True
				ufolocklight.state =2
				pizzaorder.state = 2
				modelight.state = 2
				ShowMsg "wizard ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "s u p r e m e", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End if 
		End if 
	End sub 

	Sub StopSpecialMode
		inamode=0
		clearmodelights(PlayerState(CurrentPlayer).SMode)
		rhumbatime.enabled=false
		modelightstime.enabled = false
		if ufogi + ptgi = 0 Then
			gicolor white
		end If
		modegi = 0
		'ShowMsg "mode time up", "" 'FormatScore(BonusPoints(CurrentPlayer))
		StopCountdown
		if PlayerState(CurrentPlayer).SMode <> -1 then 
			StopCountdown
			If PlayerState(CurrentPlayer).SMode = 2 then 	' Turn off lane lights 
				L13.state = 0
				L26.state = 0
				L27.state = 0
				L28.state = 0
			elseif PlayerState(CurrentPlayer).SMode = 4 then
				Light109.State = 1
				roni1.state = 1
				roni2.state = 1
				roni3.state = 1
			End if 
			StackState(kStack_Pri1).Disable()
			
			spinCount=spinCount+1
			PlayerState(CurrentPlayer).Specialties(kSpecial_Spins) = 25 '+ (10*spinCount)	 	' Reset the spinner
'			if bWizardModeSupremeFinished then
'				PlayerState(CurrentPlayer).Specialties(kSpecial_Spins) = 72*2	' Reset the spinner
'			Else 
'				PlayerState(CurrentPlayer).Specialties(kSpecial_Spins) = 25 + (10*spinCount)	 	' Reset the spinner
'			End if 
			puPlayer.LabelSet pBackglass,"CollectVal1",	PlayerState(CurrentPlayer).Specialties( kSpecial_Spins)	,1,""
			PlayerState(CurrentPlayer).SMode = -1
		if ufogi + ptgi = 0 Then
			StopModeMusic
		end If
			RestoreAllLights
			ShowRule(kRule_PizzaTime)
			'puPlayer.LabelSet pBackglass,"ModeTmr",	""	,1,""
		End If 
	End Sub 

	Dim NextMode:NextMode=0
	Sub SModeUpdate()	' Cycle to the next available specialty mode (Bumpers cycle this)
		checkModeProgress("bumper")
		dim cnt:cnt=0
		Do
			cnt=cnt+1
			NextMode = (NextMode + 1)MOD 6
		Loop While PlayerState(CurrentPlayer).SModePercent(NextMode) >= 100 and cnt<8
debug.print "NextMode:" & NextMode
Debug.print "  Progress:" & PlayerState(CurrentPlayer).SModePercent(NextMode)
	End Sub

	Sub BonusProgress(pType)
		BonusTotals(pType)=BonusTotals(pType)+1
	End Sub 

	Sub SpecialProgress(pType)
		if ufomulti=true then exit sub
		if PlayerState(CurrentPlayer).Specialties(pType) >0 then 
debug.print "SpecialProgress:" & pType
			PlayerState(CurrentPlayer).Specialties(pType) = PlayerState(CurrentPlayer).Specialties(pType) - 1

			Select case pType				' SCORING 
				Case kSpecial_Spins:		' Mode-Spins
					AddScore 5
					AddBonus 10
				Case kSpecial_Locks:		' SpacePizza-Locks
					AddBonus 2000	
				Case kSpecial_Ramps:		' Video Mode-Ramps
					AddScore 1000
					AddBonus 300
				Case kSpecial_Multiplier:	' Beer Frenzy-Multiplier X
					' TBD - What do we do here
				Case kSpecial_SlicesEaten:	' ExtraBall-Slices Eaten (During PizzaTime)
					AddBonus 500 * BallsOnPlayfield
				Case kSpecial_LoopDeLoop:	' Flippen Mad-Loop-De-Loops
					AddScore 1000
					AddBonus 300
				Case kSpecial_Tip:			' Tip Jar-Tip Bumper
					AddBonus 200
			End Select 

			puPlayer.LabelSet pBackglass,"CollectVal1",	PlayerState(CurrentPlayer).Specialties(0)	,1,""
			puPlayer.LabelSet pBackglass,"CollectVal2",	PlayerState(CurrentPlayer).Specialties(1)	,1,""
			puPlayer.LabelSet pBackglass,"CollectVal3",	PlayerState(CurrentPlayer).Specialties(2)	,1,""
			puPlayer.LabelSet pBackglass,"CollectVal4",	PlayerState(CurrentPlayer).Specialties(3)	,1,""
			puPlayer.LabelSet pBackglass,"CollectVal5",	PlayerState(CurrentPlayer).Specialties(4)	,1,""
			puPlayer.LabelSet pBackglass,"CollectVal6",	PlayerState(CurrentPlayer).Specialties(5)	,1,""
			puPlayer.LabelSet pBackglass,"CollectVal7",	PlayerState(CurrentPlayer).Specialties(6)	,1,""
			
			if PlayerState(CurrentPlayer).Specialties(pType) = 0 then 
				Select case pType
					Case kSpecial_Spins:		' Mode-Spins
						modelight.state = 2
						playmedia "","audio-modelit",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						playmedia "mode-lit.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					Case kSpecial_Locks:		' SpacePizza-Locks
						' Nothing to do here
					Case kSpecial_Ramps:		' Video Mode-Ramps
						StartVideoMode
					Case kSpecial_Multiplier:	' Beer Frenzy-Multiplier X
						
					Case kSpecial_SlicesEaten:	' ExtraBall-Slices Eaten (During PizzaTime)
						if bEB_Eat = False then
							bEB_Eat = True
							StartExtraBall
						End if 
					Case kSpecial_LoopDeLoop:	' Flippen Mad-Loop-De-Loops (2x for 20 or 3x for 40)

						If doublepoints = False Then
			playmedia "","audio-flippinman",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "flippin-mad.mp4","video-scenes",pBackglass,"cineon",3000,"ebkickit",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
							tmrBeerFrenzy.UserValue = 20
							doublepoints = True
							twoxpoints.state = 2
								backlamp "run"
						Else
					playmedia "","audio-triplescoring",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "triple-scoring.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
							tmrBeerFrenzy.UserValue = 40
							triplepoints = True
							threexpoints.state = 2
								backlamp "run"
						End If
						tmrBeerFrenzy.Enabled = True
					Case kSpecial_Tip:			' Tip Jar-Tip Bumper
						
 				End Select 
			end if 

		End If 
	End Sub 

	sub cabon
		cabscreen.enabled=True
	end Sub

	sub caboff
		cabscreen.enabled = False
		cabpos=0
		Flasher1.ImageA = "cabscreen"
		Flasher1.ImageB = "cabscreen"
	end Sub

	dim cabpos:cabpos=0
	sub cabscreen_timer
		cabpos=cabpos+1
		select case cabpos
			case 1
				Flasher1.ImageA = "cabscreen1"
				Flasher1.ImageB = "cabscreen1"
			case 2
				Flasher1.ImageA = "cabscreen2"
				Flasher1.ImageB = "cabscreen2"
			case 3
				Flasher1.ImageA = "cabscreen3"
				Flasher1.ImageB = "cabscreen3"
			case 4
				Flasher1.ImageA = "cabscreen4"
				Flasher1.ImageB = "cabscreen4"
			case 5
				Flasher1.ImageA = "cabscreen5"
				Flasher1.ImageB = "cabscreen5"
			case 6
				Flasher1.ImageA = "cabscreen6"
				Flasher1.ImageB = "cabscreen6"
			case 7
				Flasher1.ImageA = "cabscreen7"
				Flasher1.ImageB = "cabscreen7"
			case 8
				Flasher1.ImageA = "cabscreen8"
				Flasher1.ImageB = "cabscreen8"
			case 9
				Flasher1.ImageA = "cabscreen7"
				Flasher1.ImageB = "cabscreen7"
			case 10
				Flasher1.ImageA = "cabscreen6"
				Flasher1.ImageB = "cabscreen6"
			case 11
				Flasher1.ImageA = "cabscreen5"
				Flasher1.ImageB = "cabscreen5"
			case 12
				Flasher1.ImageA = "cabscreen4"
				Flasher1.ImageB = "cabscreen4"
			case 13
				Flasher1.ImageA = "cabscreen3"
				Flasher1.ImageB = "cabscreen3"
			case 14
				Flasher1.ImageA = "cabscreen2"
				Flasher1.ImageB = "cabscreen2"
				cabpos=0
		end Select
	end Sub


	Sub StartVideoMode
				cabon
				ShowMsg "minigame lit!", "" 'FormatScore(BonusPoints(CurrentPlayer))
					playmedia "","audio-minigamelit",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "minigame-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
debug.print "Starting Video:" & pizzamulti
		if pizzamulti or bMultiBallMode then		' Dont start this if we are in pizza time or MB
			videoready=True
		Else 

			If BallsOnPlayfield > 1 then 
				MsgBox "ERROR BMultiBallMode doesnt match BallsOnPlayField"
			End if 

			kicker3.Enabled =True
			Gate003.Open=True
			videomode.state = 2
			videoready=True
			KickerVideoMode.Enabled = True
		End if 
'		if tmrVideoMode.Enabled = False and VideoModePopup.z < 0 then  ' Is isnt raising/lowering and it is already all the way down
'			tmrVideoMode.Interval = 100
'			tmrVideoMode.UserValue = 1		' 1=Raise up, -1=Close
'			tmrVideoMode.Enabled = True
'			videoready=True
'		End If 
	End Sub 

	Sub StopVideoMode
		caboff
		if KickerVideoMode.Enabled Then
debug.print "Stop VideoMode"
			kicker3.Enabled=False
			Gate003.Open=False
			videomode.state = 0
			videoready=False
			KickerVideoMode.Enabled = False
		End if 

'		if tmrVideoMode.Enabled = False and KickerVideoMode.Enabled then  ' Video mode is up
'debug.print "Stop VideoMode"
'			tmrVideoMode.Interval = 100
'			tmrVideoMode.UserValue = -1		' 1=Raise up, -1=Close
'			tmrVideoMode.Enabled = True
'			videoready=False
'		End If 
	End Sub


'	Sub tmrVideoMode_timer 
'		VideoModePopup.z = VideoModePopup.z + (10 * tmrVideoMode.UserValue)
'		if VideoModePopup.z <= -86 then 
'			VideoModePopup.z = -86
'			tmrVideoMode.Enabled = False 
'			VideoModePopup.Collidable = False
'			KickerVideoMode.Enabled = False 
'		End If 
'		if VideoModePopup.z >=-10 then 
'			VideoModePopup.z = -10
'			VideoModePopup.Collidable = True
'			tmrVideoMode.Enabled = False 
'			KickerVideoMode.Enabled = True
'		End If 
'
'	End sub 

	Sub KickerVideoMode_Hit
		PlayerState(CurrentPlayer).VideoModeCount=PlayerState(CurrentPlayer).VideoModeCount+1
		'playmedia "defenseagainst.mp4","videoscenes",pBackglass,"",0,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		startminigame
		'bumplvl(CurrentPlayer) = bumplvl(CurrentPlayer) + 1
		inminigame = 1
		'pbumps(CurrentPlayer) = 0
	End Sub 


	Sub StartExtraBall
		playmedia "","audio-eblit",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		playmedia "eb-is-lit.mp4","video-scenes",pBackglass,"cineon",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		ShowMsg "extraball lit!", "" 'FormatScore(BonusPoints(CurrentPlayer))

		EBQueue=EBQueue+1			' Queue it up so we can still capture it 
		Kicker3.Enabled = True
		Gate003.Open=True
		extraball.state = 2
		'bEB_Eat = True
		KickerExtraBall.Enabled = True
'		if tmrExtraBall.Enabled = False and ExtraBallPopup.z < 0 then  ' Is isnt raising/lowering and it is already all the way down
'			tmrExtraBall.Interval = 100
'			tmrExtraBall.UserValue = 1		' 1=Raise up, -1=Close
'			tmrExtraBall.Enabled = True
'		End If 
	End Sub 

Sub StopExtraBall
if KickerExtraBall.Enabled then
if Kicker3.Enabled then Kicker3.Enabled = False
if KickerVideoMode.Enabled=False then Gate003.Open=False
extraball.state = 0
KickerExtraBall.Enabled = False
End If
End Sub

'	Sub tmrExtraBall_timer 
'		ExtraBallPopup.z = ExtraBallPopup.z + (10 * tmrExtraBall.UserValue)
'		if ExtraBallPopup.z <= -86 then 
'			ExtraBallPopup.z = -86
'			tmrExtraBall.Enabled = False 
'			ExtraBallPopup.Collidable = False
'			KickerExtraBall.Enabled = False 
'		End If 
'		if ExtraBallPopup.z >=-10 then 
'			ExtraBallPopup.z = -10
'			ExtraBallPopup.Collidable = True
'			tmrExtraBall.Enabled = False 
'			KickerExtraBall.Enabled = True
'		End If 
'
'	End sub 

	Sub KickerExtraBall_Hit
		EBQueue=EBQueue-1
		AwardExtraBall(True)			' This plays the video and calls kickit
	End Sub 

	sub ebkickit
debug.print "EBKick:" & EBQueue
		KickerExtraBall.Kick 90, 4
		' Only stop if we dont have any more queued up
		if EBQueue = 0 then StopExtraBall	
	end sub

'   Changed to standard timers 
'	Sub tmrWizardAte_Timer
'		tmrWizardAte.UserValue = tmrWizardAte.UserValue -1 
'		puPlayer.LabelSet pBackglass,"WizTmr", "Wizard:"	& tmrWizardAte.UserValue	,1,""
'
'		if tmrWizardAte.UserValue <= 5 and tmrWizardAte.UserValue >=0 then 
'			PlaySound "so_timer"
'		End if 
'
'		if tmrWizardAte.Uservalue = 0 then  
'debug.print "Ate Timer Expired"
'			StopWizardAte
'		End if 
'	End Sub
'
'	Sub tmrWizardIllum_Timer
'		tmrWizardIllum.UserValue = tmrWizardIllum.UserValue -1 
'		puPlayer.LabelSet pBackglass,"WizTmr", "Wizard:"	& tmrWizardIllum.UserValue	,1,""
'
'		if tmrWizardIllum.UserValue <= 5 and tmrWizardIllum.UserValue >=0 then 
'			PlaySound "so_timer"
'		End if
'
'		if tmrWizardIllum.Uservalue = 0 then  
'debug.print "Illum Timer Expired"
'			StopWizardIllum
'		End if 
'	End Sub

	Sub StartWizardIllum
		GiOff
		illgi = 1
		StackState(kStack_Pri2).Enable()
		pupDMDDisplay "-", "Illuminati", "" ,3, 0, 10		'3 seconds
' TBD Need music 
'		playclear pMusic
'		playmedia "Pizza Time - Pizza Time.mp3", MusicDir, pMusic, "", -1, "", 1, 1

		' End other modes 
		EndUfo
		StopSpecialMode
		illuminati.state = 2

		SSetLightColor kStack_Pri2, pepperonititle, yellow, 2		' Loops
		bWizardModeIllum = True				' Start illuminati
		bWizardModeIllumReady = False 
		if pizzaisopen = False then pizzaopen
		ShowRule(kRule_Illuminati)

		StartCountdown(15)					' 15 second countdown
		'tmrWizardIllum.UserValue = 15
		'tmrWizardIllum.Enabled = True
		'puPlayer.LabelSet pBackglass,"WizTmr", "Wizard:"	& tmrWizardIllum.UserValue	,1,""
	End Sub 

	Sub StopWizardIllum
		illgi=0
		GiOn
		dim CompleteCount

debug.print "STOP Illuninati"
		StackState(kStack_Pri2).Disable()
		RestoreAllLights

		bWizardModeIllum = False 
		StopCountdown
		'tmrWizardIllum.Enabled = False
		'puPlayer.LabelSet pBackglass,"WizTmr", ""	,1,""
		StopModeMusic
		if bWizardModeIllumFinished then 							' GAME FINISHED !!!!
			illuminati.state = 1
			AddScore 12000000
			ShowMsg "Wizard Illuminati", FormatScore(12000000)
								backlamp "flash"
								flasherspop white,"crazy" 'left,right,bottom,rightkick,top,crazy
								lightrun white,circlein,10
					playmedia "","audio-illuminaticomplete",pCallouts,"cineon",16000,"kickit",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				ShowMsg "Illuminati Champ", "12,000,000" 'FormatScore(BonusPoints(CurrentPlayer))
			PlaySoundVol "cheer", VolDef
			bBallSaverReady = True
			'kickit

			tmrUFOSpin.UserValue=0									' Flip the ship
			tmrUFOSpin.Enabled = True
			' TBD Play Video 
		Elseif BallsOnPlayfield <> 0 then
			pizzaopen
			DisableFlippers=True									' Let all balls drain
			RightFlipper.RotateToStart
			LeftFlipper.RotateToStart
			LeftFlipper2.RotateToStart
			vpmTimer.addTimer 8000, "DisableFlippers=False:bBallSaverReady = True:kickit '"
		End If 


		' Clear player state except number of completions 
		CompleteCount = PlayerState(CurrentPlayer).completedBBCount
		PlayerState(CurrentPlayer).Reset()

		' Clear Table states 
		jps = 0
		targetbonuses = 0
		glorycount=0
		ufoPizzasCollected=0
		gottips=False
		tips=0
		ufolock=0
		ronibumps=0
		ptready=False 
		videoready=False 
		pzlnum = 0
		pzlnum2 = 0
		extraballgiven = False 
		addaballmade = False
		currentpizza =0
		pizzasize = 1
		cheesevalue = 0
		totalronis = 0
		spinCount=0

		bEB_Video=False
		bEB_Modes=False
		bEB_Eat = False
		EBQueue=0

		puPlayer.LabelSet pBackglass,"CollectVal1", PlayerState(CurrentPlayer).Specialties(0)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal2",	PlayerState(CurrentPlayer).Specialties(1)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal3",	PlayerState(CurrentPlayer).Specialties(2)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal4",	PlayerState(CurrentPlayer).Specialties(3)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal5",	PlayerState(CurrentPlayer).Specialties(4)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal6",	PlayerState(CurrentPlayer).Specialties(5)	,1,""
		puPlayer.LabelSet pBackglass,"CollectVal7",	PlayerState(CurrentPlayer).Specialties(6)	,1,""

		' Blank Lights 
		Dim bulb
		For each bulb in aLights
			bulb.State = 0
		Next
		cheese4l.opacity = 0
		cheese3l.opacity = 0
		cheese2ll.opacity = 0
		cheese1l.opacity = 0
		ufolightlock = false
		bWizardModeSupreme = False 
		bWizardModeSupremeReady = False 
		bWizardModeSupremeFinished = False 

		bWizardModeIllum = False 
		bWizardModeIllumReady = False 
		bWizardModeIllumFinished = False 

		bWizardModeAte = False 
		bWizardModeAteReady = False 
		bWizardModeAteFinished = False 

		PlayerState(CurrentPlayer).Save()
		PlayerState(CurrentPlayer).completedBBCount=CompleteCount
		pizzaorder.state = 2

	End Sub 

	Sub StartWizardAte
		ategi=1
		gicolor orange
		if bWizardModeAteFinished=False then 
debug.print "Start Wizard Ate"
								backlamp "run"
								lightrun orange,circleout,3
				ShowMsg "You ate the whole thing!", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "Wizard Mode!", "" 'FormatScore(BonusPoints(CurrentPlayer))
			playmedia "", "audio-bbwizstart", pCallouts, "", 3000, "", 1, 1
			PlayModeMusic  "Ty Segall - Every 1's a Winner.mp3"
			' End other modes 
			EndUfo
			StopSpecialMode

			StackState(kStack_Pri2).Enable()
			pupDMDDisplay "-", "I Ate It All^Wizard Mode", "" ,3, 0, 10		'3 seconds
			SSetLightColor kStack_Pri2, pepperonititle, purple, 2		' Loops
			SSetLightColor kStack_Pri2, anchtitle, purple, 2
			skillarrow.state = 2
			leftarrow.state = 2
			rightarrow.state = 2
			L45.state=2
			jackpotl1.state =2
			jackpotl3.state = 2

			iateitall.state = 2

			ShowRule(kRule_AteWholeThing)
			bWizardModeAte = True	' Start I Ate the whole thing 
			pz4title.state =1

			EnableBallSaver 45
			StartCountdown(45)

			'tmrWizardAte.UserValue = 30		' 30 second countdown
			'tmrWizardAte.Enabled = True
			'puPlayer.LabelSet pBackglass,"WizTmr", "Wizard:"	& tmrWizardAte.UserValue	,1,""
		End if 
	End Sub 

	Sub StopWizardAte
		ategi = 0
        gicolor white
			playmedia "", "crust-bbcomplete", pBackglass, "", 6500, "", 1, 1
debug.print "STOP Wizard Ate"
								backlamp "flash"
								lightrun red,circleout,4
								flasherspop red,"crazy" 'left,right,bottom,rightkick,top,crazy
				ShowMsg "wizard complete", "" 'FormatScore(BonusPoints(CurrentPlayer))
		SSetLightColor kStack_Pri2, anchtitle, purple, 0
		StackState(kStack_Pri2).Disable()
		RestoreAllLights

		StopModeMusic
		if bWizardModeSupremeFinished then 	' If we finished Supreme then start Illuminati
			DisableFlippers=True									' Let all balls drain
			RightFlipper.RotateToStart
			LeftFlipper.RotateToStart
			LeftFlipper2.RotateToStart
			bAutoPlunger = True
			vpmTimer.addTimer 8000, "DisableFlippers=False:AddMultiball 1:StartWizardIllum '" 	' Final Final wizard mode of the game 
		End if 

		skillarrow.state = 0
		leftarrow.state = 0
		rightarrow.state = 0
		L45.state=0
		jackpotl1.state =0
		jackpotl3.state =0

		StopCountdown
		'tmrWizardAte.Enabled = False
		puPlayer.LabelSet pBackglass,"WizTmr", ""	,1,""
		bWizardModeAteFinished = True	' Finish I Ate the whole thing 
		iateitall.state = 1
		bWizardModeAte = False 

		WizardAteFinished
	End Sub 

	Sub WizardAteFinished()
		' Clear Table states 
		jps = 0
		targetbonuses = 0
		glorycount=0
		ufoPizzasCollected=0
		gottips=False
		tips=0
		ufolock=0
		ronibumps=0
		ptready=False 
		pzlnum = 0
		pzlnum2 = 0

		currentpizza =0
		pizzasize = 1
		cheesevalue = 0
		totalronis = 0
		spinCount=0

		' Blank Lights 
		Dim bulb
		For each bulb in aTargetLights
			bulb.State = 0
		Next
		cheese4l.opacity = 0
		cheese3l.opacity = 0
		cheese2ll.opacity = 0
		cheese1l.opacity = 0

		PlayerState(CurrentPlayer).completedBBCount=PlayerState(CurrentPlayer).completedBBCount+1
		pizzaorder.state = 2
		pz4title.state =1		' Keep this one lit		
	End Sub 

	Sub StartWizardSupreme
		supgi = 1
		gicolor blue
Debug.print "Start Wizard Mode Supreme"
				ShowMsg "s u p r e m e", "" 'FormatScore(BonusPoints(CurrentPlayer))
				ShowMsg "wizard mode!", "" 'FormatScore(BonusPoints(CurrentPlayer))
		if bWizardModeSupreme = False then
			pupDMDDisplay "-", "Supreme^Wizard Mode", "" ,3, 0, 10		'3 seconds
			startkick
								backlamp "run"
								lightrun blue,arcblu,3
			StackState(kStack_Pri2).Enable()
			AddMultiball 1
			ufolocklight.state =0
			pizzaorder.state = 0
			modelight.state = 0	

			' End other modes 
			EndUfo
			StopSpecialMode
			supreme.state = 2

			'playclear pMusic
			'playmedia "Sham 69 - Hurry Up Harry.mp3", MusicDir, pMusic, "", -1, "", 1, 1
			PlayModeMusic "Sham 69 - Hurry Up Harry.mp3"

			bWizardModeSupreme = True
			bWizardModeSupremeMB1=False
			bWizardModeSupremeMB2=False
			bWizardModeSupremeReady=False 
			SSetLightColor kStack_Pri2, ecl2, green, 2
			SSetLightColor kStack_Pri2, olivetitle, green, 2
			SSetLightColor kStack_Pri2, mushtitle, green, 2
			SSetLightColor kStack_Pri2, peptitle, green, 2
			SSetLightColor kStack_Pri2, pepperonititle, green, 2
			SSetLightColor kStack_Pri2, bacontitle, green, 2
			SSetLightColor kStack_Pri2, pinetitle, green, 2
			SSetLightColor kStack_Pri2, saustitle, green, 2
			SSetLightColor kStack_Pri2, anchtitle, green, 2
		End If 
	End Sub 
	Sub StopWizardSupreme()
		supgi=0
		gicolor white
		if bWizardModeSupreme Then 
debug.print "Stop WizardSupreme"
								backlamp "flash"
								lightrun blue,arcbld,5
								flasherspop blue,"crazy" 'left,right,bottom,rightkick,top,crazy
					'playmedia "","audio-supremecomplete",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "","crust-supremecomplete",pBackglass,"",5500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				ShowMsg "wizard complete", "" 'FormatScore(BonusPoints(CurrentPlayer))
			bWizardModeSupreme = False 
			StackState(kStack_Pri2).Disable()
			RestoreAllLights

			if bWizardModeAteFinished then 	' If we finished IAteTheWholeThing then start Illuminati
				DisableFlippers=True									' Let all balls drain
				RightFlipper.RotateToStart
				LeftFlipper.RotateToStart
				LeftFlipper2.RotateToStart
				bAutoPlunger = True
					playmedia "","audio-illuminatistart",pCallouts,"",16000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					'playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				vpmTimer.addTimer 16000, "DisableFlippers=False:AddMultiball 1:StartWizardIllum '" 	' Final Final wizard mode of the game 
			End if

			StopModeMusic
		End If 
		if bWizardModeSupremeReady then 
			ufolocklight.state =0
			pizzaorder.state = 0
			modelight.state = 0	
		End if 
	End Sub

	Sub SetBGPizza(bHide)
		dim pType, sCollected, sEaten, show
		Dim MaxSlices
		if ufomulti = True then exit sub
		if currentpizza=0 then exit sub 
		if bMediaSet(pBackglass) and bHide=False then	' If something is being displayed then just updated and make it hidden
debug.print "Skipping Media is playing" & bhide
			Exit Sub 
		End if 
'Setting Pizza-video-pizzas\\pp-2.5-0.png Show:1 bhide:False
		if pizzasize=1 then MaxSlices=4.0
		if pizzasize=2 then MaxSlices=8.0
		if pizzasize=3 then MaxSlices=12.0
		if pizzasize=4 then MaxSlices=16.0

Debug.print "SetBGPizza Size: " & pizzasize & " MaxSlices:" & MaxSlices & " pzlnum:" & pzlnum & " jps:" & jps


		'pizzasize=1,2,3,4
		' jps - slices eaten
		if jps>=MaxSlices then 
			sEaten = 8
		Elseif jps<>0 and pzlnum<>MaxSlices then 	' We are in hard mode and/or recollecting 
			sEaten=0
		Else
			sEaten=INT(jps/MaxSlices * 8)
		End If 
		'pzlnum	- Collected 
		sCollected=INT(pzlnum/MaxSlices * 8) - sEaten
		show=1
		if bHide then show=0
		'puPlayer.LabelSet pBackglass,"PProgress", "video-pizzas\\gp-8-0.png", 1,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"
		Select Case currentpizza
			Case 1
				pType="pp"
			Case 2
				pType="vg"
			Case 3
				pType="sp"
			Case 4
				pType="ml"
			Case 5
				pType="mh"
			Case 6
				pType="ss"
			Case 7
				pType="gp"
			Case 8
				pType="hw"
		End Select
		puPlayer.LabelSet pBackglass,"PProgress", "video-pizzas\\"&pType&"-"&sCollected&"-"&sEaten&".png", show,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"
debug.print "Setting Pizza-" & "video-pizzas\\"&pType&"-"&sCollected&"-"&sEaten&".png" & " Show:" & show & " bhide:" & bHide


	End Sub

	Function GetPizzaName()
		GetPizzaName=""
		Select Case currentpizza
			Case 1
				GetPizzaName="pp"
			Case 2
				GetPizzaName="vg"
			Case 3
				GetPizzaName="sp"
			Case 4
				GetPizzaName="ml"
			Case 5
				GetPizzaName="mh"
			Case 6
				GetPizzaName="ss"
			Case 7
				GetPizzaName="gp"
			Case 8
				GetPizzaName="hw"
		End Select
	End Function


	Sub pizzatype		' Setup This Pizza Type 
Debug.print "Selecting Pizza:" & currentpizza & " PTMB Ready:" & pizzaisopen & " Type: " & currentpizza
		if pizzaisopen then exit sub 
	
	' pizzas
	' 1 - Pepperoni - 700
	' 2 - Veggies - 750
	' 3 - Supreme - 795
	' 4 - Meat Lovers - 830
	' 5 - Manhattan - 880
	' 6 - Sea Side - 920
	' 7 - Garden Party - 960
	' 8 - Hawaiian - 1000

		PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\clear.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
		PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\clear.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
		PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\clear.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"
		PuPlayer.LabelSet pBackglass, "Item4", "PuPOverlays\\clear.png",1,"{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':79.5}"
		PuPlayer.LabelSet pBackglass, "Item5", "PuPOverlays\\clear.png",1,  "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':88}"
		
		StackState(kStack_Pri0).Enable()		' Enable the stack

' Reset all the lights 
		SSetLightColor kStack_Pri0, ecl2, white, 0
		SSetLightColor kStack_Pri0, olivetitle, white, 0
		SSetLightColor kStack_Pri0, mushtitle, white, 0
		SSetLightColor kStack_Pri0, peptitle, white, 0
		SSetLightColor kStack_Pri0, pepperonititle, white, 0
		SSetLightColor kStack_Pri0, bacontitle, white, 0
		SSetLightColor kStack_Pri0, pinetitle, white, 0
		SSetLightColor kStack_Pri0, saustitle, white, 0
		SSetLightColor kStack_Pri0, anchtitle, white, 0

		roni1.state = 0
		roni2.state = 0
		roni3.state = 0

		Select Case currentpizza
			Case 0

			Case 1
				roni1.state = 2
				roni2.state = 2
				roni3.state = 2
				'ecl2.state = 2
				SSetLightColor kStack_Pri0, ecl2, white, 2
				SSetLightColor kStack_Pri0, pepperonititle, white, 2

				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\pepperoni.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\cheese.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"

			Case 2
'				olivetitle.state = 2
'				mushtitle.state = 2
'				peptitle.state = 2
				SSetLightColor kStack_Pri0, olivetitle, white, 2
				SSetLightColor kStack_Pri0, mushtitle, white, 2
				SSetLightColor kStack_Pri0, peptitle, white, 2

				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\olive.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\mushroom.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\pepper.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"

			Case 3
'				olivetitle.state = 2
'				mushtitle.state = 2
'				peptitle.state = 2
'				saustitle.state = 2
				roni1.state = 2
				roni2.state = 2
				roni3.state = 2
				SSetLightColor kStack_Pri0, olivetitle, white, 2
				SSetLightColor kStack_Pri0, mushtitle, white, 2
				SSetLightColor kStack_Pri0, peptitle, white, 2
				SSetLightColor kStack_Pri0, saustitle, white, 2
				SSetLightColor kStack_Pri0, pepperonititle, white, 2


				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\olive.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\mushroom.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\pepper.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"
				PuPlayer.LabelSet pBackglass, "Item4", "PuPOverlays\\sausage.png",1,"{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':79.5}"
				PuPlayer.LabelSet pBackglass, "Item5", "PuPOverlays\\pepperoni.png",1,  "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':88}"

			Case 4
'				saustitle.state = 2
				roni1.state = 2
				roni2.state = 2
				roni3.state = 2
'				bacontitle.state = 2
				fishy1.state = 2
				fishy2.state = 2
				fishy3.state = 2
				fishy4.state = 2

				SSetLightColor kStack_Pri0, saustitle, white, 2
				SSetLightColor kStack_Pri0, pepperonititle, white, 2
				SSetLightColor kStack_Pri0, bacontitle, white, 2
				SSetLightColor kStack_Pri0, anchtitle, white, 2


				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\sausage.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\pepperoni.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\bacon.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"
				PuPlayer.LabelSet pBackglass, "Item4", "PuPOverlays\\sardine.png",1,"{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':79.5}"

			Case 5
'				ecl2.state = 2
'				mushtitle.state = 2
'				saustitle.state = 2
'				bacontitle.state = 2

				SSetLightColor kStack_Pri0, ecl2, white, 2
				SSetLightColor kStack_Pri0, mushtitle, white, 2
				SSetLightColor kStack_Pri0, saustitle, white, 2
				SSetLightColor kStack_Pri0, bacontitle, white, 2


				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\cheese.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\mushroom.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\sausage.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"
				PuPlayer.LabelSet pBackglass, "Item4", "PuPOverlays\\bacon.png",1,"{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':79.5}"

			Case 6
'				olivetitle.state = 2
				fishy1.state = 2
				fishy2.state = 2
				fishy3.state = 2
				fishy4.state = 2
'				peptitle.state = 2

				SSetLightColor kStack_Pri0, olivetitle, white, 2
				SSetLightColor kStack_Pri0, anchtitle, white, 2
				SSetLightColor kStack_Pri0, peptitle, white, 2

				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\olive.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\sardine.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\pepper.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"

			Case 7
'				bacontitle.state = 2
'				peptitle.state = 2
'				olivetitle.state = 2
'				pinetitle.state = 2

				SSetLightColor kStack_Pri0, bacontitle, white, 2
				SSetLightColor kStack_Pri0, peptitle, white, 2
				SSetLightColor kStack_Pri0, olivetitle, white, 2
				SSetLightColor kStack_Pri0, pinetitle, white, 2


				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\bacon.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\pepper.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\olive.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"
				PuPlayer.LabelSet pBackglass, "Item4", "PuPOverlays\\pineapple.png",1,"{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':79.5}"

			Case 8
				roni1.state = 2
				roni2.state = 2
				roni3.state = 2
'				pinetitle.state = 2
'				ecl2.state = 2

				SSetLightColor kStack_Pri0, pepperonititle, white, 2
				SSetLightColor kStack_Pri0, pinetitle, white, 2
				SSetLightColor kStack_Pri0, ecl2, white, 2

				PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\pepperoni.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
				PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\pineapple.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
				PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\cheese.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"

		End Select
	End Sub

	Sub playerlights
		resetalllights
	End Sub

' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Table Hit Events
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 
 
	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	 '-> play a sound
	 '-> do some physical movement
	 '-> add a score, bonus
	 '-> check some variables/Mode this trigger is a member of
	 '-> set the "LastSwitchHit" variable in case it is needed later
	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	' Slingshots has been hit

	Dim LStep
	Dim RStep


	Sub Spinner1_Spin()
		BonusProgress(kBonus_Spins)
		SpecialProgress(kSpecial_Spins)
		if PlayerState(CurrentPlayer).Specialties(kSpecial_Spins) < 1 then
			if inamode=0 and modelight.state = 0 and ufomulti = False Then
				modelight.state = 2
			end if 
		end if
		DOF 332, DOFPulse 'MX-middleramp
		DOF 432, DOFPulse 'rgb
		DOF 405, DOFPulse 'strobe
		PlaySound "fx_spinner"
	End sub 
	Sub Spinner2_Spin()
		BonusProgress(kBonus_Spins)
		SpecialProgress(kSpecial_Spins)
		PlaySound "fx_spinner"
		if PlayerState(CurrentPlayer).Specialties(kSpecial_Spins) < 1 then
			if inamode=0 and modelight.state = 0 and ufomulti = False Then
				modelight.state = 2
			end if 
		end if
	End sub 
	Sub Spinner3_Spin()
		BonusProgress(kBonus_Spins)
		SpecialProgress(kSpecial_Spins)
		PlaySound "fx_spinner"
		if PlayerState(CurrentPlayer).Specialties(kSpecial_Spins) < 1 then
			if inamode=0 and modelight.state = 0 and ufomulti = False Then
				modelight.state = 2
			end if 
		end if
	End sub 

	Sub leftslingwall_Slingshot
		extrasmallpoints
		leftskullpop
		If Tilted Then Exit Sub
		startB2S(1)
		LightEffect 7
		PlaySound SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
		PlaySound "LeftSlingShot"
		'PlaySound "leftbones"
		playsfx("sling")
		DOF 308, DOFPulse	'MX_slingl
		DOF 409, DOFPulse	'rgb
		LStep = 0
		leftskull
		AddScore 110
		LastSwitchHit = "LeftSlingShot"
	End Sub

	LeftSlingShot1.enabled = 0
	Sub leftskull
		LeftSlingShot1.enabled = 1
	End Sub

	Sub LeftSlingShot1_Timer
		Select Case LStep
			Case 1:LeftSling4.Visible = 0:LeftSling3.Visible = 1
			Case 2:LeftSling3.Visible = 0:LeftSling2.Visible = 1
			Case 3:LeftSling2.Visible = 0:LeftSlingShot1.Enabled = 0
		End Select

		LStep = LStep + 1
	End Sub

	skulllefttime.enabled = 0
	Sub leftskullpop
		skulllefttime.enabled = 1
	End Sub
	
	Dim skullleftfull
	skullleftfull = false

	Sub skulllefttime_Timer
		If skullleftfull = false Then
			skullleft.RotZ = (skullleft.RotZ + 1) Mod 360
			If skullleft.RotZ = 270 Then
				skullleftfull = true
			End If
		Else
			skullleft.RotZ = (skullleft.RotZ - 1) Mod 360
			If skullleft.RotZ = 250 Then
				skullleftfull = false
				skulllefttime.enabled = 0
			End If
		End If
	End Sub


	Sub rightslingwall_Slingshot
		extrasmallpoints
		rightskullpop
		If Tilted Then Exit Sub
		startB2S(1)
		LightEffect 6
		PlaySound SoundFXDOF("fx_slingshot",104, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
		PlaySound "RightSlingShot"
		'PlaySound "leftbones"
		DOF 408, DOFPulse	'rgb
		DOF 307, DOFPulse	'MX_slingr
		RStep = 0
		rightskull
		AddScore 110
		LastSwitchHit = "RightSlingshot"
	End Sub

	RightSlingShot1.enabled = 0
	Sub rightskull
		RightSlingShot1.enabled = 1
	End Sub


	Sub RightSlingShot1_Timer
		Select Case RStep
			Case 1:RightSling4.Visible = 0:RightSling3.Visible = 1
			Case 2:RightSling3.Visible = 0:RightSling2.Visible = 1
			Case 3:RightSling2.Visible = 0:RightSlingShot1.Enabled = 0
		End Select
		RStep = RStep + 1
	End Sub


	skullrighttime.enabled = 0
	Sub rightskullpop
		skullrighttime.enabled = 1
	End Sub
	
	Dim skullrightfull
	skullrightfull = false

	Sub skullrighttime_Timer
		If skullrightfull = false Then
			skullright.RotZ = (skullright.RotZ - 1) Mod 360
			If skullright.RotZ = 230 Then
				skullrightfull = true
			End If
		Else
			skullright.RotZ = (skullright.RotZ + 1) Mod 360
			If skullright.RotZ = 250 Then
				skullrightfull = false
				skullrighttime.enabled = 0
			End If
		End If
	End Sub

	Sub sw22_hit()
		AddScore 500
		LastSwitchHit = "sw22"
	End Sub 
	Sub sw23_hit()
		AddScore 500
		LastSwitchHit = "sw23"
	End Sub 
	Sub Gate12_Hit()
		if LastSwitchHit="sw23" then 		' Death Save 
			checkModeProgress("deathsave")
			PlaySound "toasty"
			AddScore 10000
				ShowMsg "Death save!", "10,000" 'FormatScore(BonusPoints(CurrentPlayer))
		End if
		if LastSwitchHit="gloryt1_hit" then 		' Death Save 
			checkModeProgress("glory")
		End if
	End Sub 
	Sub Gate13_Hit()
		if LastSwitchHit="sw22" then 		' Death Save 
			checkModeProgress("deathsave")
			PlaySound "nope"
			AddScore 10000
				ShowMsg "Death save!", "10,000" 'FormatScore(BonusPoints(CurrentPlayer))
		End if
		if LastSwitchHit="gloryt2_hit" then 		' Death Save 
			checkModeProgress("glory")
		End if		
	End Sub 

	Sub gloryt1_hit
		LastSwitchHit="gloryt1_hit"
		counttheglory
	End Sub

	Sub gloryt2_hit
		LastSwitchHit="gloryt2_hit"
		counttheglory
	End Sub

	Sub counttheglory
		AddScore 10000
				'ShowMsg "Glory!", "10,000" 'FormatScore(BonusPoints(CurrentPlayer))
		glorycount = glorycount + 1
		pDMDSplashBig glorycount, 2, 8388863
		Select Case glorycount
			Case 1
				'mysteryflash.opacity = 2000
				PlayerState(CurrentPlayer).ismysteryon = True
			Case 4
				'mysteryflash.opacity = 2000
				PlayerState(CurrentPlayer).ismysteryon = True
			Case 10
				'mysteryflash.opacity = 2000
				PlayerState(CurrentPlayer).ismysteryon = True
			Case 20
				'mysteryflash.opacity = 2000
				PlayerState(CurrentPlayer).ismysteryon = True
			Case 30
				'mysteryflash.opacity = 2000
				PlayerState(CurrentPlayer).ismysteryon = True
		End Select
	End Sub


	Sub RotateLaneLightsLeft
		Dim TempState
		TempState = fish1.State
		fish1.State = fish2.State
		fish2.State = fish3.State
		fish3.State = TempState
	End Sub

	Sub RotateLaneLightsRight
		Dim TempState
		TempState = fish3.State
		fish3.State = fish2.State
		fish2.State = fish1.State
		fish1.State = TempState
	End Sub


	Sub DropTargets
' Not sure whant these were suppose to do
'		cheese1.IsDropped = 1
'		cheese2.IsDropped = 1
'		cheese3.IsDropped = 1
		'addflash.opacity = 2000
	End Sub


'*************************************
' I dont think these are used any more 
'	Sub Cheese1_hit
'		targetpointscore
'		targetlightup
'		If ecl2.state = 2 Then	
'			pizzamodelights
'		End If
'	End Sub
'
'	Sub Cheese2_hit
'		targetpointscore
'		targetlightup
'		If ecl2.state = 2 Then	
'			pizzamodelights
'		End If
'	End Sub
'
'	Sub Cheese3_hit
'		targetpointscore
'		If addaballmade = False Then
'			aablight.state = 2
'			aablighton(CurrentPlayer) = True
'		End If
'		targetlightup
'		If ecl2.state = 2 Then	
'			pizzamodelights
'		End If
'		If pizzamulti = False Then
'			addaballgate.open = True
'		End If
'		addflash.opacity = 2000
'	End Sub
' End I dont think these are used anymore 
'*****************************************

	Sub Bumper3_hit
		BonusProgress(kBonus_Bumpers)
		AddScore 50
		countbumps
		SModeUpdate
        PlaySoundAt SoundFXDOF("LeftJet", 107, DOFPulse, DOFContactors), Bumper3
			DOF 302, DOFPulse   'DOF MX - Bumper 1
			DOF 401, DOFPulse    'DOF RGB 
		PlaySound "LeftJet"
		BumperSequence.enabled = 1
		pepcap1.objrotz = pepcap1.objrotz + 10
	End Sub

	Sub Bumper1_hit
		BonusProgress(kBonus_Bumpers)
		AddScore 50
		countbumps
		SModeUpdate
        PlaySoundAt SoundFXDOF("BottomJet", 108, DOFPulse, DOFContactors), Bumper1
			DOF 303, DOFPulse   'DOF MX - Bumper 2
			DOF 402, DOFPulse	'DOF RGB
		PlaySound "BottomJet"
		BumperSequence.enabled = 1
		pepcap2.objrotz = pepcap2.objrotz + 10
	End Sub

	Sub Bumper4_hit
		BonusProgress(kBonus_Bumpers)
		AddScore 50
		countbumps
		SModeUpdate
		PlaySoundAt SoundFXDOF("RightJet", 109, DOFPulse, DOFContactors), Bumper4
			DOF 304, DOFPulse   'DOF MX - Bumper 3
			DOF 403, DOFPulse	'DOF RGB
		PlaySound "RightJet"
		BumperSequence.enabled = 1
		pepcap3.objrotz = pepcap3.objrotz + 10
	End Sub

	Sub r8_hit
		Smallpoints
		countbumps
		PlaySound "LeftJet"
		BumperSequence.enabled = 1
		pepcap3.objrotz = pepcap3.objrotz + 10
	End Sub

	
	Sub countbumps
		playsfx("bump")
		ronibumps = ronibumps + 1
			Select Case ronibumps
			case 1
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 2
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 3
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 4
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 5
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 6
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 7
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 8
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 9
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 10
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 11
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 12
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
			case 13
			'DMD "black.png", "Pepperonis Collected", ronibumps & "/15",  100
				ronibumps = 0
				addaroni
			End Select
	End Sub
	
	Sub addaroni
		totalronis=totalronis + 1
Debug.print "Add Pepperoni:" & totalronis
		checkModeProgress("pepperonititle")
		targetlightup
		If StackState(kStack_Pri0).TargetState(pepperonititle) = 2 Then
			Select Case totalronis
				Case 0
				Case 1
	'				If roni1.state = 2 Then
	'					roni1.state = 1
	'				End If
					pizzamodelights
				Case 2
	'				If roni2.state = 2 Then
	'					roni2.state = 1
	'				End If
					pizzamodelights
				Case 3
	'				If roni3.state = 2 Then
	'					roni3.state = 1
	'				End If
					pizzamodelights
					totalronis = 0
			End Select
		End if 
	End Sub


	Dim bumperpos
	bumperpos = 0
	BumperSequence.enabled = 0
	Sub BumperSequence_Timer 'so what is to happen when we start that sequence, well this is
		bumperpos = bumperpos + 1 'this advances the series
		Select Case bumperpos
			Case 0
			bumps1.state = 1
			bumps2.state = 1
			Case 1
			bumps1.state = 0
			bumps2.state = 0
			Case 2
			bumps1.state = 1
			bumps2.state = 1
			Case 3
			bumps1.state = 0
			bumps2.state = 0
			Case 4
			bumps1.state = 1
			bumps2.state = 1
			Case 5
			bumps1.state = 0
			bumps2.state = 0
			Case 6
			bumps1.state = 1
			bumps2.state = 1
			Case 7
			bumps1.state = 0
			bumps2.state = 0
			bumperpos = 0
			BumperSequence.enabled = 0
		End Select
	End Sub


	
	spiralcaps.enabled = 0
	Sub spiralspin
		spiralcaps.enabled = 1
	End Sub

	Sub spiralcaps_Timer
		spiralcap.ObjRotZ = (spiralcap.ObjRotZ - 1) Mod 360
		If spiralcap.ObjRotZ = -359 Then
			spiralcaps.enabled = 0
		End If
	End Sub

	Sub Bumper5_hit
		if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
			lightrun green,clockleft,1
		end if
		PlaySoundAt SoundFXDOF("BottomJet", 111, DOFPulse, DOFContactors), Bumper5
			DOF 306, DOFPulse   'DOF MX - spiralspin
			DOF 406, DOFPulse	'DOF RGB spiral
			DOF 407, DOFPulse	'DOF Beacon
		playsfx("deathbumper")
		checkModeProgress("bumper")
		AddScore 600
		PlaySound "fx_Bumper3"
		'PlaySound "BottomJet"
		spiralspin
		tipjar
	End Sub


	Sub tipjar
		if gottips then exit sub 		' We already got out tip, nothing to do

		if tips < 6 and pizzamulti=False and ufomulti=False and bWizardModeSupreme=False then
			tips = tips + 1
			tipjarrl.state = 0
			tipjaral.state = 0
			tipjarjl.state = 0
			tipjarpl.state = 0
			tipjaril.state = 0
			tipjartl.state = 0
			Select Case tips
				Case 0
				Case 1	
					tipjartl.state = 1
				Case 2	
					tipjaril.state = 1
				Case 3	
					tipjarpl.state = 1
				Case 4	
					tipjarjl.state = 1
				Case 5	
					tipjaral.state = 1
				Case 6	
					tipjarrl.state = 2
					tipjaral.state = 2
					tipjarjl.state = 2
					tipjarpl.state = 2
					tipjaril.state = 2
					tipjartl.state = 2
				ShowMsg "tip jar ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
					Dim waittime
					waittime = 1000
					vpmtimer.addtimer waittime, "tiplock'"
				Case 7	
					tipjarrl.state = 2
					tipjaral.state = 2
					tipjarjl.state = 2
					tipjarpl.state = 2
					tipjaril.state = 2
					tipjartl.state = 2
				ShowMsg "tip jar ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
					'Dim waittime
					waittime = 1000
					vpmtimer.addtimer waittime, "tiplock'"
				Case 8	
					tipjarrl.state = 2
					tipjaral.state = 2
					tipjarjl.state = 2
					tipjarpl.state = 2
					tipjaril.state = 2
					tipjartl.state = 2
				ShowMsg "tip jar ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
					'Dim waittime
					waittime = 1000
					vpmtimer.addtimer waittime, "tiplock'"
				Case 9	
					tipjarrl.state = 2
					tipjaral.state = 2
					tipjarjl.state = 2
					tipjarpl.state = 2
					tipjaril.state = 2
					tipjartl.state = 2
				ShowMsg "tip jar ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
					'Dim waittime
					waittime = 1000
					vpmtimer.addtimer waittime, "tiplock'"
			End Select
		End if
	End Sub

	Sub tiplock
		Magnet.MagnetON = True
		tipjark.enabled = 1
	End sub

	Sub tipoff
		Magnet.MagnetON = False
		tipjark.enabled = 0
	End sub

	Sub tipjark_hit
		AddScore 40000
					'playmedia "","audio-tipcollected",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "","crust-tip",pBackglass,"cineon",3000,"tipkickout",1,3  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				ShowMsg "tip jar collected", "40,000" 'FormatScore(BonusPoints(CurrentPlayer))
								backlamp "flash"
								flasherspop green,"bottom" 'left,right,bottom,rightkick,top,crazy
								lightrun darkgreen,arctru,2
		If pizzaorder.state = 2 Then			' We are waiting to place an order add tipped to the next level
			If pz1title.state = 2 Then
				tipped2.state = 1
			End If
			If pz2title.state = 2 Then
				tipped3.state = 1
			End If
			If pz3title.state = 2 Then
				tipped4.state = 1
			End If
			If pz4title.state = 2 Then
				tipped1.state = 1
			End If
		Else									' Add it to this level
			If pz1title.state = 2 Then
				tipped1.state = 1
			End If
			If pz2title.state = 2 Then
				tipped2.state = 1
			End If
			If pz3title.state = 2 Then
				tipped3.state = 1
			End If
			If pz4title.state = 2 Then
				tipped4.state = 1
			End If			
		End If 
'
'
'		If pizzaorder.state = 2 Then			' We are waiting to place an order add tipped to the playfield (Waht does this mean?)
'			If pz1title.state = 2 Then
'				tipped2.state = 1
'			End If
'			If pz2title.state = 2 Then
'				tipped3.state = 1
'			End If
'			If pz3title.state = 2 Then
'				tipped4.state = 1
'			End If
'			If pz4title.state = 2 Then
'				tipped1.state = 1
'			End If
'		Else 
'			If pizzamulti = true Then		' Adds a multiball if you do this during pizza time
'				PlaySoundAtVol "vo_addaball", VolDef
'				AddMultiball 1
'				If doublepoints = False Then
'					doublepoints = True
'					twoxpoints.state = 2
'				Else
'					triplepoints = True
'					threexpoints.state = 2
'				End If
'				If pz1title.state = 2 Then
'					tipped1.state = 1
'				End If
'				If pz2title.state = 2 Then
'					tipped2.state = 1
'				End If
'				If pz3title.state = 2 Then
'					tipped3.state = 1
'				End If
'				If pz4title.state = 2 Then
'					tipped4.state = 1
'				End If
'			Else
'				If pz1title.state = 2 Then
'					tipped1.state = 1
'				End If
'				If pz2title.state = 2 Then
'					tipped2.state = 1
'				End If
'				If pz3title.state = 2 Then
'					tipped3.state = 1
'				End If
'				If pz4title.state = 2 Then
'					tipped4.state = 1
'				End If
'			End If
'		End If

		Magnet.MagnetON = False
		'vpmtimer.addtimer 1300, "tipkickout'"
		Playsound "fx_hole-enter"

	End Sub

	Sub tipkickout
		tipjark.DestroyBall

		BallsOnPlayfield = BallsOnPlayfield - 1
		Playsound "fx_hole1"
		vpmtimer.addtimer 700, "tipkickout2 '"
		SpecialProgress(kSpecial_Tip)
		resettips
		gottips = true
	End Sub

	Sub tipkickout2
		bAutoPlunger=True
debug.print "Tip Kick"
		AddMultiball 1
		'CreateNewBall				' Use this instead because it starts autoplunger when in multiball
		'BallRelease.CreateBall
		'BallRelease.Kick 175, 45
	End Sub

	Sub resettips
		Magnet.MagnetON = False
		tipjark.enabled = 0
		tips = 0
		tipjartl.state = 0
		tipjaril.state = 0
		tipjarpl.state = 0
		tipjarjl.state = 0
		tipjaral.state = 0	
		tipjarrl.state = 0
	End Sub

'////Ingredients
	Sub pinet_hit
		playsfx("hittarget")
		targetpointscore
		checkModeProgress("pinetitle")
		DOF 320, DOFPulse 'MX-Pine
		DOF 420, DOFPulse 'rgb
		If StackState(kStack_Pri0).TargetState(pinetitle) = 2 Then
			targetlightup
		
			if pizzamodelights then 
				If pinel1.state + pinel2.state = 1 Then
					pinel2.state = 1
					'SSetLightColor kStack_Pri0, pinetitle, white, 0 		' Stop progress when full
				End If
				If pinel1.state = 0 Then
					pinel1.state = 1
				End If
			End if 
		End if 
	End Sub

	Sub olivet_hit
		playsfx("hittarget")
		targetpointscore
		checkModeProgress("olivetitle")
		DOF 321, DOFPulse 'MX olivet
		DOF 421, DOFPulse 'rgb
		If StackState(kStack_Pri0).TargetState(olivetitle) = 2 Then
			targetlightup
			if pizzamodelights then 

				If olivel1.state + olivel2.state = 1 Then
					olivel2.state = 1
					'SSetLightColor kStack_Pri0, olivetitle, white, 0 		' Stop progress when full
				End If
				If olivel1.state = 0 Then
					olivel1.state = 1
				End If
			End if 
		End If 
	End Sub

	Sub musht_hit
		playsfx("hittarget")
		targetpointscore
		checkModeProgress("mushtitle")
		DOF 322, DOFPulse 'MX-Mush
		DOF 422, DOFPulse 'rgb
		If StackState(kStack_Pri0).TargetState(mushtitle) = 2 Then
			targetlightup
			if pizzamodelights then 
				If mushl1.state + mushl2.state + mushl3.state = 2 Then
					mushl3.state = 1
					'SSetLightColor kStack_Pri0, mushtitle, white, 0 		' Stop progress when full
				End If
				If mushl1.state + mushl2.state + mushl3.state = 1 Then
					mushl2.state = 1
				End If
				If mushl1.state = 0 Then
					mushl1.state = 1
				End If
			End if 
		End if 
	End Sub

	Sub saust_hit
		playsfx("hittarget")
		targetpointscore
		checkModeProgress("saustitle")
		DOF 323, DOFPulse 'MX-Saust
		DOF 423, DOFPulse 'rgb
		If StackState(kStack_Pri0).TargetState(saustitle) = 2 Then
			targetlightup
			if pizzamodelights then 
				If sausl1.state + sausl2.state + sausl3.state = 2 Then
					sausl3.state = 1
					'SSetLightColor kStack_Pri0, saustitle, white, 0 		' Stop progress when full
				End If
				If sausl1.state + sausl2.state + sausl3.state = 1 Then
					sausl2.state = 1
				End If
				If sausl1.state = 0 Then
					sausl1.state = 1
				End If
			End if 
		End if 
	End Sub

	Sub pept_hit
		playsfx("hittarget")
		targetpointscore
		checkModeProgress("peptitle")
		DOF 324, DOFPulse 'MX-Pept
		DOF 424, DOFPulse 'rgb
		If StackState(kStack_Pri0).TargetState(peptitle) = 2 Then
			targetlightup
			if pizzamodelights then 
				If pepl1.state + pepl2.state + pepl3.state = 2 Then
					pepl3.state = 1
					'SSetLightColor kStack_Pri0, peptitle, white, 0 		' Stop progress when full
				End If
				If pepl1.state + pepl2.state + pepl3.state = 1 Then
					pepl2.state = 1
				End If
				If pepl1.state = 0 Then
					pepl1.state = 1
				End If
			End if 
		End if 
	End Sub

	Sub bacont_hit
		playsfx("hittarget")
		targetpointscore
		checkModeProgress("bacontitle")
		DOF 325, DOFPulse 'MX-bacon
		DOF 425, DOFPulse 'rgb
		If StackState(kStack_Pri0).TargetState(bacontitle) = 2 Then
			targetlightup
			if pizzamodelights then 
				If baconl1.state + baconl2.state = 1 Then
					baconl2.state = 1
					'SSetLightColor kStack_Pri0, bacontitle, white, 0 		' Stop progress when full
				End If
				If baconl1.state = 0 Then
					baconl1.state = 1
				End If
			End if 
		End if 
	End Sub

	Sub fish1t_hit
		playsfx("lane")
		AddScore 200
		checkModeProgress("anchtitle")
		If StackState(kStack_Pri0).TargetState(anchtitle) = 2 Then
			targetlightup
			pizzamodelights
		End if
		fish1.state = 1
	End Sub

	Sub fish2t_hit
		playsfx("lane")
		AddScore 200
		checkModeProgress("anchtitle")
		If StackState(kStack_Pri0).TargetState(anchtitle) = 2 Then
			targetlightup
			pizzamodelights
		End if
		fish2.state = 1
	End Sub

	Sub fish3t_hit
		playsfx("lane")
		AddScore 200
		checkModeProgress("anchtitle")
		If StackState(kStack_Pri0).TargetState(anchtitle) = 2 Then
			targetlightup
			pizzamodelights
		End if
		fish3.state = 1
	End Sub

	Sub looplights							' Lights up the right beer
debug.print "Loop Bonus:" & loopbonus
		loopbonus = loopbonus + 1
		Select Case loopbonus
			Case 1
				loopx1.state = 0
				loopx2.state = 0
				loopx3.state = 0
				loopx4.state = 0
				loopx5.state = 1
			Case 2
				loopx4.state = 1
			Case 3
				loopx3.state = 1
			Case 4
				loopx2.state = 1
			Case 5
				loopx1.state = 1
				if bonuslight5.state <> 2 then loopbonus = 0		' Dont reset once we get to 6x
				bonusadd
		End Select
	End Sub


	Sub targetlightup							' Lights up the Left Beer 
debug.print "Target Bonus:" & targetbonuses
		targetbonuses = targetbonuses + 1
		Select Case targetbonuses
			Case 1
				targetx1.state = 0
				targetx2.state = 0
				targetx3.state = 0
				targetx4.state = 0
				targetx5.state = 1
			Case 2
				targetx4.state = 1
			Case 3
				targetx3.state = 1
			Case 4
				targetx2.state = 1
			Case 5
				targetx1.state = 1
				if bonuslight5.state <> 2 then targetbonuses = 0		' Dont reset once we get to 6x
				bonusadd
		End Select
	End Sub

	Dim bonuses
	Sub bonusadd
		ShowMsg "bonus level added", "" 'FormatScore(BonusPoints(CurrentPlayer))
		if bonuses < 6 then 
			BonusMultiplier(CurrentPlayer)=BonusMultiplier(CurrentPlayer)+1
			bonuses = bonuses + 1
			SpecialProgress(kSpecial_Multiplier)
			Select Case bonuses
				Case 1
					bonuslight1.state = 1
				Case 2
					bonuslight2.state = 1
				Case 3
					bonuslight3.state = 1
				Case 4
					bonuslight4.state = 1
				Case 5
					bonuslight5.state = 1
				Case 6							' Beer Frenzy Ready 
					bonuslight1.state = 2
					bonuslight2.state = 2
					bonuslight3.state = 2
					bonuslight4.state = 2
					bonuslight5.state = 2
			End Select
		End if 

		' If LeftBeer (Loop) is at 1000 and  Right beer (Targets) is at 1000 and and we have the 6x Bonus is flashing (Filled 5 beers)
		if loopx1.state = 1 and targetx1.state = 1 and bonuslight5.state = 2 then ' StartBeerFrenzy 
			playmedia "","audio-beerfrenzy",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "beer-frenzy.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		ShowMsg "Beer Frenzy!", "2x" 'FormatScore(BonusPoints(CurrentPlayer))
								backlamp "run"
			' Setup multiplier 
			If doublepoints = False Then
				tmrBeerFrenzy.UserValue = 20
				doublepoints = True
				twoxpoints.state = 2
			Else
				tmrBeerFrenzy.UserValue = 40
				triplepoints = True
				threexpoints.state = 2
			End If
			tmrBeerFrenzy.Enabled = True
		End if 

	End Sub

	Sub StopBeerFrenzy()
				'ShowMsg "beer frenzy complete", "" 'FormatScore(BonusPoints(CurrentPlayer))
		if tmrBeerFrenzy.Enabled then 
			tmrBeerFrenzy.Enabled = False 
			threexpoints.state = 0
			twoxpoints.state = 0
			doublepoints = False
			triplepoints = False 
			targetbonuses=0
			loopbonus=0
			
			targetx1.state = 0
			targetx2.state = 0
			targetx3.state = 0
			targetx4.state = 0
			targetx5.state = 0

			loopx1.state = 0
			loopx2.state = 0
			loopx3.state = 0
			loopx4.state = 0
			loopx5.state = 0

			If PlayerState(CurrentPlayer).Specialties(kSpecial_LoopDeLoop) = 0 then 		' Reset the Loops if this is 0
				PlayerState(CurrentPlayer).Specialties(kSpecial_LoopDeLoop) = 20	 
				puPlayer.LabelSet pBackglass,"CollectVal6",	PlayerState(CurrentPlayer).Specialties(kSpecial_LoopDeLoop)	,1,""
			End If 

			Select case CurrentPlayer
				case 0:
					puPlayer.LabelSet pBackglass,"Play1scoreX", "PuPOverlays\\Pizza2x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':1}"
					puPlayer.LabelSet pBackglass,"Play1scoreXTime", "", 0,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':12.2}"
				case 1:
					puPlayer.LabelSet pBackglass,"Play2scoreX", "PuPOverlays\\Pizza2x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':25}"
					puPlayer.LabelSet pBackglass,"Play2scoreXTime", "", 0,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':36.1}"
				case 2:
					puPlayer.LabelSet pBackglass,"Play3scoreX", "PuPOverlays\\Pizza2x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':53}"
					puPlayer.LabelSet pBackglass,"Play3scoreXTime", "", 0,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':64.1}"
				case 3:
					puPlayer.LabelSet pBackglass,"Play4scoreX", "PuPOverlays\\Pizza2x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':77}"
					puPlayer.LabelSet pBackglass,"Play4scoreXTime", "", 0,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':88}"
			End Select
		End if 
	End Sub 

	Sub tmrBeerFrenzy_Timer()
		Dim img
		tmrBeerFrenzy.UserValue = tmrBeerFrenzy.UserValue-1

		if tmrBeerFrenzy.UserValue <= 5 and tmrBeerFrenzy.UserValue >=0 then 
			PlaySound "so_timer"
		End if 

		if tmrBeerFrenzy.UserValue = 0 then
			StopBeerFrenzy()
		Else
			if triplepoints then 
				img="PuPOverlays\\Pizza3x.png"
			Else 
				img="PuPOverlays\\Pizza2x.png"
			End if 
			Select case CurrentPlayer
				case 0:
					puPlayer.LabelSet pBackglass,"Play1scoreX", img, 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':1}"
					puPlayer.LabelSet pBackglass,"Play1scoreXTime", tmrBeerFrenzy.UserValue, 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':12.2}"
				case 1:
					puPlayer.LabelSet pBackglass,"Play2scoreX", img, 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':25}"
					puPlayer.LabelSet pBackglass,"Play2scoreXTime", tmrBeerFrenzy.UserValue, 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':36.1}"
				case 2:
					puPlayer.LabelSet pBackglass,"Play3scoreX", img, 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':53}"
					puPlayer.LabelSet pBackglass,"Play3scoreXTime", tmrBeerFrenzy.UserValue, 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':64.1}"
				case 3:
					puPlayer.LabelSet pBackglass,"Play4scoreX", img, 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':77}"
					puPlayer.LabelSet pBackglass,"Play4scoreXTime", tmrBeerFrenzy.UserValue, 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':88}"
			End Select
		End If 
	End Sub 

	dim roready
	Dim lolights
	Sub LOdone_hit
		if ufomulti Then		' We are in UFO Multiball (and not PizzaTime), Deliver Pizzas
			ufoPizzasCollected=ufoPizzasCollected+1
			ufostacks
			BeamFlash.opacity = 100
			FlasherSequence.Enabled = True
			playsfx("beam")
			'PlaySound "ro2"		' TBD Change 
			ShowMsg "Pizzas Collected", ufoPizzasCollected
			playsfx("pickup")
			flasherspop darkgreen,"top"
			lightrun green,screwr,2
			playmedia "","audio-igmbpizzacollect",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'playmedia "beer-frenzy.mp4","video-scenes",pBackglass,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End if 
		if bWizardModeAte Then JackpotScore

		if bSuperSkillShotEnabled then 	' Light the scoop
			LightSeqSuperSkill.UpdateInterval = 5
			LightSeqSuperSkill.Play SeqDiagUpRightOn, 30, 100
		End If 

		checkModeProgress("leftloop")
		looplights
		roready = false
		lolights = lolights + 1
		looppointscore
		Select Case lolights
			Case 0
				lostar1.state = 0
				lostar2.state = 0
				lostar3.state = 0
				lostar4.state = 0
				lostar5.state = 0
				lostar6.state = 0
			Case 1
				lostar1.state = 1
				lostar2.state = 0
				lostar3.state = 0
				lostar4.state = 0
				lostar5.state = 0
				lostar6.state = 0
			Case 2
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 0
				lostar4.state = 0
				lostar5.state = 0
				lostar6.state = 0
			Case 3
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 0
				lostar5.state = 0
				lostar6.state = 0
			Case 4
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 1
				lostar5.state = 0
				lostar6.state = 0
			Case 5
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 1
				lostar5.state = 1
				lostar6.state = 0
			Case 6
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 1
				lostar5.state = 1
				lostar6.state = 1
			Case 7
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 1
				lostar5.state = 1
				lostar6.state = 2
			Case 8
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 1
				lostar5.state = 2
				lostar6.state = 2
			Case 9
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 1
				lostar4.state = 2
				lostar5.state = 2
				lostar6.state = 2
			Case 10
				lostar1.state = 1
				lostar2.state = 1
				lostar3.state = 2
				lostar4.state = 2
				lostar5.state = 2
				lostar6.state = 2
			Case 11
				lostar1.state = 1
				lostar2.state = 2
				lostar3.state = 2
				lostar4.state = 2
				lostar5.state = 2
				lostar6.state = 2
			Case 12
				lostar1.state = 2
				lostar2.state = 2
				lostar3.state = 2
				lostar4.state = 2
				lostar5.state = 2
				lostar6.state = 2

		End Select
	End Sub


	roready = false
	Sub rostart_hit
		roready = True
	End Sub

	Sub loindone_hit
		roready = False
	End Sub
	
	Dim rolights
	rolights = 0
	Sub rodone_hit
		if bWizardModeAte Then JackpotScore
		If roready = True Then			
			checkModeProgress("rightloop")
			looplights
			rolights = rolights + 1
			looppointscore
			Select Case rolights
			Case 1
				ropizza1.state = 1
				ropizza2.state = 0
				ropizza3.state = 0
			Case 2
				ropizza1.state = 1
				ropizza2.state = 1
				ropizza3.state = 0
			Case 3
				ropizza1.state = 1
				ropizza2.state = 1
				ropizza3.state = 1
			Case 4
				ropizza1.state = 1
				ropizza2.state = 1
				ropizza3.state = 2
			Case 5
				ropizza1.state = 1
				ropizza2.state = 2
				ropizza3.state = 2
			Case 6
				ropizza1.state = 2
				ropizza2.state = 2
				ropizza3.state = 2
			End Select
		End If
		roready = false
	End Sub
		


'////UFO DELIVERY


	Sub ufo1_hit
		ufo1l.state = 1
		AddScore 150
		checkufos
		spinufo1
		DOF 326, DOFPulse 'Mx-Ufo1
		DOF 426, DOFPulse 'Rgb 
	End Sub

	Sub spinufo1
		ufo1time.enabled = 1
	End Sub

	Dim ufo1pos:ufo1pos = 0
	Sub ufo1time_timer
		If ufo1pos = 0 Then
			If ufo1m.z = 190 Then
				ufo1pos = 1
			Else
				ufo1m.z = ufo1m.z + 1
				ufo1m.ObjRotZ = ufo1m.ObjRotZ  + 4.5
			End If
		Else
			If ufo1m.z = 110 Then
				ufo1pos = 0
				ufo1time.enabled = false				
			Else
				ufo1m.z = ufo1m.z - 1
				ufo1m.ObjRotZ = ufo1m.ObjRotZ  + 4.5
			End If
		End If
	End Sub





	Sub ufo2_hit
		ufo2l.state = 1
		AddScore 150
		checkufos
		spinufo2
		DOF 327, DOFPulse 'MX-Ufo2
		DOF 427, DOFPulse 'Rgb
	End Sub

	Sub spinufo2
		ufo2time.enabled = 1
	End Sub

	Dim ufo2pos:ufo2pos = 0
	Sub ufo2time_timer
		If ufo2pos = 0 Then
			If ufo2m.z = 190 Then
				ufo2pos = 1
			Else
				ufo2m.z = ufo2m.z + 1
				ufo2m.ObjRotZ = ufo2m.ObjRotZ  + 4.5
			End If
		Else
			If ufo2m.z = 110 Then
				ufo2pos = 0
				ufo2time.enabled = false				
			Else
				ufo2m.z = ufo2m.z - 1
				ufo2m.ObjRotZ = ufo2m.ObjRotZ  + 4.5
			End If
		End If
	End Sub




	Sub ufo3_hit
		ufo3l.state = 1
		AddScore 150
		checkufos
		spinufo3
		DOF 328, DOFPulse 'MX-ufo3
		DOF 428, DOFPulse 'RGB
	End Sub

	Sub spinufo3
		ufo3time.enabled = 1
	End Sub

	Dim ufo3pos:ufo3pos = 0
	Sub ufo3time_timer
		If ufo3pos = 0 Then
			If ufo3m.z = 190 Then
				ufo3pos = 1
			Else
				ufo3m.z = ufo3m.z + 1
				ufo3m.ObjRotZ = ufo3m.ObjRotZ  + 4.5
			End If
		Else
			If ufo3m.z = 110 Then
				ufo3pos = 0
				ufo3time.enabled = false				
			Else
				ufo3m.z = ufo3m.z - 1
				ufo3m.ObjRotZ = ufo3m.ObjRotZ  + 4.5
			End If
		End If
	End Sub





	Sub ufo4_hit
		ufo4l.state = 1
		AddScore 150
		checkufos
		spinufo4
		DOF 329, DOFPulse 'MX-ufo4
		DOF 429, DOFPulse 'rgb
	End Sub

	Sub spinufo4
		ufo4time.enabled = 1
	End Sub

	Dim ufo4pos:ufo2pos = 0
	Sub ufo4time_timer
		If ufo4pos = 0 Then
			If ufo4m.z = 190 Then
				ufo4pos = 1
			Else
				ufo4m.z = ufo4m.z + 1
				ufo4m.ObjRotZ = ufo4m.ObjRotZ  + 4.5
			End If
		Else
			If ufo4m.z = 110 Then
				ufo4pos = 0
				ufo4time.enabled = false				
			Else
				ufo4m.z = ufo4m.z - 1
				ufo4m.ObjRotZ = ufo4m.ObjRotZ  + 4.5
			End If
		End If
	End Sub







	Sub ufo5_hit
		ufo5l.state = 1
		AddScore 150
		checkufos
		spinufo5
		DOF 330, DOFPulse 'MX-ufo5
		DOF 430, DOFPulse 'rgb
	End Sub

	Sub spinufo5
		ufo5time.enabled = 1
	End Sub

	Dim ufo5pos:ufo5pos = 0
	Sub ufo5time_timer
		If ufo5pos = 0 Then
			If ufo5m.z = 190 Then
				ufo5pos = 1
			Else
				ufo5m.z = ufo5m.z + 1
				ufo5m.ObjRotZ = ufo5m.ObjRotZ  + 4.5
			End If
		Else
			If ufo5m.z = 110 Then
				ufo5pos = 0
				ufo5time.enabled = false				
			Else
				ufo5m.z = ufo5m.z - 1
				ufo5m.ObjRotZ = ufo5m.ObjRotZ  + 4.5
			End If
		End If
	End Sub


	dim ufocall:ufocall=0
	Sub checkufos
		playsfx("standup")
		if pizzamulti = False and ufomulti=False and bMultiBallMode=False then		' cant start UFO during MB 
			If ufo1l.state + ufo2l.state + ufo3l.state + ufo4l.state + ufo5l.state = 5 Then
				ufolocklight.state = 2
				ufolightlock = true
				DOF 338, DOFOn 'MX-UFO
				DOF 438, DOFPulse 'RGB
				DOF 111, DOFPulse 'Shaker
				'PlaySound "Lockislit"
				If ufocall=0 then
					if ufolock = 0 Then
						ufocall=1
						playmedia "","audio-locklit",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						playmedia "lock-lit.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						ShowMsg "UFO Lock Ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
					elseif ufolock = 1 Then
						ufocall=1
						playmedia "","audio-igmbready",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						playmedia "ig-mb-ready.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						ShowMsg "UFO Multi Ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
					end If
				end if
				'DMD "black.png", "Lock", "IS LIT",  1000
				BeamFlash.opacity = 100
				playsfx("beam")
			End If
		End If 
	End Sub

	Sub resetufos
		ufo1l.state = 0
		ufo2l.state = 0
		ufo3l.state = 0
		ufo4l.state = 0
		ufo5l.state = 0
	End Sub

	dim giactive:giactive=0
' Overall kicker command post. Going to start all modes and ufo multi here
	' RightScoop 

'START kicker waterfall

	Sub ckick_hit
		bik=bik+1
		if ufocall=1 Then
			ufocall=0
		end if 
			DOF 305, DOFPulse   'DOF MX - ckick_hit
			DOF 404, DOFPulse   'DOF RGB
			DOF 405, DOFPulse	'DOF Strobe	
			DOF 411, DOFPulse
		PlaySound "fx_kicker_enter"
		ckick.DestroyBall
		AddScore 300
		checkskillshot

	end Sub
'------>
		sub checkskillshot
			if bSuperSkillShotEnabled then
				'ShowMsg "Super Skillshot", FormatScore(1000000)
				'pupDMDDisplay "-", "Super Skillshot^" & FormatScore(1000000), "" ,3, 0, 10		'3 seconds
				AddScore 80000
				ShowMsg "super secret skillshot", "80,000" 'FormatScore(BonusPoints(CurrentPlayer))
								backlamp "flash"

					playmedia "","audio-superskillshot",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "super-skillshot.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				tmrResetSuperSkillshot_Timer
			End if
			checksupreme
		end Sub
'---------->
			sub checksupreme
				if bWizardModeSupremeReady then StartWizardSupreme
				checkufo
			end Sub
'-------------->
			dim startingpizza:startingpizza=0
				sub checkufo
					Dim tmpBonus
					If pizzamulti=False then    ' We put this on hold if we are in PizzaTime
						If ufolightlock = true and ufomulti = false and bWizardModeAte=False and bWizardModeIllum=False and bWizardModeSupreme=False Then	' UFO Mode and not in wizard mode or Pizza Mode
							ufolock = ufolock +1
							BonusProgress(kBonus_Locks)
							SpecialProgress(kSpecial_Locks)
							Select Case ufolock
								Case 1
									PlaySound "LeftSlingShot"
									playmedia "","crust-ball1locked",pBackglass,"cineon",2200,"checkpizzaorder",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
									ShowMsg "ball 1 locked", "200" 'FormatScore(BonusPoints(CurrentPlayer))
									AddScore 200
									backlamp "flash"
									resetufos
									ufolightlock = false
									DOF 338, DOFOff 'MX-UFO
									ufolocklight.state = 0
									BeamFlash.opacity = 0
									uforedl.state = 2
									Exit Sub			' Bail out and let the queue take over 
								Case 2
									PlaySound "LeftSlingShot"
									ShowMsg "Hit ship to deliver & cash", ""
									ShowMsg "Hit orbits to get pizzas", ""
									ShowMsg "ufo Multiball", ""
									DOF 338, DOFOff 'MX-UFO
									AddScore 200
									backlamp "flash"
									playmedia "","crust-igmbintro",pBackglass,"cineon",6000,"startufo",1,1
									PlaySoundVol "alien", voldef
									gicolor green
									ufo.enabled = 1
									startingpizza=1
									resetufos
									FlasherSequence.enabled = 1
									ufolock = 0
									ufolightlock = false
									BeamFlash.opacity = 1
									ufolocklight.state = 0
									'uforedl.state = 2
							End Select
						end If
						if ufomulti = true Then		' We are in UFO Multiball, Deliver Pizzas
							startkick
							if ufoPizzasCollected > 0 then 
								ufo.enabled = 0
								tmpBonus=0
								playsfx("deliver")
								playmedia "","audio-jackpot",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
								lightrun green,screwl,2
								flasherspop green,"crazy"
								playmedia "pizza-delivered.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
								if ufoPizzasCollected = 1 then 
									tmpBonus= 150000 * ufoPizzasCollected

								elseif ufoPizzasCollected >=2 and ufoPizzasCollected<=5 Then
									tmpBonus= 250000 * ufoPizzasCollected

								elseif ufoPizzasCollected >=6 and ufoPizzasCollected<=10 Then
									tmpBonus= 350000 * ufoPizzasCollected

								elseif ufoPizzasCollected >=11 then 
									tmpBonus= 500000 * ufoPizzasCollected

								End If
								AddScore tmpBonus
								ShowMsg ufoPizzasCollected & " Delivered ", FormatScore(tmpBonus)
								backlamp "run"
								BeamFlash.opacity = 0
								FlasherSequence.Enabled = False
								ufo.enabled = 1
								ufoPizzasCollected = 0
								ufostacks
							End If
						End If
					End If 
					If startingpizza=0 Then
						checkpizzaorder
					end if
				End Sub
'------------------>
					sub checkpizzaorder
						If pizzaorder.state = 2 and _
							bWizardModeAte=False and bWizardModeIllum=False and bWizardModeSupreme=False Then	' Start Pizza Order and not in wizard mode
debug.print "Start Pizza Order"
							resetingredlights					' Reset all the lights to start the next pizza
							pizzasizemodes
							pizzaspin
							pizzatype
							updatePizzaHitsLeft
		'ShowMsg "New pizza order...", "" 'FormatScore(BonusPoints(CurrentPlayer))
							pizzaorder.state = 0
							uforedl.state = 2
						End If
						checkmodes
					end Sub
'---------------------->
						sub checkmodes			' Start a 2nd waterfall
							if modelight.state = 2 and _
								bWizardModeAte=False and bWizardModeIllum=False and bWizardModeSupreme=False Then	' Specialties Mode and not in wizard mode
								modeintro
							Else
								startkick
							End If
						end Sub
'-------------------------->
							sub startkick
								kicktime.enabled=1
								'Kick Rules
								FlashForms uforedl, 2000, 40, 0
								'vpmtimer.addtimer 2000, "kickitt '"
								'kicksounds
								flasherspop red,"rightkick" 'left,right,bottom,rightkick,top,crazy
							end Sub
'------------------------------>
								sub kicksounds
									playsfx("beep")
									'vpmtimer.addtimer 1000, "beept '"
									'vpmtimer.addtimer 2000, "shoott '"
								end Sub
'---------------------------------->
									sub beep
										playsfx("beep")
									end Sub
'-------------------------------------->
										sub shoot
											playsfx("shoot")
										end sub
'------------------------------------------>
											Sub kickit
												ckick.CreateBall
												ckick.Kick (175 - RndNum(0,6)), (45 - RndNum(0,2))		' Random scatter angle
												if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
													GiOn
												end if
												UfoShake(True)
												PlaySound "fx_kicker"
												DOF 113,DOFPulse
												uforedl.state = 0
											End Sub

	dim bik:bik=0
	dim ktpos:ktpos=0
	sub kicktime_timer
		ktpos=ktpos+1
		select case ktpos
			case 1
				kicksounds
			case 2
				beep
			case 3
				shoot
				kickit
				bik=bik-1
				if bik=0 then
					ktpos=0
					kicktime.enabled=0
				Else
					ktpos=0
				end If
		end Select
	end Sub



'END kicker waterfall

' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Larger Animations
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 

	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	 '-> Pizza Box Open & Close
	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	pizzatimer.enabled = 0
	Sub pizzaopen
	playsfx("boxopen")
debug.print "Setting Pizza"
	If pizzaisopen = false Then
		pizzastrobe 1
		ShowMsg "pizza time ready", "" 'FormatScore(BonusPoints(CurrentPlayer))
		playmedia "","audio-ptready",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		playmedia "pt-ready.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	end if 
		pizzatimer.enabled = 1
	End Sub

	Dim pizzaisopen
	pizzaisopen = false
	Sub pizzatimer_Timer
		If pizzaisopen = true Then
debug.print "Closing Pizza"

			pizzabox.ObjRotY = (pizzabox.ObjRoty - 1) Mod 360
			pizzabox.TransY = (pizzabox.TransY - 1) Mod 360
			pizzabox.TransZ = (pizzabox.TransZ + .5) Mod 360
			If pizzabox.ObjRotY = -60 Then
				pizzatimer.enabled = 0
				pizzastrobe 0
				pizzabox.ObjRotY = (pizzabox.ObjRotY + 1) Mod 360
				pizzaisopen = False
				pizzalocker.enabled = 0
			End If
		Else
			pizzabox.ObjRotY = (pizzabox.ObjRoty + 1) Mod 360
			pizzabox.TransY = (pizzabox.TransY + 1) Mod 360
			pizzabox.TransZ = (pizzabox.TransZ - .5) Mod 360
			If pizzabox.ObjRotY = 0 Then
				pizzatimer.enabled = 0
				pizzabox.ObjRotY = (pizzabox.ObjRotY + 1) Mod 360
				pizzaisopen = True
				pizzalocker.enabled = 1
			End If
debug.print "Opening Pizza"
		End If
	End Sub


	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	 '-> UFO Flashing & Moving
	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	FlasherSequence.enabled = 0 'make sure it's off
	FlasherSequence.Interval = 100

	Dim sequencepos 'this we will use to track the sequence's position
	sequencepos = 0 'so let's make sure it starts at the start or 0
	BeamFlash.opacity = 0 'making my initial flasher transparent
	'FlasherSequence.enabled = True

	Sub FlasherSequence_Timer 'so what is to happen when we start that sequence, well this is
		sequencepos = sequencepos + 1 'this advances the series
		Select Case sequencepos
			Case 0
				BeamFlash.opacity = 0
			Case 1
				BeamFlash.opacity = 20
			Case 2
				BeamFlash.opacity = 40
			Case 3
				BeamFlash.opacity = 60
			Case 4
				BeamFlash.opacity = 80
			Case 5
				BeamFlash.opacity = 100
			Case 6
				BeamFlash.opacity = 100
			Case 7
				BeamFlash.opacity = 100
			Case 8
				BeamFlash.ImageA = "beam4-7a"
				BeamFlash.ImageB = "beam4-7a"
				BeamFlash.opacity = 90
			Case 9
				BeamFlash.ImageA = "beam4-7b"
				BeamFlash.ImageB = "beam4-7b"
				BeamFlash.opacity = 80
			Case 10
				BeamFlash.ImageA = "beam4-7c"
				BeamFlash.ImageB = "beam4-7c"
				BeamFlash.opacity = 70
			Case 11
				BeamFlash.ImageA = "beam4-7a"
				BeamFlash.ImageB = "beam4-7a"
				BeamFlash.opacity = 85
			Case 12
				BeamFlash.ImageA = "beam4-7b"
				BeamFlash.ImageB = "beam4-7b"
				BeamFlash.opacity = 100
				sequencepos = 8
				'FlasherSequence.enabled = 0
		End Select
	End Sub

	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	 '-> Pizza Spinner
	 '->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

	
	pizzaspinner.enabled = 0
	Sub pizzaspin
		ShowMsg "New order in...", "" 'FormatScore(BonusPoints(CurrentPlayer))
		playsfx("pizzawheel")
			pizzawheel.ObjRotY = 0
		If currentpizza = 0 Then
			currentpizza = RndNum(1,8)
			pizzaspinner.enabled = 1
debug.print "Currentpizza:" & currentpizza
								backlamp "run"
			playmedia GetPizzaName() & "-spin.mp4", "video-pizzas", pBackglass, "", 3000, "", 1, 1
			PlaySoundVol "phonering" , 0.1
			vpmtimer.addtimer 2000, "pizzacallin '"
		End If
	End Sub

	sub pizzacallin
			playmedia "", "audio-" & GetPizzaSize() & "-" & GetPizzaName() & "", pCallouts, "", 2000, "", 150, 1
	end Sub

	Function GetPizzaSize()
		GetPizzaSize=""
		Select Case pizzasize
		case 1
			GetPizzaSize="pp"
		case 2
			GetPizzaSize="md"
		case 3
			GetPizzaSize="lg"
		case 4
			GetPizzaSize="bb"
		end Select
	end Function

	' pizzas
	' 1 - Pepperoni - 700
	' 2 - Veggies - 750
	' 3 - Supreme - 795
	' 4 - Meat Lovers - 830
	' 5 - Manhattan - 880
	' 6 - Sea Side - 920
	' 7 - Garden Party - 960
	' 8 - Hawaiian - 1000

	Sub pizzaspinner_Timer
		pizzawheel.ObjRotY = (pizzawheel.ObjRoty - 1) Mod 1080
		DOF 339, DOFPulse 'MX-PizzaSpinner
		DOF 405, DOFPulse 'Strobe
		DOF 439, DOFPulse 'RGB

		If currentpizza = 1 Then
			If pizzawheel.ObjRotY = -700 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Pepperoni"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Pepperoni pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		If currentpizza = 2 Then
			If pizzawheel.ObjRotY = -750 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Veggies"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Veggie pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		If currentpizza = 3 Then
			If pizzawheel.ObjRotY = -795 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Supreme"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Supreme pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		If currentpizza = 4 Then
			If pizzawheel.ObjRotY = -830 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Meat Lovers"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Meat lovers pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		If currentpizza = 5 Then
			If pizzawheel.ObjRotY = -880 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Manhattan"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Manhattan pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		If currentpizza = 6 Then
			If pizzawheel.ObjRotY = -920 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Sea Side"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Sea side pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		If currentpizza = 7 Then
			If pizzawheel.ObjRotY = -960 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Garden Party"	,1,""
				pizzaspinner.enabled = 0
			End If
		End If
		If currentpizza = 8 Then
			If pizzawheel.ObjRotY = -1000 Then
				puPlayer.LabelSet pBackglass,"DetailCollect",	" Hawaiian"	,1,""
				pizzaspinner.enabled = 0
		ShowMsg "Hawaiian pizza", "" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If
		
		If pizzaspinner.enabled = 0 then 
			playmedia GetPizzaName() & "-start.mp4", "video-pizzas", pBackglass, "", 3000, "", 1, 1
		End If

	End Sub

	Sub pizzaspinnerSet
		If currentpizza = 1 Then
			pizzawheel.ObjRotY = -700
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Pepperoni"	,1,""
		End If
		If currentpizza = 2 Then
			pizzawheel.ObjRotY = -750
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Veggies"	,1,""
		End If
		If currentpizza = 3 Then
			pizzawheel.ObjRotY = -795
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Supreme"	,1,""
		End If
		If currentpizza = 4 Then
			pizzawheel.ObjRotY = -830
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Meat Lovers"	,1,""
		End If
		If currentpizza = 5 Then
			pizzawheel.ObjRotY = -880
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Manhattan"	,1,""
		End If
		If currentpizza = 6 Then
			pizzawheel.ObjRotY = -920
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Sea Side"	,1,""
		End If
		If currentpizza = 7 Then
			pizzawheel.ObjRotY = -960
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Garden Party"	,1,""
		End If
		If currentpizza = 8 Then
			pizzawheel.ObjRotY = -1000
			puPlayer.LabelSet pBackglass,"DetailCollect",	" Hawaiian"	,1,""
		End If
	End Sub

' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Modes
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 

	

	Sub pizzasizemodes
debug.print "PizzaSizeModes:" & pizzasize
		Select Case pizzasize
			Case 1
				pz1title.state = 2
				pz2title.state = 0
				pz3title.state = 0
				pz4title.state = 0
			Case 2
				pz1title.state = 1
				pz2title.state = 2
				pz3title.state = 0
				pz4title.state = 0
			Case 3
				pz1title.state = 1
				pz2title.state = 1
				pz3title.state = 2
				pz4title.state = 0
			Case 4
				pz1title.state = 1
				pz2title.state = 1
				pz3title.state = 1
				pz4title.state = 2
		End Select
	End Sub

	Sub updatePizzaHitsLeft()		' Update Progress on Pup
		If ufomulti = true then exit Sub
debug.print "updatePizzaHitsLeft: Size" & pizzasize & " Cnt:" & pzlnum
		SetBGPizza(False)
		Select case pizzasize
			case 1:
				if (pzlnum <=4) then
				'if (pzlnum >0) then ShowMsg "Ingredient collected", "2,000" end if
					puPlayer.LabelSet pBackglass,"DetailCount",	4-pzlnum	,1,""
				End If
			case 2:
				if (pzlnum <=8) then
				'if (pzlnum >0) then ShowMsg "Ingredient collected", "2,000" end if
					puPlayer.LabelSet pBackglass,"DetailCount",	8-pzlnum	,1,""
				End If 
			case 3:
				if (pzlnum <=12) then 
				'if (pzlnum >0) then ShowMsg "Ingredient collected", "2,000" end if
					puPlayer.LabelSet pBackglass,"DetailCount",	12-pzlnum	,1,""
				End If 
			case 4:
				if (pzlnum <=16) then
				'if (pzlnum >0) then ShowMsg "Ingredient collected", "2,000" end if
					puPlayer.LabelSet pBackglass,"DetailCount",	16-pzlnum	,1,""
				End If 
		End Select 
	End Sub 

	Function pizzamodelights
	If ufomulti = true then exit Function
debug.print "PizzaModeLights: " & pzlnum & "," & pzlnum2 & " complete:" & PlayerState(CurrentPlayer).completedBBCount
		if pzlnum2 = PlayerState(CurrentPlayer).completedBBCount then   ' Make things harder next time around when you complete the game 
			'pzlnum = pzlnum +1
			pzlnum2=0
			pizzamodelights=True
		Else 
			pzlnum2=pzlnum2+1
			pizzamodelights=False		' Dont count this one 
debug.print "Add cnt"
			exit Function 
		End if 
				

		' Add score for each ingredient collected 
		If (pizzasize = 1 and pzlnum<4) or _ 
			(pizzasize = 2 and pzlnum<8) or _
			(pizzasize = 3 and pzlnum<12) or _
			(pizzasize = 4 and pzlnum<16) Then
			pzlnum = pzlnum +1
			AddScore 300
		Else
			Exit Function 	' No more to collect 
		End If
		updatePizzaHitsLeft
		
		' Process progress
		Select Case pzlnum
			Case 0
			Case 1
				If pizzasize = 1 Then
					pz1l1.state = 1
				Elseif pizzasize = 2 Then
					pz2l1.state = 1
				Elseif pizzasize = 3 Then
					pz3l1.state = 1
				Elseif pizzasize = 4 Then
					pz4l1.state = 1
				End If
			Case 2
				If pizzasize = 1 Then
					pz1l2.state = 1
				Elseif pizzasize = 2 Then
					pz2l2.state = 1
				Elseif pizzasize = 3 Then
					pz3l2.state = 1
				Elseif pizzasize = 4 Then
					pz4l2.state = 1
				End If
			Case 3
				If pizzasize = 1 Then
					pz1l3.state = 1
				Elseif pizzasize = 2 Then
					pz2l3.state = 1
				Elseif pizzasize = 3 Then
					pz3l3.state = 1
				Elseif pizzasize = 4 Then
					pz4l3.state = 1
				End If
			Case 4
				If pizzasize = 1 Then			' Pizza 1 Complete 
					pz1l4.state = 1
					ptready = True
					StackState(kStack_Pri0).Disable				'Done with the mode 
					RestoreAllLights
					pizzaopen
				Elseif pizzasize = 2 Then
					pz2l4.state = 1
				Elseif pizzasize = 3 Then
					pz3l4.state = 1
				Elseif pizzasize = 4 Then
					pz4l4.state = 1
				End If
			Case 5
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l5.state = 1
				Elseif pizzasize = 3 Then
					pz3l5.state = 1
				Elseif pizzasize = 4 Then
					pz4l5.state = 1
				End If
			Case 6
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l6.state = 1
				Elseif pizzasize = 3 Then
					pz3l6.state = 1
				Elseif pizzasize = 4 Then
					pz4l6.state = 1
				End If
			Case 7
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l7.state = 1
				Elseif pizzasize = 3 Then
					pz3l7.state = 1
				Elseif pizzasize = 4 Then
					pz4l7.state = 1
				End If
			Case 8
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then			' Pizza 2 Complete 
					pz2l8.state = 1
					ptready = True
					StackState(kStack_Pri0).Disable				'Done with the mode 
					RestoreAllLights
					pizzaopen
				Elseif pizzasize = 3 Then
					pz3l8.state = 1
				Elseif pizzasize = 4 Then
					pz4l8.state = 1
				End If
			Case 9
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l9.state = 1
				Elseif pizzasize = 4 Then
					pz4l9.state = 1
				End If
			Case 10
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l10.state = 1
				Elseif pizzasize = 4 Then
					pz4l10.state = 1
				End If
			Case 11
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l11.state = 1
				Elseif pizzasize = 4 Then
					pz4l11.state = 1
				End If
			Case 12
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then			' Pizza 3 complete 
					pz3l12.state = 1
					StackState(kStack_Pri0).Disable				'Done with the mode 
					RestoreAllLights
					pizzaopen
					ptready = True
				Elseif pizzasize = 4 Then
					pz4l12.state = 1
				End If
			Case 13
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l13.state = 1
				End If
			Case 14
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l14.state = 1
				End If
			Case 15
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l15.state = 1
				End If
			Case 16
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then				' Pizza 4 Complete 
					pz4l16.state = 1
					StackState(kStack_Pri0).Disable				'Done with the mode 
					RestoreAllLights
					pizzaopen
					ptready = True
				End If
		End Select
	End Function

	Dim curOffset:curOffset=0
	Sub ShowMsgOffset(offset)
Debug.print "MsgOffset " & curOffset & ":" & offset
		if offset < kCheckSize-5 then 
			curOffset=offset
			puPlayer.LabelSet pBackglass,"Check1", CheckArray(curOffset,0), 1,"{'mt':2,'ypos':65.7}"
			puPlayer.LabelSet pBackglass,"Check2", CheckArray(curOffset+1,0), 1,"{'mt':2,'ypos':69.3}"
			puPlayer.LabelSet pBackglass,"Check3", CheckArray(curOffset+2,0), 1,"{'mt':2,'ypos':72.7}"
			puPlayer.LabelSet pBackglass,"Check4", CheckArray(curOffset+3,0), 1,"{'mt':2,'ypos':76.1}"
			puPlayer.LabelSet pBackglass,"Check5", CheckArray(curOffset+4,0), 1,"{'mt':2,'ypos':79.5}"
			puPlayer.LabelSet pBackglass,"Check6", CheckArray(curOffset+5,0), 1,"{'mt':2,'ypos':82.8}"

			puPlayer.LabelSet pBackglass,"CheckS1", CheckArray(curOffset,1), 1,"{'mt':2,'ypos':65.7}"
			puPlayer.LabelSet pBackglass,"CheckS2", CheckArray(curOffset+1,1), 1,"{'mt':2,'ypos':69.3}"
			puPlayer.LabelSet pBackglass,"CheckS3", CheckArray(curOffset+2,1), 1,"{'mt':2,'ypos':72.7}"
			puPlayer.LabelSet pBackglass,"CheckS4", CheckArray(curOffset+3,1), 1,"{'mt':2,'ypos':76.1}"
			puPlayer.LabelSet pBackglass,"CheckS5", CheckArray(curOffset+4,1), 1,"{'mt':2,'ypos':79.5}"
			puPlayer.LabelSet pBackglass,"CheckS6", CheckArray(curOffset+5,1), 1,"{'mt':2,'ypos':82.8}"		
		End if 
	End Sub 


	Sub ShowMsg(Msg1, Score)
		Dim i
		For i = kCheckSize-1 to 1 Step -1
			CheckArray(i,0) = CheckArray(i-1,0)
			CheckArray(i,1) = CheckArray(i-1,1)
		Next 
		CheckArray(0,0) = " " & Msg1
		CheckArray(0,1) = " " & Score

		puPlayer.LabelSet pBackglass,"Check1", CheckArray(0,0), 1,"{'mt':2,'ypos':65.7}"
		puPlayer.LabelSet pBackglass,"Check2", CheckArray(1,0), 1,"{'mt':2,'ypos':69.3}"
		puPlayer.LabelSet pBackglass,"Check3", CheckArray(2,0), 1,"{'mt':2,'ypos':72.7}"
		puPlayer.LabelSet pBackglass,"Check4", CheckArray(3,0), 1,"{'mt':2,'ypos':76.1}"
		puPlayer.LabelSet pBackglass,"Check5", CheckArray(4,0), 1,"{'mt':2,'ypos':79.5}"
		puPlayer.LabelSet pBackglass,"Check6", CheckArray(5,0), 1,"{'mt':2,'ypos':82.8}"

		puPlayer.LabelSet pBackglass,"CheckS1", CheckArray(0,1), 1,"{'mt':2,'ypos':65.7}"
		puPlayer.LabelSet pBackglass,"CheckS2", CheckArray(1,1), 1,"{'mt':2,'ypos':69.3}"
		puPlayer.LabelSet pBackglass,"CheckS3", CheckArray(2,1), 1,"{'mt':2,'ypos':72.7}"
		puPlayer.LabelSet pBackglass,"CheckS4", CheckArray(3,1), 1,"{'mt':2,'ypos':76.1}"
		puPlayer.LabelSet pBackglass,"CheckS5", CheckArray(4,1), 1,"{'mt':2,'ypos':79.5}"
		puPlayer.LabelSet pBackglass,"CheckS6", CheckArray(5,1), 1,"{'mt':2,'ypos':82.8}"

'		if Msg1 <> "" then 
'			puPlayer.LabelSet pBackglass,"SplashMsg1", Msg1, 1,""
'			vpmTimer.addTimer time, "puPlayer.LabelSet pBackglass,""SplashMsg1"", """",0,"""" '"
'		End If 
'
'		if Msg2<> "" then 
'			puPlayer.LabelSet pBackglass,"SplashMsg2", Msg2, 1,""
'			vpmTimer.addTimer time, "puPlayer.LabelSet pBackglass,""SplashMsg2"", """",0,"""" '"
'		End if 
	End Sub 

	Sub pizzalocker_hit
		dim SJScore
		if pizzamulti then ' Super Jackpot
			select case pizzasize
				case 1:
		'ShowMsg "Super Jackpot", "240,000" 'FormatScore(BonusPoints(CurrentPlayer))
					SJScore=240000
				case 2:
		'ShowMsg "Super Jackpot", "390,000" 'FormatScore(BonusPoints(CurrentPlayer))
					SJScore=390000
				case 3:
		'ShowMsg "Super Jackpot", "600,000" 'FormatScore(BonusPoints(CurrentPlayer))
					SJScore=600000
				case 4:
		'ShowMsg "Super Jackpot", "900,000" 'FormatScore(BonusPoints(CurrentPlayer))
					SJScore=900000
			End Select 

			SJScore = SJScore * SJMultiplier
								backlamp "flash"
			lightrun yellow,circlein,3
								flasherspop yellow,"crazy" 'left,right,bottom,rightkick,top,crazy
			playmedia "","audio-ptcomplete",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "super-jackpot.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			ShowMsg "Superjackpot x" & SJMultiplier, FormatScore(SJScore)
			'pupDMDDisplay "-", "Superjackpot " & FormatScore(500000), "" ,3, 0, 10		'3 seconds
			AddScore SJScore
			SJMultiplier=1
			jps=0		' Start it over 
			pizzalocker.DestroyBall
			pizzaopen		' Close the box 
			vpmtimer.addtimer 2000, "kickit'"
			Exit Sub 
		End if 


		if ufomulti=False and bWizardModeAte=False and bWizardModeSupreme=False then 	' Not in MB
			pizzaopen			' Close the Pizza Box 
			if bWizardModeIllum then 
				bWizardModeIllumFinished = True
				pizzalocker.DestroyBall
				vpmtimer.addtimer 1500, "StopWizardIllum '"
			else 
				'Dim waittime
				'waittime = 1500
				'vpmtimer.addtimer waittime, "pizzakill'"
				uforedl.state = 2
				'waittime = 2000
				vpmtimer.addtimer 1300, "pizzalocker.DestroyBall '"
				playmedia "","crust-pizzatime",pBackglass,"cineon",3100,"pizzakill",1,4  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				'playmedia "ptmb-bg.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				'vpmtimer.addtimer 3000, "pizzakill '"
						pausemedia pMusic
				PlayModeMusic "Pizza Time - Pizza Time.mp3"
				backlamp "flash"
				flasherspop red,"right" 'left,right,bottom,rightkick,top,crazy
				gicolor red
				ptgi=1
				lightrun red,randoms,3
			End if
		else 
Debug.print "In MB - Skipping PizzaTime"
			vpmtimer.addtimer 500, "pizzalocker.kick 0, 40 '"
		End if 
	End Sub

	Sub pizzakill
		vpmtimer.addtimer 500, "kickit '"
		'kickit
		'pizzalocker.DestroyBall
		StartPizzaTime
	End Sub



' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Multiballs
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 


	dim ufopos:ufopos=1
	dim ufox:ufox=0
	dim ufodir:ufodir=1
	sub allufooff
		dim a
		for each a in ufolights
			a.state=0
			a.color = RGB(0, 18, 0)
			a.colorfull = RGB(0, 255, 0)
			a.intensity=70
		Next
		for each a in aTargetTitles
			a.state=0
		Next
	end Sub

	dim ufojppos:ufojppos=0
	sub ufojpt_timer
		ufojppos=ufojppos+1
		Select case ufojppos
			case 1
				modelight.state = 0
				ufolocklight.state = 1
				ufolocklight.intensity = 20
			case 2
				ufolocklight.state = 0
				pizzaorder.state = 1
				pizzaorder.intensity = 20
			case 3
				pizzaorder.state = 0
				modelight.state = 1
				pizzaorder.intensity = 20
				ufojppos=0
		end Select
	end Sub

	sub ufostacks
		select case ufoPizzasCollected
			case 0
				BeamFlash.opacity = 0
				FlasherSequence.Enabled = False
				ufojpt.enabled=0
				ufolocklight.state = 0
				pizzaorder.state = 0
				modelight.state = 0
				ufodir=1
				ufox = 0
				ufo.Interval = 180
			case 1
				ufojpt.enabled=1
				ufodir=1
				ufox = 1
				ufo.Interval = 150
			case 2
				ufox = 1
				ufo.Interval = 145
			case 3
				ufox = 2
				ufo.Interval = 140
			case 4
				ufox = 2
				ufo.Interval = 135
			case 5
				ufox = 3
				ufo.Interval = 130
			case 6
				ufox = 3
				ufo.Interval = 125
			case 7
				ufox = 4
				ufo.Interval = 120
			case 8
				ufox = 4
				ufo.Interval = 115
			case 9
				ufox = 5
				ufo.Interval = 110
			case 10
				ufox = 5
				ufo.Interval = 105
			case 11
				ufodir=0
				ufox = 1
				ufo.Interval = 150
			case 12
				ufox = 1
				ufo.Interval = 145
			case 13
				ufox = 2
				ufo.Interval = 140
			case 14
				ufox = 2
				ufo.Interval = 135
			case 15
				ufox = 3
				ufo.Interval = 130
			case 16
				ufox = 3
				ufo.Interval = 125
			case 17
				ufox = 4
				ufo.Interval = 120
			case 18
				ufox = 4
				ufo.Interval = 115
			case 19
				ufox = 5
				ufo.Interval = 110
			case 20
				ufox = 5
				ufo.Interval = 105
		end select
	end Sub

	sub ufobg_timer
		SetBGPizza(True)
		playmedia "igmb-bg.mp4","video-scenes",pBackglass,"",2700,"",1,1
	end Sub



	ufo.enabled = 0

	sub ufo_timer
		if ufodir=1 Then
			ufopos=ufopos+1
		Else
			ufopos=ufopos-1
		end If
		allufooff
		select case ufopos
			case -1
				ufopos=10
			case 0
			case 1
				if ufox=0 Then
					fish1.state=1
					l39.state=1
				elseif ufox=1 Then
					fish1.state=1
					l39.state=1
					fish2.state=1
					pepperonititle.state=1
				elseif ufox=2 Then
					fish1.state=1
					l39.state=1
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
				elseif ufox=3 Then
					fish1.state=1
					l39.state=1
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
				elseif ufox=4 Then
					fish1.state=1
					l39.state=1
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
				elseif ufox=5 Then
					fish1.state=1
					l39.state=1
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
				end If

			case 2
				if ufox=0 Then
					fish2.state=1
					pepperonititle.state=1
				elseif ufox=1 Then
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
				elseif ufox=2 Then
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
				elseif ufox=3 Then
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
				elseif ufox=4 Then
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
				elseif ufox=5 Then
					fish2.state=1
					pepperonititle.state=1
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
				end If


			case 3
				if ufox=0 Then
					fish3.state=1
					leftarrow.state=1
				elseif ufox=1 Then
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
				elseif ufox=2 Then
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
				elseif ufox=3 Then
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
				elseif ufox=4 Then
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
				elseif ufox=5 Then
					fish3.state=1
					leftarrow.state=1
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
				end If


			case 4
				if ufox=0 Then
					ropizza3.state=1
					lostar1.state=1
				elseif ufox=1 Then
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
				elseif ufox=2 Then
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
				elseif ufox=3 Then
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
				elseif ufox=4 Then
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
				elseif ufox=5 Then
					ropizza3.state=1
					lostar1.state=1
					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
				end If


			case 5
				if ufox=0 Then
					ropizza2.state=1
					lostar2.state=1
				elseif ufox=1 Then

					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
				elseif ufox=2 Then

					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
				elseif ufox=3 Then

					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
				elseif ufox=4 Then

					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
				elseif ufox=5 Then

					ropizza2.state=1
					lostar2.state=1
					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
				end If


			case 6
			if ufox=0 Then
					ropizza1.state=1
					lostar3.state=1
				elseif ufox=1 Then

					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
				elseif ufox=2 Then

					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
				elseif ufox=3 Then

					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
				elseif ufox=4 Then

					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
				elseif ufox=5 Then

					ropizza1.state=1
					lostar3.state=1
					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
				end If


			case 7
			if ufox=0 Then
					rightarrow.state=1
					lostar4.state=1
				elseif ufox=1 Then

					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
				elseif ufox=2 Then

					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
				elseif ufox=3 Then

					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
				elseif ufox=4 Then

					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
				elseif ufox=5 Then

					rightarrow.state=1
					lostar4.state=1
					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
					leftarrow.state=1
					fish3.state=1
				end If


			case 8
			if ufox=0 Then
					anchtitle.state=1
					lostar5.state=1
				elseif ufox=1 Then

					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
				elseif ufox=2 Then

					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
				elseif ufox=3 Then

					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
				elseif ufox=4 Then

					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
					leftarrow.state=1
					fish3.state=1
				elseif ufox=5 Then

					anchtitle.state=1
					lostar5.state=1
					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
					leftarrow.state=1
					fish3.state=1
					lostar1.state=1
					ropizza3.state=1
				end If


			case 9
			if ufox=0 Then
					pz4title.state=1
					lostar6.state=1
				elseif ufox=1 Then

					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
				elseif ufox=2 Then


					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
				elseif ufox=3 Then

					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
					leftarrow.state=1
					fish3.state=1
				elseif ufox=4 Then

					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
					leftarrow.state=1
					fish3.state=1
					lostar1.state=1
					ropizza3.state=1
				elseif ufox=5 Then

					pz4title.state=1
					lostar6.state=1
					L39.state=1
					fish1.state=1
					pepperonititle.state=1
					fish2.state=1
					leftarrow.state=1
					fish3.state=1
					lostar1.state=1
					ropizza3.state=1
					lostar2.state=1
					ropizza2.state=1
				end If
				ufopos=0
		end Select
	end Sub




	Dim ufomulti
	ufomulti = False
	Sub StartUfo() 'Multiball
		startkick
		SetBGPizza(True)
		playmedia "igmb-bg.mp4","video-scenes",pBackglass,"",2700,"",1,1
		ufobg.enabled=1
		Gate7.Open=true
		startingpizza=0
		if videoready=True Then
			Gate003.Open=false
			videomode.state = 0
		end if
		gicolor green
		ufogi=1
		DOF 340, DOFPulse 'MX-Pizzatime/UFO
		DOF 440, DOFPulse 'RGB
		DOF 405, DOFPulse 'Strobe
		DOF 114, DOFPulse 'Bell
		DOF 407, DOFOn    'Beacon ON
		startB2S(4)
		ufolock = 0
		bMultiBallMode = True
		ufomulti = True
		AddMultiball 2
		EnableBallSaver 20
		BeamFlash.opacity = 1
		ShowRule kRule_Interg
		' Intergalactic Delivery Multiball 
		'playclear pMusic
		'playmedia "The Misfits-I Turned Into A Martian.mp3", MusicDir, pMusic, "", -1, "", 1, 1
		PlayModeMusic "The Misfits-I Turned Into A Martian.mp3"
		ufostacks
	End Sub

	Sub EndUfo
		ufoPizzasCollected = 0
		dim a
		for each a in ufolights
			a.state=0
			a.intensity=30
		Next
		ufobg.enabled=0
		ufojpt.enabled=0
		ufolocklight.intensity = 7
		pizzaorder.intensity = 7
		modelight.intensity = 7
		ufolocklight.state = 0
		pizzaorder.state = 0
		modelight.state = 0
		ufopos=1
		ufox=0
		ufodir=1
		Gate7.Open=False
		ufo.enabled=0
		RestoreAllLights
		ResetAllLightsColor
		if videoready=True Then
			Gate003.Open=true
			videomode.state = 2
		end if
		ufogi=0
		gicolor white
debug.print "End UFO"
		ufomulti = False
		if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
			GiOn
		Else
			gicolor purple
		end if
		PlayerState(CurrentPlayer).Specialties(kSpecial_Locks) = 2	 ' Reset the spinner
		puPlayer.LabelSet pBackglass,"CollectVal2",	PlayerState(CurrentPlayer).Specialties( kSpecial_Locks)	,1,""
		ShowMsg "ufo multi finished", "" 'FormatScore(BonusPoints(CurrentPlayer))
		DOF 407, DOFOff 'Beacon OFF
		BeamFlash.opacity = 0
		FlasherSequence.Enabled = False
		ShowRule kRule_PizzaTime
		StopModeMusic
		SetBGPizza(False)
	End Sub



	Dim pizzaTimeFinished
	Dim pizzamulti
	Dim SJMultiplier
	pizzamulti = False
	pizzaTimeFinished = False 
	SJMultiplier=1

	Sub StartPizzaTime
		pausemedia pMusic
		pizzastrobe 0
		Dim MultiplierX
Debug.print "Start PizzaTime"
		if videoready then	' Close the Video during pizza time (save state though)
Debug.print "SAVING VIDEO MODE for later"
			StopVideoMode
			videoready=True
		End if 
		if ufolightlock =True then  	' Ready to lock a UFO Light (Save it for later)
Debug.print "SAVING UFO Lock for later"
			resetufos
			ufolocklight.state = 0
			BeamFlash.opacity = 0
		End if 
		if ufomulti then 		' End UFO MB  (Save the state)
Debug.print "SAVING UFO MB for later"
			EndUfo	
		End if 
		ShowMsg "Pizza Time!", "" 'FormatScore(BonusPoints(CurrentPlayer))
		DropTargets
		addaballgate.open = False
		jackpotl1.State = 2
		jackpotl2.State = 2
		jackpotl3.State = 2
		ptready = False
		bMultiBallMode = True
		pizzamulti = True
		pizzaTimeFinished = False 
		EnableBallSaver 10
		resetingredlights
		AddMultiball 2
		PizzaTimeCrazy.UpdateInterval = 150
		PizzaTimeCrazy.Play SeqRandom, 10, , 200000
		DOF 340, DOFPulse 'MX-Pizzatime
		DOF 440, DOFPulse 'RGB
		DOF 405, DOFPulse 'Strobe
		DOF 407, DOFPulse 'Beacon
		DOF 114, DOFPulse 'Bell

		'playclear pMusic
		'playmedia "Pizza Time - Pizza Time.mp3", MusicDir, pMusic, "", -1, "", 1, 1


		resettips
		MultiplierX	=0
		if tipped1.state = 1 then 
			if tipped1.state = 1 and extracheesel1.state = 1 Then
				MultiplierX = 2
			Elseif tipped1.state = 1 or extracheesel1.state = 1 Then
				MultiplierX = 1
			End If 
		End if 
		if tipped2.state = 1 then 
			if tipped2.state = 1 and extracheesel2.state = 1 Then
				MultiplierX = 2
			Elseif tipped2.state = 1 or extracheesel2.state = 1 Then
				MultiplierX = 1
			End If 
		End if 
		if tipped3.state = 1 then 
			if tipped3.state = 1 and extracheesel3.state = 1 Then
				MultiplierX = 2
			Elseif tipped3.state = 1 or extracheesel3.state = 1 Then
				MultiplierX = 1
			End If 
		End if 
		if tipped4.state = 1 then 
			if tipped4.state = 1 and extracheesel4.state = 1 Then
				MultiplierX = 2
			Elseif tipped4.state = 1 or extracheesel4.state = 1 Then
				MultiplierX = 1
			End If 
		End if 
		
		If MultiplierX >=1 then		' If you have one you get another MB 
			AddMultiball 1
		ShowMsg "PT Bonus Ball", "" 'FormatScore(BonusPoints(CurrentPlayer))
		End If

		If MultiplierX >=2 then		' If you gave both you also get 2x
			If doublepoints = False Then
				doublepoints = True
				twoxpoints.state = 2
		ShowMsg "PT 2x Scoring", "2x" 'FormatScore(BonusPoints(CurrentPlayer))
			Else
				triplepoints = True
				threexpoints.state = 2
		ShowMsg "PT 3x Scoring", "3x" 'FormatScore(BonusPoints(CurrentPlayer))
			End If
		End If 
		pausemedia pMusic
		addaballgate.open = False
	End Sub


	Sub jp1t_hit							' (Pizza time)
		ultracombo 2

		if ResetSkillShotTimer.Enabled then 
			AddScore PlayerState(CurrentPlayer).SkillshotValue
					playmedia "","audio-skillshot",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					playmedia "skillshot.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					lightrun orange,diagdl,1
					flasherspop orange,"crazy"
								backlamp "run"
			ShowMsg "Skillshot", FormatScore(PlayerState(CurrentPlayer).SkillshotValue)
			'pupDMDDisplay "-", "Skillshot " & FormatScore(PlayerState(CurrentPlayer).SkillshotValue), "" ,3, 0, 10		'3 seconds
			PlayerState(CurrentPlayer).SkillshotValue=PlayerState(CurrentPlayer).SkillshotValue+100000
			'TBD: Add skillshot sound 
		End if 
		BonusProgress(kBonus_Ramps)
		SpecialProgress(kSpecial_Ramps)
		checkModeProgress("ramp_jp1")
		If jackpotl1.State = 2 or jackpotl3.State =2 and ufomulti=False Then		' Ramp can go both ways (Pizza Time) 
			JackpotScore
								backlamp "run"
								flasherspop orange,"right" 'left,right,bottom,rightkick,top,crazy
			'DMD "black.png", "Super", "Jackpot",  100
			jplights
		End If
	End Sub

	Sub jp2t_hit
		playsfx("loopde")
		flippizza
		BonusProgress(kBonus_Ramps)
		BonusProgress(kBonus_LoopDeLoop)
		SpecialProgress(kSpecial_Ramps)
		SpecialProgress(kSpecial_LoopDeLoop)
		if pizzamulti and pizzaisopen then	' We are in PTSJ 
			if SJMultiplier < 12 then
				SJMultiplier=SJMultiplier+1.5
				ShowMsg "Jackpot Multiplier", FormatScore(SJMultiplier)
			End if 
		End If 

		checkModeProgress("ecl2")
		targetlightup
		If StackState(kStack_Pri0).TargetState(ecl2) = 2 Then					' Extra Cheese Light is flashing 
			pizzamodelights
		End If		
		If jackpotl2.State = 2 Then				' Jackpot Light is flashing (Pizza time)
			JackpotScore
								backlamp "run"
								flasherspop orange,"left" 'left,right,bottom,rightkick,top,crazy
			'DMD "black.png", "", "Jackpot",  100
			jplights
		Else
			addcheeseboard
		End If
	End Sub



	Sub jplights		' Eat some Pizza 
		jps = jps +1
		SetBGPizza(False)
		BonusProgress(kBonus_SlicesEaten)
		SpecialProgress(kSpecial_SlicesEaten)
		Select case jps
			Case 0
			Case 1
				If pizzasize = 1 Then
					pz1l1.state = 2
				Elseif pizzasize = 2 Then
					pz2l1.state = 2
				Elseif pizzasize = 3 Then
					pz3l1.state = 2
				Elseif pizzasize = 4 Then
					pz4l1.state = 2
				End If
			Case 2
				If pizzasize = 1 Then
					pz1l2.state = 2
				Elseif pizzasize = 2 Then
					pz2l2.state = 2
				Elseif pizzasize = 3 Then
					pz3l2.state = 2
				Elseif pizzasize = 4 Then
					pz4l2.state = 2
				End If
			Case 3
				If pizzasize = 1 Then
					pz1l3.state = 2
				Elseif pizzasize = 2 Then
					pz2l3.state = 2
				Elseif pizzasize = 3 Then
					pz3l3.state = 2
				Elseif pizzasize = 4 Then
					pz4l3.state = 2
				End If
			Case 4
				If pizzasize = 1 Then
					pz1l4.state = 2
					StartSuper									'4th slice on Personal
				Elseif pizzasize = 2 Then
					pz2l4.state = 2
				Elseif pizzasize = 3 Then
					pz3l4.state = 2
				Elseif pizzasize = 4 Then
					pz4l4.state = 2
				End If
			Case 5
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l5.state = 2
				Elseif pizzasize = 3 Then
					pz3l5.state = 2
				Elseif pizzasize = 4 Then
					pz4l5.state = 2
				End If
			Case 6
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l6.state = 2
				Elseif pizzasize = 3 Then
					pz3l6.state = 2
				Elseif pizzasize = 4 Then
					pz4l6.state = 2
				End If
			Case 7
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l7.state = 2
				Elseif pizzasize = 3 Then
					pz3l7.state = 2
				Elseif pizzasize = 4 Then
					pz4l7.state = 2
				End If
			Case 8
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
					pz2l8.state = 2
					StartSuper								'8th slice on Medium
				Elseif pizzasize = 3 Then
					pz3l8.state = 2
				Elseif pizzasize = 4 Then
					pz4l8.state = 2
				End If
			Case 9
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l9.state = 2
				Elseif pizzasize = 4 Then
					pz4l9.state = 2
				End If
			Case 10
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l10.state = 2
				Elseif pizzasize = 4 Then
					pz4l10.state = 2
				End If
			Case 11
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l11.state = 2
				Elseif pizzasize = 4 Then
					pz4l11.state = 2
				End If
			Case 12
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
					pz3l12.state = 2
					StartSuper									'12th slice on Large
				Elseif pizzasize = 4 Then
					pz4l12.state = 2
				End If
			Case 13
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l13.state = 2
				End If
			Case 14
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l14.state = 2
				End If
			Case 15
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l15.state = 2
				End If
			Case 16
				If pizzasize = 1 Then
				Elseif pizzasize = 2 Then
				Elseif pizzasize = 3 Then
				Elseif pizzasize = 4 Then
					pz4l16.state = 2
					StartSuper									'16th slice on Belly Buster
				End If
		End Select
	End Sub

	Sub StartSuper()
								flasherspop red,"top" 'left,right,bottom,rightkick,top,crazy
		SJMultiplier=1	' Set the Loop Multiplier to 1
		pizzaopen		' Open the pizza to get SJ
		pizzaTimeFinished=True
	End Sub 

	Sub EndPizza(bFinished)
		resetingredlights
		ptgi=0
		if modegi=1 Then
			gicolor purple
		Else
			gicolor white
		end if
		Dim light
		dim num 
Debug.print "End PizzaTime: " & bFinished

		If addaballmade = True Then					' No way to get to this code (yet)
			AddMultiball 1
			addaballmade = False
		Else 
			if pizzaisopen then pizzaopen		' Close the Pizza (From SJ)
			'pizzaisopen = False
			jackpotl1.State = 0
			jackpotl2.State = 0
			jackpotl3.State = 0
			resetingredlights
			gottips = False		' We moved to the next mode

			pizzaorder.state = 2
			'RaiseTargets
			if bFinished=False and pizzasize=4 Then	' We didnt collect all the pizzas, restart the mode to continue
				' Dont increase Pizza Size (no more)
				' Dont reset pzlnum (Pizza eaten)

				' Turn off all the lights that need to be recollected
				pzlnum=jps
				if pzlnum < 16 then pz4l16.state = 0
				if pzlnum < 15 then pz4l15.state = 0
				if pzlnum < 14 then pz4l14.state = 0
				if pzlnum < 13 then pz4l13.state = 0
				if pzlnum < 12 then pz4l12.state = 0
				if pzlnum < 11 then pz4l11.state = 0
				if pzlnum < 10 then pz4l10.state = 0
				if pzlnum < 9 then pz4l9.state = 0
				if pzlnum < 8 then pz4l8.state = 0
				if pzlnum < 7 then pz4l7.state = 0
				if pzlnum < 6 then pz4l6.state = 0
				if pzlnum < 5 then pz4l5.state = 0
				if pzlnum < 4 then pz4l4.state = 0
				if pzlnum < 3 then pz4l3.state = 0
				if pzlnum < 2 then pz4l2.state = 0
				if pzlnum < 1 then pz4l1.state = 0
			elseif bFinished=True and pizzasize=4 Then	' Finished Belly Buster Start Wizard Mode IAteTheWholeThing
				pizzaorder.state = 0
				StartWizardAte
			elseif (bHardMode and bFinished=True) or bHardMode=False then 		' If hardmode we dont move on 
				pizzasize = pizzasize + 1
				pzlnum = 0
				pzlnum2 = 0
				jps = 0
			elseif (bHardMode and bFinished=False) then
				pizzasize = pizzasize + 1
				pzlnum = 0
				pzlnum2 = 0
				jps = 0
				'pzlnum=jps
				'For each light in aTargetLights
				'	if left(light.name,4) = "pz" & pizzasize & "l" then 
				'		num=CINT(mid(light.name, 5))
				'		if pzlnum < num then light.state = 0
				'	End if
				'Next
			End if
	' TBD: Finish Extra Ball Code
	'		Select Case pizzasize(CurrentPlayer)
	'			Case 3
	'				ebnowlit(CurrentPlayer) = True
	'				extraballlight.state = 2
	'				ebflash.opacity = 2000
	'		End Select
			currentpizza = 0
			addaballmade = False
			PizzaTimeCrazy.StopPlay
			multimulti.enabled = 0
			doublepoints = False
			twoxpoints.state = 0
			triplepoints = False
			threexpoints.state = 0
			pizzamulti = False
			pizzaTimeFinished=False
			resetufos									' Clear UFO's since we cant do during PizzaTime 

			if videoready then StartVideoMode			' Open Video Mode back up after MB
			if ufolightlock then						' Light the lock back Up
				ufo1l.state = 1
				ufo2l.state = 1
				ufo3l.state = 1
				ufo4l.state = 1
				ufo5l.state = 1
				checkufos
			End If

			StopModeMusic

		End If
	End Sub


 sub Kicker1_hit
	Kicker1.Kick 45, 64
 end Sub

sub Kicker001_hit
	Kicker001.Kick 30, 10
end Sub
sub Kicker002_hit
	Kicker002.Kick 30, 20
end Sub

 sub Kicker3_hit				' TV Mode (Mini Game) or Extra Ball is enabled
	Kicker3.Kick 65, 10
 end Sub

 sub Kicker2_hit
	Kicker2.Kick 0, 64
	DOF 331, DOFPulse 'MX-leftramp
	DOF 431, DOFPulse 'rgb
	DOF 405, DOFPulse 'strobe
 end Sub


	' speed belt
	dim beltpos
	Sub Beltmove_Timer
		Select Case beltpos
			Case 1:belt1.opacity = 50:belt2.opacity = 0:belt3.opacity = 0:belt4.opacity = 0:belt5.opacity = 0:belt6.opacity = 0
			Case 2:belt1.opacity = 0:belt2.opacity = 50:belt3.opacity = 0:belt4.opacity = 0:belt5.opacity = 0:belt6.opacity = 0
			Case 3:belt1.opacity = 0:belt2.opacity = 0:belt3.opacity = 50:belt4.opacity = 0:belt5.opacity = 0:belt6.opacity = 0
			Case 4:belt1.opacity = 0:belt2.opacity = 0:belt3.opacity = 0:belt4.opacity = 50:belt5.opacity = 0:belt6.opacity = 0
			Case 5:belt1.opacity = 0:belt2.opacity = 0:belt3.opacity = 0:belt4.opacity = 0:belt5.opacity = 50:belt6.opacity = 0
			Case 6:belt1.opacity = 0:belt2.opacity = 0:belt3.opacity = 0:belt4.opacity = 0:belt5.opacity = 0:belt6.opacity = 50:beltpos = 0
		End Select
		beltpos = beltpos + 1
	End Sub



	Sub addcheeseboard
		if cheesevalue < 4 then 
			cheesevalue = cheesevalue + 1
		ShowMsg "Extra cheese added", "1,000" 'FormatScore(BonusPoints(CurrentPlayer))
			select case cheesevalue
				Case 1:cheese4l.opacity = 2000:cheese3l.opacity = 0:cheese2ll.opacity = 0:cheese1l.opacity = 0
				Case 2:cheese4l.opacity = 2000:cheese3l.opacity = 2000:cheese2ll.opacity = 0:cheese1l.opacity = 0
				Case 3:cheese4l.opacity = 2000:cheese3l.opacity = 2000:cheese2ll.opacity = 2000:cheese1l.opacity = 0
				Case 4:cheese4l.opacity = 2000:cheese3l.opacity = 2000:cheese2ll.opacity = 2000:cheese1l.opacity = 2000
			playmedia "","audio-extracheese",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "extra-cheese.mp4","video-scenes",pBackglass,"",3000,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
								backlamp "run"
					If pz1title.state = 2 Then
						extracheesel1.state = 1
					End If
					If pz2title.state = 2 Then
						extracheesel2.state = 1
					End If
					If pz3title.state = 2 Then
						extracheesel3.state = 1
					End If
					If pz4title.state = 2 Then
						extracheesel4.state = 1
					End If
			End select
		End if 
	End Sub

	Sub flippizza
		pizzaflip.enabled = true
	End Sub

	Dim pizzapos
	Dim doughpos:doughpos = 0
	Sub pizzaflip_timer
		If doughpos = 0 Then
			If dough.z = 149 Then
				doughpos = 1
			Else
				dough.z = dough.z + 1
			End If
		Else
			If dough.z = 50 Then
				doughpos = 0
				pizzaflip.enabled = false				
			Else
				dough.z = dough.z - 1
			End If
		End If
	End Sub





' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Show rules
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 

Sub ShowRuleMessage(Title, Line1, Line2, Line3, Line4)
	puPlayer.LabelSet pBackglass,"RulesT",	Title	,1,""
	puPlayer.LabelSet pBackglass,"Rules1",	Line1   ,1,""
	puPlayer.LabelSet pBackglass,"Rules2",	Line2   ,1,""
	puPlayer.LabelSet pBackglass,"Rules3",	Line3   ,1,""
	puPlayer.LabelSet pBackglass,"Rules4",	Line4   ,1,""

End Sub 

Const kRule_PizzaTime = 0
Const kRule_AteWholeThing = 1
Const kRule_Illuminati = 2
Const kRule_PizzaParty = 3
Const kRule_TimeBomb = 4
Const kRule_DeathOrGlory = 5
Const kRule_Rhumba = 6
Const kRule_BadTown = 7
Const kRule_PizzaLove = 8
Const kRule_Interg = 9

Sub ShowRule(Index)
	Select case index 
		Case kRule_PizzaTime:
			ShowRuleMessage " PIZZA TIME", _
					" To Start PizzaTime you will need to collect", _
					"the flashing ingredients on the playfield. Once", _
					"the pizza box will open, hit to", _
					"start pizza time!"
		Case kRule_AteWholeThing:
			ShowRuleMessage " I ate the whole thing", _
					" Unlimited balls for 30 seconds.  All orbits and", _
					"ramps score jackpots.  Once time is up balls", _
					"drain and you get a special video.", _
					""
		Case kRule_Illuminati:
			ShowRuleMessage " Pizza Illuminati", _
					" You have 15 seconds to hit the pizza box and ", _
					"ascend to greatness", _
					"", _
					""
		Case kRule_PizzaParty:
			ShowRuleMessage " Pizza Party", _
					" Hit every shot to clear the table.  ", _
					"Once hit each shot will be complete til all ", _
					"are gone. 40 seconds", _
					""
		Case kRule_TimeBomb:
			ShowRuleMessage " Time Bomb", _
					" Shoot the roving shots to keep the mode alive. ", _
					"Points increase with every shot. 15 shots", _
					"", _
					""
		Case kRule_DeathOrGlory:
			ShowRuleMessage " Death or Glory", _
					" Hit the Glory outlane as many times as possible.", _
					"Even scarier can you do a death save from the", _
					"death lane? That would count for 2 points", _
					"5 points completes the mode."
		Case kRule_Rhumba:
			ShowRuleMessage " 3 Pizza Rhumba", _
					" Combo challenge. Top gate is opened, so you ", _
					"have to hit the pizza surfers orbit then hit ", _
					"the ramp with the upper flipper. Every ", _
					"successful combo scores huge. 40 seconds"
		Case kRule_BadTown:
			ShowRuleMessage " Bad Town", _
					" Time to wreck the competitors pizza shop. ", _
					"Hit the bumpers and side bumpers to smash ", _
					"the other shop to bits. 100 bumper hits ", _
					"40 seconds"
		Case kRule_PizzaLove:
			ShowRuleMessage " Pizza You Love", _
					" Unlimited Skillshots (top flipper to ramp.", _
					"Eash ic larger than the last.", _
					"10 shots 40 seconds", _
					""
		Case kRule_Interg:
			ShowRuleMessage " Intergalatic Delivery MB", _
					" Pick up Pizzas (Ramps).  Deliver to tractor", _
					"beam when on. Build up as many pizzas as you ", _
					"feel comfortable risking. Deliver them to the ", _
					"ship. When mode ends undelivered pizzas are lost"
	End Select 
End Sub 


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Video Manager & skipper
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
'	Const pTopper=0			' show, 
'	Const pDMD=1			' ForceBack
'	Const pBackglass=2		' ForceBack
'	Const pPlayfield=3		' off
'	Const pMusic=4			' Music Only
'	Const pAudio=5			' Music Only
'	Const pCallouts=8		' Music Only
'	Const pOvervid=14		' ForcePopBack
'	Const pGame=15			' ForcePop - In Game mini game

	dim currentqueue
	dim cineon:cineon = 0
	dim skipped:skipped = 0
	dim bCancelNext:bCancelNext=False
	dim bMediaPaused(19)
	dim bMediaSet(19)
	bMediaPaused(0)=False:bMediaPaused(1)=False:bMediaPaused(2)=False:bMediaPaused(3)=False:bMediaPaused(4)=False:bMediaPaused(5)=False
	bMediaPaused(6)=False:bMediaPaused(7)=False:bMediaPaused(8)=False:bMediaPaused(9)=False:bMediaPaused(10)=False:bMediaPaused(11)=False
	bMediaPaused(12)=False:bMediaPaused(13)=False:bMediaPaused(14)=False:bMediaPaused(15)=False:bMediaPaused(16)=False:bMediaPaused(17)=False
	bMediaPaused(18)=False:bMediaPaused(19)=False
	bMediaSet(0)=False:bMediaSet(1)=False:bMediaSet(2)=False:bMediaSet(3)=False:bMediaSet(4)=False:bMediaSet(5)=False
	bMediaSet(6)=False:bMediaSet(7)=False:bMediaSet(8)=False:bMediaSet(9)=False:bMediaSet(10)=False:bMediaSet(11)=False
	bMediaSet(12)=False:bMediaSet(13)=False:bMediaSet(14)=False:bMediaSet(15)=False:bMediaSet(16)=False:bMediaSet(17)=False
	bMediaSet(18)=False:bMediaSet(19)=False
	sub pausemedia(channel) 
		if bUsePUPDMD=False then exit sub 

debug.print "pause media ch:" & channel & " current:" & bMediaPaused(channel)
		if bMediaSet(channel) = False then Exit Sub 
		if bMediaPaused(channel) = False then
debug.print "pause"
			PuPlayer.playpause channel
			bMediaPaused(channel)=True
		End If 
	End Sub

	sub resumemedia(channel) 
		if bUsePUPDMD=False then exit sub 
debug.print "resume media ch:" & channel & " current:" & bMediaPaused(channel)
		if bMediaSet(channel) = False then Exit Sub 
		if bMediaPaused(channel) then
debug.print "resume"
			PuPlayer.playresume channel
			bMediaPaused(channel)=False
		End If 
	End Sub
			
	sub playclear(chan)
		if bUsePUPDMD=False then exit sub 
		debug.print "play clear'd " & chan
		bMediaSet(chan) = False
		bMediaPaused(chan) = False

		if chan = pOverVid then 
			PuPlayer.PlayStop pOverVid
			'PuPlayer.SetLoop pOverVid, 0
		End If 

		if chan = pAudio Then
			'PuPlayer.playlistplayex pAudio,"audioclear","clear.mp3",100,20
			PuPlayer.SetLoop pAudio, 0
			PuPlayer.playstop pAudio
		End If

		if chan = pBonusScreen then 
			PuPlayer.SetBackGround chan, 0
			PuPlayer.SetLoop chan, 0
			PuPlayer.playstop chan
		End If 

		if chan = pMusic Then
			'PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100,20
			PuPlayer.playstop pMusic
			'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic &", ""FN"":11, ""VL"":0 }"
		End If

		if chan = pBackglass Then
			if currentqueue <> "" then 
				bCancelNext = True
debug.print "Clear is cancelling " & currentqueue
			End If 
			PuPlayer.SetBackGround pBackglass, 0
			PuPlayer.SetLoop pBackglass, 0
			PuPlayer.playstop pBackglass
		end if 
	end Sub


	dim lastvocall:lastvocall=""
	dim noskipper:noskipper=0

	'example playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	sub playmedia(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		if bUsePUPDMD=False then exit sub 
Debug.print "PlayMedia"
		Dim NextQueue:NextQueue=""
		bMediaSet(channel) = True
		if audiolevel = 1 Then
			if channel = pBackglass Then
				audiolevel = VolBGMusic
				currentqueue = ""
			Elseif channel = pCallouts Then
				audiolevel = VolBGMusic
			Elseif channel = pMusic Then
				audiolevel = VolMusic
			Elseif channel = pAudio Then
				audiolevel = VolBGMusic 
			Elseif channel = pOverVid Then
				audiolevel = VolBGMusic  
			end If
		end If

		if channel = pCallouts Then
		'	if lastvocall = name then exit sub
		end if
	

		if length <> -1 then 
			if channel = pBackglass Then	' Clear Pizza and restore after the media is playing
debug.print "Hide BGPizza" 
				SetBGPizza(True)			' Hide the Pizza in center and set a timer to show it later 
				NextQueue="bMediaSet("&channel&") = False:SetBGPizza(False)"
			End if 

			If cinematic = "cineon" Then	' Turn down the music and then turn it back up
debug.print "Turn down music"
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic &", ""FN"":11, ""VL"":40 }"	' Turn it down
		if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
				GiOff
		end if
				NextQueue=NextQueue&":turnitbackupcine"
			end If

			' Queue up next item
			if nextitem <> "" and cinematic = "cineon" then 				' Supports flipper skip
debug.print "Queue Scene for Cineon"
				QueueScene NextQueue & ":" & nextitem & " '", length, 1	
			Elseif nextitem <> "" or NextQueue <> "" then 					' we have something left to queue up
debug.print "Standard Queue:" & NextQueue & ":" & nextitem
				vpmtimer.addtimer length, NextQueue & ":" & nextitem & " '"
			End If
		End if 

debug.print "PlayMedia " & channel & " Dir:" & playlist & " File:" & name & " Vol:" & audiolevel & " Pri:" & priority & " Len:" & length
		PuPlayer.playlistplayex channel,playlist,name,audiolevel,priority
		'if channel = pBackglass then PuPlayer.SetBackGround channel, 1
		if channel = pAudio then PuPlayer.SetLoop channel, 1
		if channel = pBonusScreen then PuPlayer.SetLoop channel, 1
		if channel = pMusic then PuPlayer.SetLoop channel, 1			' Loop the music on this game

		If channel = pCallouts and length <> -1 Then
debug.print "ChangeVol pBackglass Volume for Callouts"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&", ""FN"":11, ""VL"":60 }"
			vpmtimer.addtimer length, "turnitbackup'"
		end If

		If channel = pOverVid and length <> -1 Then
debug.print "ChangeVol pBackglass for OverVid" 
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":50 }"
			vpmtimer.addtimer length, "turnitbackupvid'"
		end If

		if channel = pCallouts Then
			lastvocall=name
		end if


	end sub

	Sub turnitbackup
debug.print "Change Volume5"

		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic &", ""FN"":11, ""VL"":"&VolMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&", ""FN"":11, ""VL"":"&VolMusic&" }"
		debug.print "turnitbackup "
		'puplayer.setvolume pMusic sndtrkvol
	End Sub

	Sub turnitbackupvid
debug.print "Change Volume4"

		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic &", ""FN"":11, ""VL"":"&VolMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&", ""FN"":11, ""VL"":"&VolMusic&" }"
		debug.print "turnitbackupvid "
	End Sub

	Sub turnitbackupcine
debug.print "Change Volume3"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic &", ""FN"":11, ""VL"":"&VolMusic&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&", ""FN"":11, ""VL"":"&VolMusic&" }"
		debug.print "turnitbackupcine "
	End Sub

	sub holder
	end sub

	sub nextitems
		if bCancelNext then 	' Cancel the last items that was sent to the queue
debug.print "Cancel Queue"
			bCancelNext=False
			Exit Sub
		End If 
debug.print "Executing " & currentqueue
		Execute currentqueue
		if ufogi + ptgi + modegi + supgi + illgi + ategi = 0 Then
			GiOn
		end If
		currentqueue = ""
		bCancelNext = False 	' Items in queue can trigger a cancel
debug.print "Queue Empty"
		bMediaSet(pBackglass) = False:SetBGPizza(False)
	end Sub

	sub vidskipper_timer		' TBD: Make this work (flippers skip cinematics)
		if cineon = 1  and noskipper = 0 Then
			if ldown = 1 and rdown = 1 Then
				nextitems
				skipped = 1
			end If
		end If
	end Sub


	'logic for skipping
	' vid starts. is it cinematic?
	' yes cineon = 1





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Orbital Scoreboard Code
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'****************************
' POST SCORES
'****************************
Dim osbtemp
Dim osbtempscore:osbtempscore = 0
if osbactive = 1 or osbactive = 2 Then osbtemp = osbdefinit
Sub SubmitOSBScore
	dim vers
	Dim ver
	vers=split(myVersion,".")			' Convert x.y.z to x.y
	ver=vers(0) & "." & vers(1)

	On Error Resume Next
	if osbactive = 1 or osbactive = 2 Then
		Dim objXmlHttpMain, Url, strJSONToSend 

		Url = "https://hook.integromat.com/82bu988v9grj31vxjklh2e4s6h97rnu0"
		strJSONToSend = "{""auth"":""" & osbkey &""",""player id"": """ & osbid & """,""player initials"": """ & osbtemp &""",""score"": " & CStr(osbtempscore) & ",""table"":"""& cGameName & """,""version"":""" & ver & """}"

		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		objXmlHttpMain.open "PUT",Url, False
		objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
		objXmlHttpMain.setRequestHeader "application", "application/json"

		objXmlHttpMain.send strJSONToSend
	end if
End Sub	

'****************************
' GET SCORES
'****************************
dim worldscores

Sub GetScores()
	if osbactive =0 then exit sub 
	if osbkey="" then exit sub
	On Error Resume Next
	Dim objXmlHttpMain, Url, strJSONToSend 

	Url = "https://hook.integromat.com/kj765ojs42ac3w4915elqj5b870jrm5c"

	strJSONToSend = "{""auth"":"""& osbkey &""", ""table"":"""& cGameName & """}"

	Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
	objXmlHttpMain.open "PUT",Url, False
	objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
	objXmlHttpMain.setRequestHeader "application", "application/json"

	objXmlHttpMain.send strJSONToSend

	worldscores = objXmlHttpMain.responseText
	vpmtimer.addtimer 3000, "showsuccess '"
	debug.print "Got OSB scores"
	'debug.print worldscores
	splitscores
End Sub	

Dim scorevar(22)
Dim dailyvar(2, 22)
Dim weeklyvar(2, 22)
Dim alltimevar(2, 42)
sub emptyscores
	dim i		
	For i = 0 to 42
		alltimevar(0, i) = ""
		alltimevar(1, i) = "0"
		if i <= 22 then 
			weeklyvar(0, i) = ""
			weeklyvar(1, i) = "0"
			dailyvar(0,i) = ""
			dailyvar(1,i) = "0"	
		End if 
	Next
End Sub
emptyscores


Sub splitscores
	On Error Resume Next
	dim a,scoreset,subset,subit,myNum,daily,weekly,alltime,x, tmpsplit, i
	a = Split(worldscores,": {") 
	subset = Split(a(1),"[")

		debug.print subset(1)
'		debug.print subset(2)
'		debug.print subset(3)
' daily scores

	for i = 1 to 3
		x = Replace(subset(i), vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, "      ", "")
		x = Replace(x, "    ]", ",")
		x = Replace(x, "{""initials"": ""","")
		x = Replace(x, "score"": ","")
		x = Replace(x, """,""",",")
		x = Replace(x, "},",";")
		x = Replace(x, ";,    ""weekly"": ","")
		x = Replace(x, ";,    ""alltime"": ","")
		x = Replace(x, ";  }}", "")
		subset(i)=x
'debug.print subset(i)  
	next

	myNum = 0
	daily = Split(subset(1),";")
	for each x in daily
		tmpsplit=Split(x,",")
		dailyvar(0, MyNum) = tmpsplit(0)
		dailyvar(1, MyNum) = tmpsplit(1)
		myNum = MyNum + 1
'debug.print "dailyvar(" &MyNum & ")=" & dailyvar(0, MyNum) & "," & dailyvar(1, MyNum)
	Next

	myNum = 0
	weekly = Split(subset(2),";")
	for each x in weekly
		tmpsplit=Split(x,",")
		weeklyvar(0, MyNum) = tmpsplit(0)
		weeklyvar(1, MyNum) = tmpsplit(1)
		myNum = MyNum + 1
'debug.print "weeklyvar(" &MyNum & ")=" & weeklyvar(0, MyNum) & "," & weeklyvar(1, MyNum)
	Next

	myNum = 0
	alltime = Split(subset(3),";")
	for each x in alltime
		tmpsplit=Split(x,",")
		alltimevar(0, MyNum) = tmpsplit(0)
		alltimevar(1, MyNum) = tmpsplit(1)
		myNum = MyNum + 1
'debug.print "alltimevar(" &MyNum & ")=" & alltimevar(0, MyNum) & "," & alltimevar(1, MyNum)
	Next

end Sub


sub showsuccess
	ShowMsg "Scoreboard Updated", ""
end sub




' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Pup Framework
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 

' Null class for those that arn using pup 
Class PinupNULL	' Dummy Pinup class so I dont have to keep adding if cases when people dont choose pinup
	Public Sub LabelShowPage(screen, pagenum, vis, Special)
	End Sub
	Public Sub LabelSet(screen, label, text, vis, Special)
	End Sub
	Public Sub playlistplayex(screen, dir, fname, volume, priority)
	End Sub 
	Public Sub SetBackground(screen, enable)
	End Sub
	Public Sub playstop(screen)
	End Sub
End Class 

'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pDMD=1
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
Const pAudio=14
Const pCallouts=6
Const pBackglass2=7
Const pPup0=8
Const pBonusScreen = 12
Const pOverVid=13

'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2






Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode




'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

on error resume next
Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
'puPlayer.Init pBonusScreen, ""
PuPlayer.B2SInit "", cGameName
on error goto 0
if not IsObject(PuPlayer) then bUsePUPDMD=False

if DMDMode <> 0 then 
	if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
		   PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
	End if
End if 

PuPlayer.LabelInit pBackglass
PuPlayer.LabelInit pDMD
PuPlayer.LabelInit pOverVid
if DMDMode <> 0 then 
	PuPlayer.LabelInit pDMD

	if PuPDMDDriverType=pDMDTypeReal then
		Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
		PUPDMDObject.DMDOpen
		PUPDMDObject.DMDPuPMirror
		PUPDMDObject.DMDPuPTextMirror
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
	END IF
End If 


pSetPageLayouts
pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pDMDStartUP				 ' firsttime running for like an startup video..

End Sub 'end PUPINIT



'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub



Sub pDMDSplashBig(msgText,timeSec, mColor)
'	PuPlayer.LabelShowPage pDMD,2,timeSec,""
'	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
end sub

Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
Dim backText
Dim middleText
Dim flashText
Dim curChar
Dim i
Dim offchars:offchars=0
Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)
	If (bUsePUPDMD) then
		Debug.print "Changing Page: " & pagenum
		PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
		PDMDCurPage=pagenum
	End if
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2Color : pLine2Color=8454143
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'*************************                see docs for examples                ************************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName, TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' 			also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
	if pDMDCurPriority>pPriority then 
		Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
		Debug.print "DMD Skipping - Hi Pri"
	End If 
	pDMDCurPriority=pPriority
	if timeSec=0 then timeSec=1 'don't allow page default page by accident


	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""


	if pAni=1 Then  'we flashy now aren't we
	pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	end If

	curPos=InStr(pText,"^")   'Lets break apart the string if needed
	if curPos>0 Then 
	   pLine1=Left(pText,curPos-1) 
	   pText=Right(pText,Len(pText) - curPos)
	   
	   curPos=InStr(pText,"^")   'Lets break apart the string
	   if curPOS>0 Then
		  pLine2=Left(pText,curPos-1) 
		  pText=Right(pText,Len(pText) - curPos)

		  curPos=InStr("^",pText)   'Lets break apart the string   
		  if curPos>0 Then
			 pline3=Left(pText,curPos-1) 
		  Else 
			if pText<>"" Then pline3=pText 
		  End if 
	   Else 
		  if pText<>"" Then pLine2=pText
	   End if    
	Else 
	  pLine1=pText  'just one line with no break 
	End if


	'lets see how many lines to Show
	pNumLines=0
	if pLine1<>"" then pNumLines=pNumlines+1
	if pLine2<>"" then pNumLines=pNumlines+1
	if pLine3<>"" then pNumLines=pNumlines+1

	if pDMDVideoPlaying Then 
				PuPlayer.playstop pDMD
				pDMDVideoPlaying=False
	End if


	if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
		
		PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
		pDMDVideoPlaying=true
	end if 'if showing a splash video with no text


	if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
		pDMDShowCounter pLine1,pLine2,pLine3,timeSec
	Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
		pDMDTargetLettersInfo pLine1,pLine2,timeSec
	Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
		pDMDShowHS pLine1,pLine2,pline3,timeSec
	Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
		pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
	Elseif (pNumLines=2) Then
		pDMDShowLines2 pLine1,pLine2,TimeSec
	Elseif (pNumLines=1) Then
		pDMDShowBig pLine1,timeSec, curLine1Color
	Else
		pDMDShowBig pLine1,timeSec, curLine1Color
	End if

	PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	pUpdateScores

    if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
		PriorityReset=PriorityReset-pupDMDUpdate.interval
		if PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false	
		End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
		pAttractReset=pAttractReset-pupDMDUpdate.interval
		if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
		End if
    end if 
End Sub

Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub
PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts
Dim i
DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed
DIM digitLCD
DIM guardians
DIM dmdRetro

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard wed set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to force a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT this will assign this label to this page or group.
'<visible> initial state of label. visible=1 show, 0 = off.

' Overlay
	'puPlayer.LabelInit pOverlayFrame

' Backglass - this is basically the FullLCD-DMD
'		-----------------------------------------------------------------------
'       | 
' 		|  ColLbl1 Val1  Typ1	SplashMsg1/2			DetailCollect
' 		|  ColLbl2 Val2  Typ2	ModeTmr				   Item1  Item2 Item3
'		|     ...   ..    ..	WizTmr					  Item4  Item5
' 		|  ColLbl5 Val6  Typ5	PProgress			DetailMode (cnt) DetailType
'		|  ColLbl6 Val5  Typ6							Check1		CheckS1
' 		|  ColLbl7 Val7  Typ7							  ..          ..
'		|												Check6		CheckS6
'		|	Play1Score   Play2Score		Ball		Play3Score	Play4Score
'		-------------------------------------------------------------------------
'
	dmdalt="Mensch"
    dmdfixed="Instruction"
    dmdscr="Impact"    'main scorefont
	dmddef="Himalaya Sans Serif"
	digitLCD="DIGIT LCD"
	dmdRetro="Zig"

'					   Scrn LblName    Fnt    			Size	Color	 			R AxAy X Y pagenum Visible 
	puPlayer.LabelInit pBackglass

	' Make sure everything overlaps the pizza progress by using labelset and visible=0
	puPlayer.LabelNew pBackglass,"PProgress",digitLCD,		3,RGB(255, 255, 255)	,0,0,0 ,42,38    ,1,0	
	puPlayer.LabelSet pBackglass,"PProgress", "video-pizzas\\gp-8-0.png", 0,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"


	puPlayer.LabelNew pBackglass,"Play1scoreX",digitLCD,		6,RGB(255, 255, 255)  	,0,0,0 ,0,88    ,1,0
	puPlayer.LabelNew pBackglass,"Play2scoreX",digitLCD,		6,RGB(255, 255, 255)  	,0,0,0 ,45,90    ,1,0
	puPlayer.LabelNew pBackglass,"Play3scoreX",digitLCD,		6,RGB(255, 255, 255)  	,0,0,0 ,73,90    ,1,0
	puPlayer.LabelNew pBackglass,"Play4scoreX",digitLCD,		6,RGB(255, 255, 255)  	,0,0,0 ,98,90    ,1,0
	puPlayer.LabelSet pBackglass,"Play1scoreX", "PuPOverlays\\Pizza3x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':1}"
	puPlayer.LabelSet pBackglass,"Play2scoreX", "PuPOverlays\\Pizza3x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':25}"
	puPlayer.LabelSet pBackglass,"Play3scoreX", "PuPOverlays\\Pizza3x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':53}"
	puPlayer.LabelSet pBackglass,"Play4scoreX", "PuPOverlays\\Pizza3x.png", 0,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':77}"
	puPlayer.LabelNew pBackglass,"Play1scoreXTime",digitLCD,		2,RGB(255, 255, 255)  	,0,0,0 ,11.3,87.5    ,1,1
	puPlayer.LabelNew pBackglass,"Play2scoreXTime",digitLCD,		2,RGB(255, 255, 255)  	,0,0,0 ,11.3,87.5    ,1,1
	puPlayer.LabelNew pBackglass,"Play3scoreXTime",digitLCD,		2,RGB(255, 255, 255)  	,0,0,0 ,11.3,87.5    ,1,1
	puPlayer.LabelNew pBackglass,"Play4scoreXTime",digitLCD,		2,RGB(255, 255, 255)  	,0,0,0 ,11.3,87.5    ,1,1


	puPlayer.LabelNew pBackglass,"CollectLbl1",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,19    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectLbl2",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,28    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectLbl3",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,38    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectLbl4",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,47.4  ,1,1	
	puPlayer.LabelNew pBackglass,"CollectLbl5",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,56.4	,1,1	
	puPlayer.LabelNew pBackglass,"CollectLbl6",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,66    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectLbl7",dmddef,		3,RGB(255, 255, 255)	,0,2,0 ,13.4,75    ,1,1	

	puPlayer.LabelNew pBackglass,"CollectVal1",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,20    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectVal2",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,30    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectVal3",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,38.6  ,1,1	
	puPlayer.LabelNew pBackglass,"CollectVal4",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,47.8  ,1,1	
	puPlayer.LabelNew pBackglass,"CollectVal5",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,56.8  ,1,1	
	puPlayer.LabelNew pBackglass,"CollectVal6",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,66    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectVal7",digitLCD,	4,RGB(255, 205, 0)	,0,1,0 ,15.5,75.5  ,1,1	


	puPlayer.LabelNew pBackglass,"CollectTyp1",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,19    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectTyp2",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,28    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectTyp3",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,38    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectTyp4",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,47.4  ,1,1	
	puPlayer.LabelNew pBackglass,"CollectTyp5",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,56.4	,1,1	
	puPlayer.LabelNew pBackglass,"CollectTyp6",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,66    ,1,1	
	puPlayer.LabelNew pBackglass,"CollectTyp7",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,18,75    ,1,1	

	puPlayer.LabelNew pBackglass,"DetailCollect",dmddef,	5,RGB(255, 255, 255)	,0,1,0 ,83.5,19    ,1,1	
	puPlayer.LabelNew pBackglass,"Item1",dmddef,			3,RGB(255, 255, 255)	,0,1,0 ,78,29    ,1,1	
	puPlayer.LabelNew pBackglass,"Item2",dmddef,			3,RGB(255, 255, 255)	,0,1,0 ,83.5,29  ,1,1	
	puPlayer.LabelNew pBackglass,"Item3",dmddef,			3,RGB(255, 255, 255)	,0,1,0 ,85,29    ,1,1	
	puPlayer.LabelNew pBackglass,"Item4",dmddef,			3,RGB(255, 255, 255)	,0,1,0 ,80.5,34  ,1,1	
	puPlayer.LabelNew pBackglass,"Item5",dmddef,			3,RGB(255, 255, 255)	,0,1,0 ,84,34    ,1,1	
	puPlayer.LabelNew pBackglass,"DetailMode",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,73,39    ,1,1	
	puPlayer.LabelNew pBackglass,"DetailCount",digitLCD,	4,RGB(255, 205, 0)		,0,1,0 ,83.5,39  ,1,1	
	puPlayer.LabelNew pBackglass,"DetailType",dmddef,		3,RGB(255, 255, 255)	,0,0,0 ,86,39    ,1,1	

	puPlayer.LabelNew pBackglass,"ModeTmr",digitLCD,		3,RGB(255, 255, 255)	,0,0,0 ,42,38    ,1,1	
	puPlayer.LabelNew pBackglass,"WizTmr",digitLCD,			3,RGB(255, 255, 255)	,0,0,0 ,42,42    ,1,1	
	'puPlayer.LabelNew pBackglass,"ModeTmr",digitLCD,		3,RGB(255, 255, 255)	,0,0,0 ,42,38    ,1,1	

	puPlayer.LabelNew pBackglass,"CheckGP",dmddef,			4,RGB(0, 0, 0)			,0,0,0 ,92,60  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckTS",dmddef,			4,RGB(0, 0, 0)			,0,0,0 ,77,60  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckV",dmddef,			4,RGB(0, 0, 0)			,0,1,0 ,74,60  ,1,1	
	puPlayer.LabelNew pBackglass,"Check1",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,71,65  ,1,1	
	puPlayer.LabelNew pBackglass,"Check2",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,71,69  ,1,1	
	puPlayer.LabelNew pBackglass,"Check3",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,71,73  ,1,1	
	puPlayer.LabelNew pBackglass,"Check4",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,71,77  ,1,1	
	puPlayer.LabelNew pBackglass,"Check5",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,71,81  ,1,1	
	puPlayer.LabelNew pBackglass,"Check6",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,71,84  ,1,1	

	puPlayer.LabelNew pBackglass,"CheckT",dmddef,			4,RGB(0, 0, 0)			,0,0,0 ,89,60  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckS1",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,89,65  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckS2",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,89,69  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckS3",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,89,73  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckS4",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,89,77  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckS5",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,89,81  ,1,1	
	puPlayer.LabelNew pBackglass,"CheckS6",dmddef,			2.6,RGB(0, 0, 0)		,0,0,0 ,89,84  ,1,1	


	puPlayer.LabelNew pBackglass,"Play1score",digitLCD,		6,RGB(255, 255, 255)  	,0,2,0 ,22,90    ,1,1
	puPlayer.LabelNew pBackglass,"Play2score",digitLCD,		6,RGB(255, 255, 255)  	,0,2,0 ,45,90    ,1,1
	puPlayer.LabelNew pBackglass,"Ball"		 ,digitLCD,		4,RGB(255, 255, 255)  	,0,1,0 ,50,92    ,1,1
	puPlayer.LabelNew pBackglass,"Play3score",digitLCD,		6,RGB(255, 255, 255)  	,0,2,0 ,73,90    ,1,1
	puPlayer.LabelNew pBackglass,"Play4score",digitLCD,		6,RGB(255, 255, 255)  	,0,2,0 ,98,90    ,1,1

	puPlayer.LabelNew pBackglass,"SplashMsg1",dmddef,		10,RGB(255, 205, 0)	,0,1,1 ,50,50    ,1,1
	puPlayer.LabelNew pBackglass,"SplashMsg2",dmddef,		10,RGB(255, 205, 0)	,0,1,1 ,50,60    ,1,1


	PuPlayer.LabelShowPage pBackglass, 1,0,""

	puPlayer.LabelSet pBackglass,"CheckGP", TotalGamesPlayed, 1,""
	puPlayer.LabelSet pBackglass,"CheckTS", HighScoreName(0) & " " & HighScore(0), 1,""
	puPlayer.LabelSet pBackglass,"CheckV", myVersion, 1,""
	puPlayer.LabelSet pBackglass,"Check1", "", 1,"{'mt':2,'ypos':65.7}"
	puPlayer.LabelSet pBackglass,"Check2", "", 1,"{'mt':2,'ypos':69.3}"
	puPlayer.LabelSet pBackglass,"Check3", "", 1,"{'mt':2,'ypos':72.7}"
	puPlayer.LabelSet pBackglass,"Check4", "", 1,"{'mt':2,'ypos':76.1}"
	puPlayer.LabelSet pBackglass,"Check5", "", 1,"{'mt':2,'ypos':79.5}"
	puPlayer.LabelSet pBackglass,"Check6", "", 1,"{'mt':2,'ypos':82.8}"

	puPlayer.LabelSet pBackglass,"CheckS1", "", 1,"{'mt':2,'ypos':65.7}"
	puPlayer.LabelSet pBackglass,"CheckS2", "", 1,"{'mt':2,'ypos':69.3}"
	puPlayer.LabelSet pBackglass,"CheckS3", "", 1,"{'mt':2,'ypos':72.7}"
	puPlayer.LabelSet pBackglass,"CheckS4", "", 1,"{'mt':2,'ypos':76.1}"
	puPlayer.LabelSet pBackglass,"CheckS5", "", 1,"{'mt':2,'ypos':79.5}"
	puPlayer.LabelSet pBackglass,"CheckS6", "", 1,"{'mt':2,'ypos':82.8}"


'	puPlayer.LabelSet pBackglass,"Play1scoreX", "PuPOverlays\\Pizza3x.png", 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':1}"
'	puPlayer.LabelSet pBackglass,"Play2scoreX", "PuPOverlays\\Pizza3x.png", 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':25}"
'	puPlayer.LabelSet pBackglass,"Play3scoreX", "PuPOverlays\\Pizza3x.png", 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':53}"
'	puPlayer.LabelSet pBackglass,"Play4scoreX", "PuPOverlays\\Pizza3x.png", 1,"{'mt':2,'color':111111,'width':22, 'height':12,'yalign':0,'ypos':86.0,'xpos':77}"
'	puPlayer.LabelSet pBackglass,"Play1scoreXTime", "10", 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':12.2}"
'	puPlayer.LabelSet pBackglass,"Play2scoreXTime", "20", 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':36.1}"
'	puPlayer.LabelSet pBackglass,"Play3scoreXTime", "30", 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':64.1}"
'	puPlayer.LabelSet pBackglass,"Play4scoreXTime", "40", 1,"{'mt':2,'color':255,'width':22, 'height':12,'yalign':0,'xalign':1,'ypos':87.5,'xpos':88}"
'	puPlayer.LabelSet pBackglass,"ModeTmr", "PuPOverlays\\1.png", 1,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"
'	puPlayer.LabelSet pBackglass,"PProgress", "video-pizzas\\gp-8-0.png", 1,"{'mt':2,'color':255,'width':26.1, 'height':46.5,'yalign':1,'xalign':1,'ypos':55.2,'xpos':50.15}"


	puPlayer.LabelSet pBackglass,"CollectLbl1",	" Mode"	,1,""
	puPlayer.LabelSet pBackglass,"CollectLbl2",	" Space Pizza"	,1,""
	puPlayer.LabelSet pBackglass,"CollectLbl3",	" Video Mode"	,1,""
	puPlayer.LabelSet pBackglass,"CollectLbl4",	" Beer Frenzy"	,1,""
	puPlayer.LabelSet pBackglass,"CollectLbl5",	" Extra Ball"	,1,""
	puPlayer.LabelSet pBackglass,"CollectLbl6",	" Flippen Mad"	,1,""
	puPlayer.LabelSet pBackglass,"CollectLbl7",	" Tip Jar 2x"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal1",	"72"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal2",	"2"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal3",	"9"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal4",	"6"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal5",	"8"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal6",	"20"	,1,""
	puPlayer.LabelSet pBackglass,"CollectVal7",	"6"	,1,""

	puPlayer.LabelSet pBackglass,"CollectTyp1",	" Spins"	,1,""
	puPlayer.LabelSet pBackglass,"CollectTyp2",	" Locks"	,1,""
	puPlayer.LabelSet pBackglass,"CollectTyp3",	" Ramps"	,1,""
	puPlayer.LabelSet pBackglass,"CollectTyp4",	" Multiplier X"	,1,""
	puPlayer.LabelSet pBackglass,"CollectTyp5",	" Slices Eaten"	,1,""
	puPlayer.LabelSet pBackglass,"CollectTyp6",	" Loop-de-loops"	,1,""
	puPlayer.LabelSet pBackglass,"CollectTyp7",	" Tip Bumper"	,1,""

	puPlayer.LabelSet pBackglass,"Play1score",	"0"	,1,""
	puPlayer.LabelSet pBackglass,"Play2score",	"0"	,1,""
	puPlayer.LabelSet pBackglass,"Play3score",	"0"	,1,""
	puPlayer.LabelSet pBackglass,"Play4score",	"0"	,1,""
	puPlayer.LabelSet pBackglass,"Ball",		"0"	,1,""

'	puPlayer.LabelSet pBackglass,"DetailCollect",	" Veggies"	,1,""
'	PuPlayer.LabelSet pBackglass, "Item1", "PuPOverlays\\mushroom.png",1, "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':75}"
'	PuPlayer.LabelSet pBackglass, "Item2", "PuPOverlays\\pepper.png",1,   "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':84}"
'	PuPlayer.LabelSet pBackglass, "Item3", "PuPOverlays\\Olive.png",1,    "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':27,'xpos':92}"
'	PuPlayer.LabelSet pBackglass, "Item4", "PuPOverlays\\pepperoni.png",1,"{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':79.5}"
'	PuPlayer.LabelSet pBackglass, "Item5", "PuPOverlays\\sardine.png",1,  "{'mt':2,'color':111111,'width':4, 'height':8,'yalign':0,'ypos':31,'xpos':88}"
'	puPlayer.LabelSet pBackglass,"DetailMode",	" Pizza Time"	,1,""
'	puPlayer.LabelSet pBackglass,"DetailCount",	" 12"	,1,""
'	puPlayer.LabelSet pBackglass,"DetailType",	" Ingedients"	,1,""


	puPlayer.LabelNew pBackglass,"HighScore1",dmddef,	15,RGB(255, 255, 255)  	,0,1,0 ,0,25   ,2,1				' Attract screen so when we show the Bonus we dont see all this info
	puPlayer.LabelNew pBackglass,"HighScore2",dmddef,	15,RGB(255, 255, 255)  	,0,1,0 ,0,45   ,2,1				' Attract screen so when we show the Bonus we dont see all this info
	puPlayer.LabelNew pBackglass,"HighScore3",dmddef,	15,RGB(255, 255, 255)  	,0,1,0 ,0,75   ,2,1				' Attract screen so when we show the Bonus we dont see all this info
	'PuPlayer.LabelShowPage pBackglass, 2,0,""

	' Screen 3 
	puPlayer.LabelNew pBackglass,"EnterHS1",dmdRetro,	3,RGB(255, 255, 255)  	,0,1,0 ,0,36   ,3,1			' HS Entry
	puPlayer.LabelNew pBackglass,"EnterHS2",dmdRetro,	3,RGB(255, 255, 255)  	,0,1,0 ,0,38   ,3,1			' HS Entry
	puPlayer.LabelNew pBackglass,"EnterHST",dmdRetro,	3,RGB(255, 255, 255)  	,0,1,0 ,0,40   ,3,1			' HS Entry
	puPlayer.LabelNew pBackglass,"EnterScr",dmdRetro,	3,RGB(255, 255, 255)  	,0,2,0 ,46,42  ,3,1			' HS Entry
	puPlayer.LabelNew pBackglass,"EnterINI",dmdRetro,	3,RGB(255, 255, 255)  	,0,2,0 ,55,42  ,3,1			' HS Entry
	puPlayer.LabelNew pBackglass,"EnterHST2",dmdRetro,	3,RGB(255, 255, 255)  	,0,1,0 ,0,42   ,3,1			' HS Entry
	puPlayer.LabelNew pBackglass,"EnterRT",dmdRetro,	3, RGB(0, 255, 255)  	,0,2,0 ,0 ,40  ,3,1			' HS Rank Title 
	puPlayer.LabelNew pBackglass,"EnterST",dmdRetro,	3, RGB(0, 255, 255)  	,0,2,0 ,46,40  ,3,1			' HS Score Title 
	puPlayer.LabelNew pBackglass,"EnterNT",dmdRetro,	3, RGB(0, 255, 255)  	,0,2,0 ,55,40  ,3,1			' HS Name Title 
	for i = 0 to 5
		puPlayer.LabelNew pBackglass,"EnterR"&i,dmdRetro,	3, RGB(255, 255, 255)  	,0,2,0 ,0 ,43  ,3,1			' HS Rank#
		puPlayer.LabelNew pBackglass,"EnterS"&i,dmdRetro,	3, RGB(255, 255, 255)  	,0,2,0 ,46,43  ,3,1			' HS Score
		puPlayer.LabelNew pBackglass,"EnterN"&i,dmdRetro,	3, RGB(255, 255, 255)  	,0,2,0 ,55,43  ,3,1			' HS Name
	Next

	' Screen 4
	puPlayer.LabelNew pBackglass,"hsTitle",dmddef,	3, RGB(255, 255, 255)  	,0,1,0 ,50,36  ,1,1			' HS Rank Title 
	puPlayer.LabelNew pBackglass,"hsRT",digitLCD,		3, RGB(255, 255, 255)  	,0,2,0 ,0 ,40  ,1,1			' HS Rank Title 
	puPlayer.LabelNew pBackglass,"hsST",digitLCD,		3, RGB(255, 255, 255)  	,0,2,0 ,46,40  ,1,1			' HS Score Title 
	puPlayer.LabelNew pBackglass,"hsNT",digitLCD,		3, RGB(255, 255, 255)  	,0,2,0 ,55,40  ,1,1			' HS Name Title 

	for i = 19 to 0 step -1
		puPlayer.LabelNew pBackglass,"hsR"&i,digitLCD,		3, RGB(255, 255, 255)  	,0,2,0 ,0 ,43  ,1,1			' HS Rank#
		puPlayer.LabelNew pBackglass,"hsS"&i,digitLCD,		3, RGB(255, 255, 255)  	,0,2,0 ,46,43  ,1,1			' HS Score
		puPlayer.LabelNew pBackglass,"hsN"&i,digitLCD,		3, RGB(255, 255, 255)  	,0,2,0 ,55,43  ,1,1			' HS Name
	Next

	PuPlayer.LabelShowPage pBackglass, 1,0,""
	puPlayer.LabelSet pBackglass,"hsTitle",	"Daily"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':50  ,'ypos':36}"
	puPlayer.LabelSet pBackglass,"hsRT",	"#"				,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':42  ,'ypos':42}"
	puPlayer.LabelSet pBackglass,"hsST",	"Score"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':54.5,'ypos':42}"
	puPlayer.LabelSet pBackglass,"hsNT",	"INI"			,1,"{'mt':2,'size':2.2,'color':"&RGB(255, 255, 255)&",'xpos':60  ,'ypos':42}"

	Dim col				' Init colors and positions 
	dim vis
	for i = 0 to 19
		select case i
			case 0:col=RGB(255, 000, 000)
			case 1:col=RGB(248, 024, 148)
			case 2:col=RGB(255, 255, 000)
			case 3:col=RGB(255, 255, 000)
			case else
				col=RGB(000, 255, 000)
		End Select
		if i>5 then vis=0 else vis=0
		puPlayer.LabelSet pBackglass,"hsR"&i,	""			,vis,"{'mt':2,'size':2.2,'color':"&col&",'xpos':42  ,'ypos':"& (47+i*4) &"}"
		puPlayer.LabelSet pBackglass,"hsS"&i,	""		    ,vis,"{'mt':2,'size':2.2,'color':"&col&",'xpos':54.5,'ypos':"& (47+i*4) &"}"
		puPlayer.LabelSet pBackglass,"hsN"&i,	""			,vis,"{'mt':2,'size':2.2,'color':"&col&",'xpos':60  ,'ypos':"& (47+i*4) &"}"
	Next

	' Page 5
	puPlayer.LabelNew pBackglass,"OverMessage1",guardians,	15,RGB(255, 255, 255)  	,0,1,0 ,0,25   ,5,1				' 
	puPlayer.LabelNew pBackglass,"OverMessage2",guardians,	18,RGB(255, 255, 255)  	,0,1,0 ,0,45   ,5,1				' 
	puPlayer.LabelNew pBackglass,"OverMessage3",guardians,	15,RGB(255, 255, 255)  	,0,1,0 ,0,65   ,5,1				' 


	' Page 6
	for i = 0 to 7
		puPlayer.LabelNew pBackglass,"Bonus"&i, 	dmddef,		3.5, RGB(255, 255, 255)  	,0,0,0 ,41 ,42.0+(i*3)  ,6,1	' Bonus Name 
		puPlayer.LabelNew pBackglass,"BonusScore"&i, dmddef,	3.5, RGB(255, 255, 255)  	,0,2,0 ,59 ,42.0+(i*3)  ,6,1	' Bonus Amount
	Next
	puPlayer.LabelNew pBackglass,"BonusTotal",dmddef,	6, RGB(255, 0, 0)  	,0,1,0 ,0 ,70  ,6,1			' HS Rank#


if PuPDMDDriverType=pDMDTypeReal Then 'using RealDMD Mirroring.  **********  128x32 Real Color DMD  

	' Player					Balls
	' 			  SCORE
	'
	' 				Switch		Credits


	'Page 1 (default score display)
	'					   Scrn LblName    Fnt    Size	Color	 			R AxAy X Y pagenum Visible 
		PuPlayer.LabelNew pDMD,"Play1"   ,dmddef,30,	1849448				,0,2,2,27,0,1,1
		PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,30,	1849448 			,0,2,2,88,0,1,1
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,15,	3397879				,0,1,0, 0,0,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdalt,60,	3397879 			,0,1,1, 0,0,1,1


	'Page 2 (default Text Splash 1 Big Line)
		 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0

	'Page 3 (default Text Splash 2 and 3 Lines)
		 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
	     PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,55,3,0


	'Page 4 (2 Line Gameplay DMD)
		 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,60,8454143,0,1,0,0,0,4,0
	     PuPlayer.LabelNew pDMD,"Splash4b",dmddef,50,33023,0,1,2,0,100,4,0


	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

 	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0



END IF  ' use PuPDMDDriver

if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************

	'dmddef="Impact"
	dmdalt="PKMN Pinball"    
    dmdfixed="Instruction"
	dmdscr="Impact"  'main score font
	dmddef="Impact"

	'Page 1 (default score display)
	'					   Scrn LblName    Fnt    Size	Color	 R AxAy X Y pagenum Visible 
	'Page 1 (default score display)
	'					   Scrn LblName    Fnt    Size	Color	 			R AxAy X Y pagenum Visible 
		PuPlayer.LabelNew pDMD,"Play1"   ,dmddef,20,	1849448				,0,2,2,27,0,1,1
		PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,20,	1849448 			,0,2,2,88,0,1,1
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,15,	3397879				,0,1,0, 0,0,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdalt,60,	3397879 			,0,1,1, 0,0,1,1


	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,60,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,50,33023,0,1,2,0,100,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver

if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

	'dmddef="Impact"
	dmdalt="PKMN Pinball"    
    dmdfixed="Instruction"
	dmdscr="Impact"  'main score font
	dmddef="Impact"

	'Page 1 (default score display)
	'					   Scrn LblName    Fnt    Size	Color	 			R AxAy X Y pagenum Visible 
		PuPlayer.LabelNew pDMD,"Play1"   ,dmddef,20,	1849448				,0,2,2,27,0,1,1
		PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,20,	1849448 			,0,2,2,88,0,1,1
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,15,	3397879				,0,1,0, 0,0,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdalt,60,	3397879 			,0,1,1, 0,0,1,1


	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,60,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,50,33023,0,1,2,0,100,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver




end Sub 'page Layouts


'*****************************************************************
'        PUPDMD Custom SUBS/Events for each Table1
'     **********    MODIFY THIS SECTION!!!  ***************
'*****************************************************************
'
'
'  we need to somewhere in code if applicable
'
'   call pDMDStartGame,pDMDStartBall,pGameOver,pAttractStart
'
'
'
'

Sub pDMDStartGame
	if bUsePUPDMD then 
debug.print "STOP ATTRACT"
		pInAttract=false
		bHSAttract=false
'		if pDMDVideoPlaying Then 
'			PuPlayer.playstop pDMD
'			pDMDVideoPlaying=False
'		End if
		' Clear the overvideo just in case it is playing 
		playclear pOverVid
		PuPlayer.SetBackground pOverVid, 0
		PuPlayer.SetLoop pOverVid, 0
		PuPlayer.LabelSet pOverVid,"OverMessage3", "", 1, "{'mt':2,'color':"&RGB(255, 255, 255)&", 'size': 10,'ypos': 75}"
		PuPlayer.LabelSet pBackglass,"HighScore1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScore2","",1,"{'mt':2}"
		PuPlayer.LabelSet pBackglass,"HighScore3","",1,""

		PuPlayer.SetBackGround pDMD, 0
		PuPlayer.playstop pDMD

		pDMDSetPage(pScores)   'set blank text overlay page.
		pBGGamePlay
	end If
end Sub

'Dim bBGPlayingVideo:bBGPlayingVideo=False
'Sub pBGPlayVideo(VideoName, length)
'debug.print "Starting BG Video"
'	PauseSong()
'	bBGPlayingVideo=True
'	dim pPriority:pPriority=1
'	PuPlayer.playlistplayex pBackglass,"PuPVideos",VideoName, 100, pPriority  'should be an attract background (no text is displayed)
'	PuPlayer.SetBackGround pBackglass, 1
'	if length <> -1 then 					' Mode video, we just play until the timer runs out and call stop (Visually looks better)
'		tmrBGVideo.Interval = length
'		tmrBGVideo.Enabled = True
'	End If 
'End sub 
'
'Sub tmrBGVideo_Timer()
'	tmrBGVideo.Enabled = False
'	pBGPlayVideoDone()
'End Sub 
'
'Sub pBGPauseVideo()
'	
'End Sub
'
'Sub pBGPlayVideoDone()
'debug.print "Stopping BG Video"
'	PuPlayer.SetBackGround pBackglass, 0
'	bBGPlayingVideo=False
'	pDMDEvent(kDMD_PlayerMode)
'	PlaySong ""
'End Sub
'

'  "c:\PinballX\ffmpeg.exe" -loop 1 -i BonusScreen-2.png -c:v libx264 -t 8 -pix_fmt yuv420p -vf scale=1920:1080 BonusScreen-2.mp4
'  ffmpeg -r 60 -f image2 -s 1920x1080 -i BonusScreen-2.png -vcodec libx264 -crf 25  -pix_fmt yuv420p -vf scale=1920:1080  BonusScreen-2.mp4
' ffmpeg -r 60 -f image2 -s 1920x1080 -i BonusScreen-2.png -i clear.mp3 -vcodec libx264 -crf 25  -pix_fmt yuv420p -vf scale=1920:1080 -strict -1 -acodec copy BonusScreen-2.mp4
'ffmpeg -r 30 -f image2 -s 1920x1080 -i BonusScreen-2.png -i clear2.mp3 -vcodec libx264 -crf 25 -b 4M -pix_fmt yuv420p -vf scale=1920:1080 -acodec copy BonusScreen-2.mp4

' Must have contrained baseline 
' ffmpeg -r 30 -s 1920x1080 -i BonusScreen-2.png -i clear.mp3 -vcodec libx264 -crf 25 -b 4M -pix_fmt yuv420p -vf scale=1920:1080 -profile:v baseline -metadata:s:v:0 language=eng -metadata:s:a:0 language=eng -acodec copy BonusScreen-2.mp4
'GOOD MP4  ffmpeg -r 30 -s 1920x1080 -i BonusScreen-2.png -vcodec libx264 -crf 25 -b 4M -pix_fmt yuv420p -vf scale=1920:1080 -profile:v baseline BonusScreen-2.mp4

'GOOD WEBM ffmpeg -i BonusScreen-2.png -c:v libvpx -pix_fmt yuva420p -b:v 1M -auto-alt-ref 0 BonusScreen-2.webm
' NOTE: need transparency on the Screen to make this work



Sub pBGGamePlay
	if bUsePUPDMD then
		'bFlashing = False
		PuPlayer.playlistplayex pBackglass,"PupOverlays","backglass-1g-notext.png", 1, 1
		playmedia "backSample3.mp4", "DMDBackground", pBackglass, "", -1, "", 1, 1
		'PuPlayer.playlistplayex pBackglass,"DMDBackground","backglass-1g-notext.png",0,1

		PuPlayer.LabelShowPage pBackglass,1,0,""
	End If
End Sub

Sub pBGCutScene
	if bUsePUPDMD then

	End If
End Sub

Sub pBgShowBonus(Message1, Message2)
	if bUsePUPDMD then
debug.print "Showing Bonus: " & Message1 & " - " & Message2
		PuPlayer.LabelSet pBonusScreen,"Message1", Message1 ,1,""
		PuPlayer.LabelSet pBonusScreen,"Message2", Message2 ,1,""
	End If
End Sub

Sub pDMDStartBall
	if bUsePUPDMD then 
	End If 
end Sub

Sub pDMDEvent(id)
	if bUsePUPDMD then
'debug.print "pDMDEvent " & id 
		PuPEvent(id)  ' Send an event to the pup pack the E500 trigger
	End If 
End Sub

'Sub pDMDBallLost
'	if bUsePUPDMD then 
'		PuPEvent(500)  ' Send an event to the pup pack the E500 trigger
'	End If 
'End Sub

'Sub pDMDGameOver
'debug.print "Game Over"
'	StartAttractMode
'	if bUsePUPDMD then
'		PuPlayer.LabelShowPage pBackglass, 2,0,""
'		'pDMDEvent(kDMD_Attract)
'		pAttractStart		' Take too long to start and kills Game Over 
'	End If 
'end Sub
'
'Sub pAttractStart
'	if (pInAttract = False) then 
'	debug.print "STARTING ATTRACT"
'		'pDMDSetPage(pDMDBlank)   'set blank text overlay page.
'		pCurAttractPos=0
'		pDMDStartUP
'	end if 
'end Sub

Sub pDMDStartUP
	'Set Background video on DMD
	PuPlayer.playlistplayex pDMD,"scene","start.mp4",0,1  'should be an attract background (no text is displayed)
	PuPlayer.SetBackground pDMD,1	

	'playclear pBackglass
	PuPlayer.playlistplayex pBackglass,"DMDBackground","blank.mp4", 1, 1
	PuPlayer.SetLoop pBackglass, 1
	PuPlayer.SetBackground pBackglass, 1

	debug.print "STARTING ATTRACT2"
	pupDMDDisplay "DMDBackground","Welcome","attract.mp4",9,0,10
'	pInAttract=true
	bHSAttract=true
	PuPlayer.LabelShowPage pBackglass, 4,0,""
	UpdateAttractHS
end Sub

DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
pCurAttractPos=pCurAttractPos+1
debug.print "ATTRACT Next" & pCurAttractPos

  if bGameInPlay Then PriorityReset=2000
  if bGameInPlay and pCurAttractPos=1 then pCurAttractPos=2		' During InstantInfo skip Intro
  if bGameInPlay and pCurAttractPos=7 then pCurAttractPos=8		' During InstantInfo skip credits 

  Select Case pCurAttractPos
'			      Name       Line1^Line2.. 	Video			 T  Flash	Priority
  Case 1:
	pupDMDDisplay "DMDBackground", "Insert Coin",	"attract.mp4" ,9, 1,		10
	'PuPlayer.LabelShowPage pBackglass, 1,0,""
	PuPlayer.playlistplayex pBackglass,"DMDBackground","blank.mp4", 1, 1
	PuPlayer.SetLoop pBackglass, 1
	PuPlayer.SetBackground pBackglass, 1
'	PuPlayer.LabelSet pBackglass,"OverMessage1","",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage2","",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage3","COPYRIGHT XXXXXX ",1,"{'mt':2,'color':255, 'size': 4,'ypos': 90}"
  Case 2:
	if bGameInPlay=False Then 
		pupDMDDisplay "DMDBackground", "Game Over",	"attract.mp4",3, 1,		10
		PuPlayer.LabelSet pBackglass,"OverMessage3", "", 1, "{'mt':2,'color':"&RGB(255, 255, 255)&", 'size': 10,'ypos': 65}"
	End If 

'	PuPlayer.LabelSet pBackglass,"OverMessage1","GRAND CHAMPION",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage2",HighScoreName(0),1,"{'mt':2}"
'	PuPlayer.LabelSet pBackglass,"OverMessage3",FormatScore(HighScore(0)),1,""
  Case 3:
	if bGameInPlay=False Then pupDMDDisplay "DMDBackground", "Insert Coin", "attract.mp4",3, 1,		10
'	PuPlayer.LabelSet pBackglass,"OverMessage1","HIGH SCORE #1",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage2",HighScoreName(1),1,"{'mt':2}"
'	PuPlayer.LabelSet pBackglass,"OverMessage3",FormatScore(HighScore(1)),1,""
  Case 4:
	if bGameInPlay=False Then pupDMDDisplay "DMDBackground", "Insert Coin", "attract.mp4",3, 1,		10
'	PuPlayer.LabelSet pBackglass,"OverMessage1","HIGH SCORE #2",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage2",HighScoreName(2),1,"{'mt':2}"
'	PuPlayer.LabelSet pBackglass,"OverMessage3",FormatScore(HighScore(2)),1,""
  Case 5:
	if bGameInPlay=False Then pupDMDDisplay "DMDBackground", "Insert Coin", "attract.mp4",3, 1,		10
'	PuPlayer.LabelSet pBackglass,"OverMessage1","HIGH SCORE #3",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage2",HighScoreName(3),1,"{'mt':2}"
'	PuPlayer.LabelSet pBackglass,"OverMessage3",FormatScore(HighScore(3)),1,""
  Case 6:
	if bGameInPlay=False Then pupDMDDisplay "DMDBackground", "Insert Coin", "attract.mp4",3, 1,		10
'	PuPlayer.LabelSet pBackglass,"OverMessage1","Games Played",1,""
'	PuPlayer.LabelSet pBackglass,"OverMessage2",FormatScore(TotalGamesPlayed),1,"{'mt':2}"
'	PuPlayer.LabelSet pBackglass,"OverMessage3"," ",1,""
  Case 7:
	pupDMDDisplay "DMDBackground", "Insert Coin", "attract.mp4",3, 1,		10
'	PuPlayer.LabelSet pBackglass,"HighScore1","Authors:",1,""
'	PuPlayer.LabelSet pBackglass,"HighScore2","Scott ",1,"{'mt':2}"
'	PuPlayer.LabelSet pBackglass,"HighScore3","and Daphishbowl",1,""
  case 8:
	if bGameInPlay then
'		PuPlayer.LabelSet pBackglass,"OverMessage1","In Game Details",1,""
'		PuPlayer.LabelSet pBackglass,"OverMessage1", 999 ,1,"{'mt':2}"
'		PuPlayer.LabelSet pBackglass,"OverMessage1","",1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  case 9:
	if bGameInPlay then
'		PuPlayer.LabelSet pBackglass,"OverMessage1", "Multipliers", 1,""
'		PuPlayer.LabelSet pBackglass,"OverMessage1", "Play: x",1, ""     ' & (Multiplier3x * PlayMultiplier),1,"{'mt':2}"
'		PuPlayer.LabelSet pBackglass,"OverMessage1", "Bonus: x" & BonusMultiplier(CurrentPlayer),1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case Else
    pCurAttractPos=0
    pAttractNext 'reset to beginning
  end Select

end Sub


'************************ called during gameplay to update Scores ***************************
Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
Dim StatusStr
	if pDMDCurPage <> pScores then Exit Sub

	PuPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(Score(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pDMD,"Play1","Player " & CurrentPlayer+1,1,""
	if BallsRemaining(CurrentPlayer) = 0 then 
		PuPlayer.LabelSet pDMD,"Ball","Ball "  &  bpgcurrent & "/" & bpgcurrent,1,""
	else 
		PuPlayer.LabelSet pDMD,"Ball","Ball "  &  bpgcurrent - BallsRemaining(CurrentPlayer) + 1 & "/" & bpgcurrent,1,""
	End If 
	PuPlayer.LabelSet pDMD,"MsgScore","",1,""

	if Score(0) > 10000000 then StatusStr=",'size':5}" else StatusStr = "}"
	puPlayer.LabelSet pBackglass,"Play1score",	FormatNumber(Score(0),0),1,P1ScoreColor & StatusStr
	if Score(1) > 10000000 then StatusStr=",'size':5}" else StatusStr = "}"
	puPlayer.LabelSet pBackglass,"Play2score",	FormatNumber(Score(1),0),1,P2ScoreColor & StatusStr
	if Score(2) > 10000000 then StatusStr=",'size':5}" else StatusStr = "}"
	puPlayer.LabelSet pBackglass,"Play3score",	FormatNumber(Score(2),0),1,P3ScoreColor & StatusStr
	if Score(3) > 10000000 then StatusStr=",'size':5}" else StatusStr = "}"
	puPlayer.LabelSet pBackglass,"Play4score",	FormatNumber(Score(3),0),1,P4ScoreColor & StatusStr
	if BallsRemaining(CurrentPlayer) = 0 then 
		PuPlayer.LabelSet pBackglass,"Ball", bpgcurrent & "/" & bpgcurrent,1,""
	else 
		PuPlayer.LabelSet pBackglass,"Ball", bpgcurrent - BallsRemaining(CurrentPlayer) + 1 & "/" & bpgcurrent,1,""
	End If 

end Sub





	Sub playsfx(channel)
		dim sfxnum:sfxnum=0
		select case channel
			case "hittarget"
			sfxnum=RndNum(1,12)
			case "standup"
			sfxnum=RndNum(13,19)
			case "deathbumper"
			sfxnum=RndNum(20,20)
			case "loopde"
			sfxnum=RndNum(21,28)
			case "boxopen"
			sfxnum=RndNum(29,29)
			case "pizzawheel"
			sfxnum=RndNum(30,30)
			case "beep"
			sfxnum=RndNum(39,39)
			case "shoot"
			sfxnum=RndNum(31,38)
			case "bump"
			sfxnum=RndNum(40,57)
			case "sling"
			sfxnum=RndNum(58,60)
			case "lane"
			sfxnum=RndNum(61,61)
			case "ultracombo"
			sfxnum=RndNum(62,62)
			case "beam"
			sfxnum=RndNum(63,65)
			case "deliver"
			sfxnum=RndNum(66,67)
			case "pickup"
			sfxnum=RndNum(68,71)
		end Select
		'chill some sounds in mb
		if bMultiBallMode = True then
			if channel = "deathbumper" then
				sfxnum = 0
			end if
		end if

		select case sfxnum
			Case 1 : PlaySoundVol "ht1", voldef
			Case 2 : PlaySoundVol "ht2", voldef
			Case 3 : PlaySoundVol "ht3", voldef
			Case 4 : PlaySoundVol "ht4", voldef
			Case 5 : PlaySoundVol "ht5", voldef
			Case 6 : PlaySoundVol "ht6", voldef
			Case 7 : PlaySoundVol "ht7", voldef
			Case 8 : PlaySoundVol "ht8", voldef
			Case 9 : PlaySoundVol "ht9", voldef
			Case 10 : PlaySoundVol "ht10", voldef
			Case 11 : PlaySoundVol "ht11", voldef
			Case 12 : PlaySoundVol "ht12", voldef
			Case 13 : PlaySoundVol "su1", voldef
			Case 14 : PlaySoundVol "su2", voldef	
			Case 15 : PlaySoundVol "su3", voldef	
			Case 16 : PlaySoundVol "su4", voldef	
			Case 17 : PlaySoundVol "su5", voldef	
			Case 18 : PlaySoundVol "su6", voldef	
			Case 19 : PlaySoundVol "su7", voldef
			Case 20 : PlaySoundVol "ill", voldef	
			Case 21 : PlaySoundVol "l1", voldef
			Case 22 : PlaySoundVol "l2", voldef
			Case 23 : PlaySoundVol "l3", voldef
			Case 24 : PlaySoundVol "l4", voldef
			Case 25 : PlaySoundVol "l5", voldef
			Case 26 : PlaySoundVol "l6", voldef
			Case 27 : PlaySoundVol "l7", voldef
			Case 28 : PlaySoundVol "l8"	, voldef
			Case 29 : PlaySoundVol "pizzaopen", voldef
			Case 30 : PlaySoundVol "pizzawheel", voldef
			Case 31 : PlaySoundVol "ss1", voldef
			Case 32 : PlaySoundVol "ss2", voldef
			Case 33 : PlaySoundVol "ss3", voldef
			Case 34 : PlaySoundVol "ss4", voldef
			Case 35 : PlaySoundVol "ss5", voldef
			Case 36 : PlaySoundVol "ss6", voldef
			Case 37 : PlaySoundVol "ss7", voldef
			Case 38 : PlaySoundVol "ss8", voldef
			Case 39 : PlaySoundVol "s12", voldef
			Case 40 : PlaySoundVol "b1", voldef
			Case 41 : PlaySoundVol "b2", voldef
			Case 42 : PlaySoundVol "b3", voldef
			Case 43 : PlaySoundVol "b4", voldef
			Case 44 : PlaySoundVol "b5", voldef
			Case 45 : PlaySoundVol "b6", voldef
			Case 46 : PlaySoundVol "b7", voldef
			Case 47 : PlaySoundVol "b8", voldef
			Case 48 : PlaySoundVol "b9", voldef
			Case 49 : PlaySoundVol "b10", voldef
			Case 50 : PlaySoundVol "b11", voldef
			Case 51 : PlaySoundVol "b12", voldef
			Case 52 : PlaySoundVol "b13", voldef
			Case 53 : PlaySoundVol "b14", voldef
			Case 54 : PlaySoundVol "b15", voldef
			Case 55 : PlaySoundVol "b16", voldef
			Case 56 : PlaySoundVol "b17", voldef
			Case 57 : PlaySoundVol "b18", voldef
			Case 58 : PlaySoundVol "sling1", voldef
			Case 59 : PlaySoundVol "sling2", voldef
			Case 60 : PlaySoundVol "sling3"	, voldef
			Case 61 : PlaySoundVol "sardine", voldef
			Case 62 : PlaySoundVol "ultra", voldef
			Case 63 : PlaySoundVol "beam1", (voldef-.3)
			Case 64 : PlaySoundVol "beam2", (voldef-.3)
			Case 65 : PlaySoundVol "beam3", (voldef-.3)
			Case 66 : PlaySoundVol "deliver1", voldef
			Case 67 : PlaySoundVol "deliver2", voldef
			Case 68 : PlaySoundVol "pick1", voldef
			Case 69 : PlaySoundVol "pick2", voldef
			Case 70 : PlaySoundVol "pick3", voldef
			Case 71 : PlaySoundVol "pick4", voldef
		end Select
	end Sub


	dim htime:htime=55
	sub helper_timer
		htime=htime+1
		select case htime
			case 90
				helpful
			case 91
				htime = 0
		end Select
	end Sub

	sub helpful
		if helpfulcalls = 1 and bMultiBallMode = false and bGameInPLay = true and bAttractMode = false and hsbModeActive = False and cineon = 0 and inbonus = false and inminigame = 0 and modepickin = 0 and startingpizza=0 Then
			playmedia "","audio-helper",pCallouts,"",1500,"",1,1  ' (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)			
		end If
	end Sub




	Dim GIInit: GIInit=0

	Sub GameTimer1_Timer
		If PreloadMe = 1 then
			GIInit = GIInit +1
			select case GIInit  
				case 1:
					Strip1.visible=True
					Strip3.visible=True
					Strip5.visible=True
					Strip7.visible=True
					Strip9.visible=True
					Flasherflash004.visible=True
					Flasherflash007.visible=True
					Flasherflash006.visible=True
					Flasherflash005.visible=True
					Flasherflash008.visible=True
					Spot2.visible=True
					prgion.visible=True
					prgioff.visible=True
					backwallon.visible=True
					backwalloff.visible=True
					psgion.visible=true
					psgioff.visible=True
					pred.visible=True
					porange.visible=True
					pgreen.visible=True
					pblue.visible=True
					ppurple.visible=True
				case 2
					Strip1.visible=False
					Strip3.visible=False
					Strip5.visible=False
					Strip7.visible=False
					Strip9.visible=False
					Spot2.visible=False
					prgion.visible=False
					prgioff.visible=True
					backwallon.visible=False
					backwalloff.visible=True
					psgion.visible=False
					psgioff.visible=True
					pred.visible=False
					porange.visible=False
					pgreen.visible=False
					pblue.visible=False
					ppurple.visible=False
					GameTimer1.enabled = false
			end Select
		End If
	end Sub

	dim ucn1,ucn2,ucn3,ucn4,ucn5,ucn6
	ucn1=0:ucn2=0:ucn3=0:ucn4=0:ucn5=0:ucn6=0
	sub ultracombo(trig)
		if bMultiBallMode = false Then
			if ucn2=0 Then
				uct.enabled=true
			end if
			ucn6=ucn5
			ucn5=ucn4
			ucn4=ucn3
			ucn3=ucn2
			ucn2=ucn1
			ucn1=trig
			checkult
		end if
	end Sub

	sub checkult
debug.print "1:" & ucn1 & " 2" & ucn2 & " 3" & ucn3 & " 4" & ucn4 & " 5" & ucn5 & " 6" & ucn6	
		if ucn6=1 and ucn5=2 and ucn4=3 and ucn3=3 and ucn2=2 and ucn1=1 Then
			PlaySoundVol "ultra", 90
			ShowMsg "Ultra Combo!", "10,000" 'FormatScore(BonusPoints(CurrentPlayer))
			AddScore 10000
			backlamp "flash"
			lightrun yellow,circlein,3
			flasherspop yellow,"crazy" 'left,right,bottom,rightkick,top,crazy
				uct.enabled=0
				ucn1=0
				ucn2=0
				ucn3=0
				ucn4=0
				ucn5=0
				ucn6=0
				uctt=0
		end If
	
	end Sub

	dim uctt:uctt=0
	sub uct_timer
		uctt=uctt+1
		select case uctt
			case 5
				uct.enabled=0
				ucn1=0
				ucn2=0
				ucn3=0
				ucn4=0
				ucn5=0
				ucn6=0
				uctt=0
		end Select
	end Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - BALL FINDER
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	dim looktimer:looktimer = 0
	sub ballfinder_timer
		if BallFinderOn = 0 then exit sub
		Dim BOT, b
		BOT = GetBalls
		looktimer = looktimer + 1

		' if no balls then no look
		If UBound(BOT) = 1 Then 
			'looktimer = 0
		End If
	
		If bAttractMode = true Then
			looktimer = 0
		end if

		if hsbModeActive = True Then
			looktimer = 0
		end If

		If cineon = 1 Then
			looktimer = 0
		end if

		If inbonus = true Then
			looktimer = 0
		end if

		if inminigame = 1 Then
			looktimer = 0
		end if

		if modepickin = 1 Then
			looktimer = 0
		end if

		if startingpizza=1 Then
			looktimer = 0
		end if

		' play the rolling sound for each ball
		For b = 0 to UBound(BOT)
            If BallVel(BOT(b) ) > 1  Then
				looktimer = 0
            End If
		Next

		If LFPress = 1 or rfpress = 1 Then
			For b = 0 to UBound(BOT)
				if BOT(b).Y > 1554 and BOT(b).Y < 1750 Then			
					if Bot(B).X > 258 and Bot(B).X < 638 then
						debug.print "being trapped"
						looktimer = 0
					end if
				end if
			Next
		end if

		If LFPress = 1 Then
			For b = 0 to UBound(BOT)
				if BOT(b).Y > 644 and BOT(b).Y < 750 Then			
					if Bot(B).X > 210 and Bot(B).X < 750 then
						debug.print "top flipper trap"
						looktimer = 0
					end if
				end if
			Next
		end if



			For b = 0 to UBound(BOT)
				if BOT(b).Y > 1586 and BOT(b).Y < 1834 Then			
					if Bot(B).X > 838 and Bot(B).X < 962 then
						debug.print "in launch lane"
						looktimer = 0
					end if
				end if
			Next

		'debug.print UBound(BOT)
		'debug.print BallsOnPlayfield

		'debug.print "looktimer" & looktimer
		Select Case looktimer
			case 5:debug.print "looktimer 5"
			case 10:debug.print "looktimer 10"
			case 12:bumperrun
			case 15:kickerclear
			case 18:merclear
			case 20:ballstuckoption
		end Select

	end Sub

	sub bumperrun
		Bumper1_Hit
		Nudge 90, 6
		vpmtimer.addtimer 300, "Bumper3_Hit '"
		vpmtimer.addtimer 600, "Bumper4_Hit '"
		vpmtimer.addtimer 700, "forwardbump '"
		vpmtimer.addtimer 900, "Bumper5_Hit '"
	end Sub

	sub forwardbump
		Nudge 0, 7
	end sub

	sub kickerclear
		debug.print "clear the kickers"
		ckick.kick 82, 32, 0
		PlaySoundAt SoundFXDOF("mbpc-popper",116,DOFPulse,DOFContactors), kicker1
		pizzalocker.Kick 90, 41
		PlaySoundAt SoundFXDOF("mbpc-ballrelease", 114, DOFPulse, DOFContactors), BallRelease
		Kicker1.Kick 180, 1
		Kicker001.Kick 180, 1
		Kicker002.Kick 180, 1
		Kicker2.Kick 180, 1
		Kicker3.Kick 180, 1
	end sub

	Sub merclear
		KickerVideoMode.kick 110, 30, 0
		KickerExtraBall.kick 110, 30, 0
	end sub

	Sub ballstuckoption
		debug.print "ball stuck- add a ball"
		BallRelease.CreateSizedball BallSize / 2
		PlaySoundAt SoundFXDOF("mbpc-ballrelease", 114, DOFPulse, DOFContactors), BallRelease
		BallRelease.Kick 90, 41
		EnableBallSaver 5
	end Sub

