'==============================================================================================='
'																								' 			  	 
'										  Haunted House	     					'
'          	              		         Gottlieb (1982)            	 	                    '
'		  				  	 http://www.ipdb.org/machine.cgi?id=1133			                    '
'																								'
' 	  		 	 	  	            Created by: cyberpez		  			                    '    					 
'																								' 			  	 
'==============================================================================================='

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

' ***********************************************************************************************
' ***********************************************************************************************

Dim GIColorMod
Dim GIColorModType
Dim CCGI
Dim CCSD
Dim FlipperColor
Dim Instruction_Cards
Dim DesktopMode: DesktopMode = HH.ShowDT
Dim FSSMode: FSSMode = HH.ShowFSS
Dim BallMod
Dim BLPlastics
Dim PostLightsOnOrOff
Dim ExtraFlashers
Dim UnderApronLights 
Dim BlacklightTargets
Dim BumperCapLights
Dim WindowFome
Dim RomSet
Dim FlipperKeyMod 
Dim CMSFS
Dim ExtraGI
Dim WindowColor
Dim LowerLighting
Dim HideRails
Dim GameInfoCard

' ***********************************************************************************************
' OPTIONS
' ***********************************************************************************************

'Ball Size and Weight
Const BallRadius = 25
Const BallMass = 1

'BallMod
'0=Normal Ball
'1=Marbled ball Green / Red
'2=Marbled ball Green / Yellow

BallMod = 2


'Hide Siderails
'0 = Show Siderails
'1 = Hide Siderails

HideRails = 0

'''Flipper colors
'0 = Random
'1 = All Red
'2 = All Green
'3 = Green Red Green
'4 = Red Green Red

FlipperColor = 1


'''Color Match small flipper screw
'0 = Normal metal color screw
'1 = Color Match to Rubber

CMSFS = 0


'''Flipper Key Mod
'1 = Standard flipper buttons (normal mail and lower and manga for upper)
'2 = LazyMan flippers (only normal button for all flippers)

FlipperKeyMod = 1


'''GI Color MOD
'0 = Random
'1 = Normal
'2 = All Red
'3 = All Green
'4 = Green/Red/Green
'5 = Red and Green
'6 = Red Green Purple

GIColorMod = 1


'''Extra GI - (adds GI lights upnder top left Plastic)

ExtraGI = 0


'''ColorChanging GI (only for two post top left of Table)

CCGI = 1


'''ColorChanging Score Display O_O

CCSD = 0


'''Lower Lighting
'0 = Normal
'1 = Green

LowerLighting = 0


'''BlackLight Plastics
'0 = Random
'1 = Normal
'2 = Red
'3 = Green
'4 = Red/Green

BLPlastics = 1


'''Blacklight Targets
'0=off
'1=On

BlacklightTargets = 1


''''BumperCap Lights
'0=Random
'1=Normal
'2=Red
'3=Green

BumperCapLights = 1


'''Extra Flasher
	'Brightens table

ExtraFlashers = 0


'''Post Lights

PostLightsOnOrOff = 0


'''Under Apron Lights
'0 = Off
'1 = Green
'2 = Red

UnderApronLights = 0


'''Instruction Cards
'0 = Random
'1 = 3ball
'2 = 5ball
'3 = Custom 1
'4 = Custom 2

Instruction_Cards = 1


''Game Info Card (from Las Vegas Pinball Museum)
'0 = Hide
'1 = Show

GameInfoCard = 0

'''Window Color
'0 = Clear
'1 = Green

WindowColor = 0


'''Window Fome
'1=Normal
'2=Green
'3=Red

WindowFome = 1


' DMD rotation
Const cDMDRotation 			= 0					' 0 or 1 for a DMD rotation of 90�

'*****************************
'Rom Version Selector
'*****************************
'1 "hh"   ' 6-digit
'2 "hh7"   ' 7-digit
RomSet = 1

' ***********************************************************************************************
' OPTIONS END
' ***********************************************************************************************

'****************************************
'Check the selected ROM version
'****************************************
Dim cGameName

If RomSet = 1 then 
	cGameName="hh"
	If HH.ShowFSS = True then
		DisplayTimerFSS6.Enabled = True 
		DisplayTimer7.Enabled = False
		DisplayTimer6.Enabled = False 
	Else
		DisplayTimer6.Enabled = true 
	End If
End If
If RomSet = 2 then 
	cGameName="hh7"
	If HH.ShowFSS = True then
		DisplayTimerFSS7.Enabled = True
		DisplayTimer7.Enabled = False
		DisplayTimer6.Enabled = False 
	Else
		DisplayTimer7.Enabled = true
	End If
End If

' ===============================================================================================
' some general constants and variables
' ===============================================================================================
	
Const UseSolenoids 		= True
Const UseLamps 			= False
Const UseGI 			= False
Const UseSync 			= True
Const HandleMech 		= False
Const SSolenoidOn 		= "SolOn"
Const SSolenoidOff 		= "SolOff"
Const SCoin 			= "Coin"
'Const SKnocker 			= "Knocker"

Dim I, x, obj, bsTrough, bsSaucer, dtRBank


' ======================--=======================================================================
' load game controller
' ===============================================================================================

LoadVPM "01560000", "sys80.VBS", 3.37 

' ===============================================================================================
' solenoids
' ===============================================================================================

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


SolCallback(1) = "Kick46"
SolCallback(2) = "UpKick"
SolCallback(5) = "UpperDropsUp"
SolCallback(6) = "BS"
SolCallback(8) = "PlayKnocker"
SolCallback(9) = "KickBallToLane"
SolCallback(10) = "GameOver"
SolCallback(11) = "SolGi"

Dim GameInPlay:GameInPlay=0

Sub GameOver(enabled)
	If enabled Then
		GameInPlay = 1
	Else
		GameInPlay = 0
	End If
End Sub


' Lights working as solenoids

Set LampCallback = GetRef("UpdateMultipleLamps")

Dim Old12, Old13, Old15, Old16,Old17
Old12=0:Old13=0:Old15=0:Old16=0:Old17=0

Sub UpdateMultipleLamps
If Controller.Lamp(12) <> Old12 Then
	If Controller.Lamp(12) Then
      BasementUpKick
    End If
    Old12 = Controller.Lamp(12)
End If

If Controller.Lamp(13) <> Old13 Then
	If Controller.Lamp(13) Then
		LowerDropsUp
    End If
    Old13 = Controller.Lamp(13)
End If

If Controller.Lamp(15) <> Old15 Then
	If Controller.Lamp(15) Then
    Kick65
    End If
    Old15 = Controller.Lamp(15)
End If

If Controller.Lamp(16) <> Old16 Then
	If Controller.Lamp(16) Then
      TrapDoorR.Collidable=0
	  TrapDoor.Enabled = 1
	  TrapDoorDir = 2
	  Playsoundat SoundFX("SOL_on",DOFContactors),pTrapDoor
	  TrapDoor.TimerEnabled = 1
    Else
	  TrapDoor.TimerEnabled = 1
	  TrapDoorDir = -2
	  Playsoundat SoundFX("SOL_off",DOFContactors),pTrapDoor
	  TrapDoor.Enabled = 0
      TrapDoorR.Collidable=1
    End If
    Old16 = Controller.Lamp(16)
End If

If Controller.Lamp(17) <> Old17 Then
	If Controller.Lamp(17) Then
		SolGIOn
	Else
		SolGiOff
	End If
    Old17 = Controller.Lamp(17)
End If
End Sub


Dim xxPostLights, xxLowerRubbers

Sub SolGIOff 'lower playfield off, upper pf on

    SetLamp 100,1


'''Upper
	gi1a.state = 1
	gi1b.state = 1
	gi1c.state = 1
	gi2a.state = 1
	gi2b.state = 1
	gi2c.state = 1
	gi3a.state = 1
	gi3b.state = 1
	gi3c.state = 1
	gi4a.state = 1
	gi4b.state = 1
	gi4c.state = 1
	gi5a.state = 1
	gi5a1.state = 1
	gi5a2.state = 1
	gi5a3.state = 1
	gi5b.state = 1
	gi5c.state = 1
	gi6a.state = 1
	gi6b.state = 1
	gi6c.state = 1
	gi7a.state = 1
	gi7a1.state = 1
	gi7a2.state = 1
	gi7a3.state = 1
	gi7a4.state = 1
	gi7b.state = 1
	gi7c.state = 1
	gi8a.state = 1
	gi8b.state = 1
	gi8c.state = 1
	gi9a.state = 1
	gi9b.state = 1
	gi9c.state = 1

	gi10a.state = 1
	gi11a.state = 1
	gi10b.state = 1
	gi11b.state = 1
	gi10c.state = 1
	gi11c.state = 1

	If ExtraGI = 1 Then
		giE1a.state = 1
		'giE1b.state = 1
		giE1c.state = 1
		giE2a.state = 1
		'giE2b.state = 1
		giE2c.state = 1
	End If

'''Post Lights
If PostLightsOnOrOff = 1 then
	for each xxPostLights in PostLights
		xxPostLights.state = 1
		next
End If


''''Bumpers
	PBumpCap1.material = "Plastic with an image trans"
	bLight1.state = 1
	bLight1b.state = 1
	PBumpCap2.material = "Plastic with an image trans"
	bLight2.state = 1
	bLight2b.state = 1
	PBumpCap3.material = "Plastic with an image trans"
	bLight3.state = 1
	bLight3b.state = 1

	Flpvl18.visible = 1
	Flpvl19.visible = 1
	Flpvl20.visible = 1
	Flpvl32.visible = 1
	Flpvl33.visible = 1
	Flpvl35.visible = 1
	Flpvl36.visible = 1
	Flpvl37.visible = 1
	Flpvl38.visible = 1
	Flpvl39.visible = 1
	Flpvl51.visible = 1


'''Lower
'	pPlayfield_Lower.image = "hh_lowerplayfield_dim"
	SetFlash 111, 0
	LPFoverhead.State = 0
	pWeb2.image = "webdim_texture"
	giLL1.state = 0
	giLL2.state = 0
	giLL3.state = 0
	giLL4.state = 0
	giLL5.state = 0
	giLL6.state = 0
	fLPFDim.ImageA = "hh_lowerplayfield_dark"
	pPlasticsLowerA.Image = "Lower_Plastics_dark"


''Flippers

	If FlipperColorType = 1 Then
		PFlipperLR.Image = "Gottflip_redR_dim"
		PFlipperLL.Image = "Gottflip_redL_dim"
	End If

	If FlipperColorType = 2 Then
		PFlipperLR.Image = "Gottflip_greenR_dim"
		PFlipperLL.Image = "Gottflip_greenL_dim"
	End If

	If FlipperColorType = 3 Then
		PFlipperLR.Image = "Gottflip_greenR_dim"
		PFlipperLL.Image = "Gottflip_greenL_dim"
	End If

	If FlipperColorType = 4 Then
		PFlipperLR.Image = "Gottflip_redR_dim"
		PFlipperLL.Image = "Gottflip_redL_dim"
	End If

''Targets
	psw00.Material = "Plastic with an imageDim"
	psw10.Material = "Plastic with an imageDim"
	psw20.Material = "Plastic with an imageDim"
	psw30.Material = "Plastic with an imageDim"
	psw40.Material = "Plastic with an imageDim"

''Rubbers

	for each xxLowerRubbers in LowerRubbers
		xxLowerRubbers.Material = "Plastic White TransDim"
		next

''Targets
	sw50.Material = "Plastic with an image transDim"
	sw60.Material = "Plastic with an image transDim"

''''Bumpers
	PBumpCap4.material = "Plastic with an image"
	bLight4.state = 0

End Sub

Sub SolGiOn 'lower playfield on, upper pf off

    SetLamp 100,0

'''Upper
	gi1a.state = 0
	gi1b.state = 0
	gi1c.state = 0
	gi2a.state = 0
	gi2b.state = 0
	gi2c.state = 0
	gi3a.state = 0
	gi3b.state = 0
	gi3c.state = 0
	gi4a.state = 0
	gi4b.state = 0
	gi4c.state = 0
	gi5a.state = 0
	gi5a1.state = 0
	gi5a2.state = 0
	gi5a3.state = 0
	gi5b.state = 0
	gi5c.state = 0
	gi6a.state = 0
	gi6b.state = 0
	gi6c.state = 0
	gi7a.state = 0
	gi7a1.state = 0
	gi7a2.state = 0
	gi7a3.state = 0
	gi7a4.state = 0
	gi7b.state = 0
	gi7c.state = 0
	gi8a.state = 0
	gi8b.state = 0
	gi8c.state = 0
	gi9a.state = 0
	gi9b.state = 0
	gi9c.state = 0

	gi10a.state = 0
	gi11a.state = 0
	gi10b.state = 0
	gi11b.state = 0
	gi10c.state = 0
	gi11c.state = 0

	If ExtraGI = 1 Then
		giE1a.state = 0
		'giE1b.state = 0
		giE1c.state = 0
		giE2a.state = 0
		'giE2b.state = 0
		giE2c.state = 0
	End If

''''Bumpers
	PBumpCap1.material = "Plastic with an image"
	bLight1.state = 0
	bLight1b.state = 0
	PBumpCap2.material = "Plastic with an image"
	bLight2.state = 0
	bLight2b.state = 0
	PBumpCap3.material = "Plastic with an image"
	bLight3.state = 0
	bLight3b.state = 0

	If PostLightsOnOrOff = 1 then
		for each xxPostLights in PostLights
			xxPostLights.state = 0
			next
	End If

	Flpvl18.visible = 0
	Flpvl19.visible = 0
	Flpvl20.visible = 0
	Flpvl32.visible = 0
	Flpvl33.visible = 0
	Flpvl35.visible = 0
	Flpvl36.visible = 0
	Flpvl37.visible = 0
	Flpvl38.visible = 0
	Flpvl39.visible = 0
	Flpvl51.visible = 0

'''Lower
'	pPlayfield_Lower.image = "hh_lowerplayfield_lit"

	fLPFDim.ImageA = "hh_lowerplayfield_Dim"

	SetFlash 111, 1
	LPFoverhead.State = 1
	pWeb2.image = "web_texture"

	pPlasticsLowerA.Image = "Lower_Plastics_dim"


''Flippers
	If FlipperColorType = 1 Then
		PFlipperLR.Image = "flipper_red_right"
		PFlipperLL.Image = "flipper_red_left"
	End If

	If FlipperColorType = 2 Then
		PFlipperLR.Image = "flipper_Green_right"
		PFlipperLL.Image = "flipper_Green_left"
	End If

	If FlipperColorType = 3 Then
		PFlipperLR.Image = "flipper_Green_right"
		PFlipperLL.Image = "flipper_Green_left"
	End If

	If FlipperColorType = 4 Then
		PFlipperLR.Image = "flipper_red_right"
		PFlipperLL.Image = "flipper_red_left"
	End If

''Targets
	psw00.Material = "Plastic with an image"
	psw10.Material = "Plastic with an image"
	psw20.Material = "Plastic with an image"
	psw30.Material = "Plastic with an image"
	psw40.Material = "Plastic with an image"

''Rubbers

	for each xxLowerRubbers in LowerRubbers
		xxLowerRubbers.Material = "Plastic White Trans"
		next

''Targets

	sw50.Material = "Plastic with an image trans"
	sw60.Material = "Plastic with an image trans"


''''Bumpers
	PBumpCap4.material = "Plastic with an image trans"
	bLight4.state = 1

End Sub

Sub SolGIAllOff
Dim i

End Sub

'Dim f,g,h,i,j
Dim Ball(6)
Dim InitTime
Dim TroughTime
Dim EjectTime
Dim MaxBalls
Dim TroughCount
Dim TroughBall(7)
Dim TroughEject
Dim Momentum
Dim UpperGIon
Dim Multiball
Dim BallsInPlay
Dim	iBall
Dim	fgBall

Dim bsLEjet, bsUpperEject, Lnell, mNell, plungerIM, dtDrop, udtDrop, ldtDrop



 Sub InitVPM()
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Haunted House" & vbNewLine & "Gottlieb 1982" & vbNewLine & "VPX cyberpez"
		'.Settings.Value("dmd_red") = 0
		'.Settings.Value("dmd_green") = 194
		'.Settings.Value("dmd_blue") = 0
		'.Games(cGameName).Settings.Value("rol")=0
		.Games(cGameName).Settings.value("sound") = 1 ' - Test table sounds...  disables ROM sounds
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 1
		On Error Resume Next
		Controller.SolMask(0) = 0
		vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - Then add the timer to renable all the solenoids after 2 seconds
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
End Sub



Sub HH_Init
	' table initialization
	InitVPM


		
	' basic pinmame timer
	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

	' nudging
	vpmNudge.TiltSwitch		= 57
	vpmNudge.Sensitivity	= 3
	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,Bumper4,UpperSlingShot,LowerSlingShot)
 
     ' Upper Drop Targets
     Set udtDrop = new cvpmDropTarget
     With udtDrop
	      .Initdrop Array(sw02,sw12,sw22,sw32), Array(2,12,22,32)
'	      .InitSnd "fx_resetdrop","fx_droptarget"
      End With 

     ' Lower Drop Targets
     Set ldtDrop = new cvpmDropTarget
     With ldtDrop
	      .Initdrop Array(sw00,sw10,sw20,sw30,sw40), Array(0,10,20,30,40)
'	      .InitSnd "fx_resetdrop","fx_droptarget"
      End With

    Const IMPowerSetting = 25 'Right Side Help Kicker Power
    Const IMTime = 0.3        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
        plungerIM.InitImpulseP sw65, IMPowerSetting, IMTime
        plungerIM.Random 0.3
        plungerIM.switch 65
'        plungerIM.InitExitSnd SoundFX("plunger2",DOFContactors), SoundFX("plunger",DOFContactors)
        plungerIM.CreateEvents "plungerIM"
 
	Backdrop_Init

	CheckOptions

	StartLampTimer

' ball through system
	MaxBalls=1	
	InitTime=61
	EjectTime=0
	TroughEject=1
	TroughCount=0
	iBall = 1
	fgBall = false


	CheckMaxBalls
    CreatBalls

	setup_backglass()

End Sub

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

sub setup_backglass()
Dim ix, xobj
xoff =550
yoff =0
zoff =1200
xrot = -90

if HH.ShowFSS = True then
'if 1 then

hhbgdark.visible =1
hhbghigh.visible =1
hhbghigh1.visible =1
hhbgFrame.visible =1

hhbgdark.x = xoff
hhbgdark.y = yoff
hhbgdark.height = zoff
hhbgdark.rotx = xrot

hhbgHigh.x = xoff
hhbgHigh.y = yoff
hhbgHigh.height = zoff
hhbgHigh.rotx = xrot
'hhbghigh.visible =0

hhbgHigh1.x = xoff
hhbgHigh1.y = yoff
hhbgHigh1.height = zoff
hhbgHigh1.rotx = xrot
'hhbghigh1.visible =0

'bgGrill.x = xoff
'bgGrill.y = yoff 
'bgGrill.height = zoff
'bgGrill.rotx = xrot

hhbgFrame.x = xoff
hhbgFrame.y = yoff 
hhbgFrame.height = zoff
hhbgFrame.rotx = xrot

FlasherMtlFSS.visible = 1
FlasherMtlFS.visible = 0

center_graphix()

	If RomSet = 1 then 
	center_digits()
	Else 
	center_digits7()
	end If

setup_sequencer()

for ix =0 to 31
	For Each xobj In LED7(ix)
	xobj.visible = 0
	next
next

Else
hhbgdark.visible =0
hhbghigh.visible =0
hhbghigh1.visible =0
hhbgFrame.visible =0

FlasherMtlFSS.visible = 0
FlasherMtlFS.visible = 1

end if


end sub


' ***************************************************************************
'                          DAY & NIGHT FUNCTIONS AND DATASETS
' ****************************************************************************

Dim nxx, DNS
DNS = HH.NightDay
'Dim OPSValues: OPSValues=Array (100,50,20,10 ,5,4 ,3 ,2 ,1, 0,0)
'Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00000)
Dim OPSValues: OPSValues=Array (100,50,20,10 ,9,8 ,7 ,6 ,5, 4,3,2,1,0)
Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00001, 0.000005, 0.000001)
Dim SysDNSVal: SysDNSVal=Array (1.0,0.9,0.8,0.7,0.6,0.5,0.5,0.5,0.5, 0.5,0.5)
Dim DivValues: DivValues =Array (1,2,4,8,16,32,32,32,32, 32,32)
Dim DivValues2: DivValues2 =Array (1,1.5,2,2.5,3,3.5,4,4.5,5, 5.5,6)
Dim DivValues3: DivValues3 =Array (1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1)
Dim RValUP: RValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255,255,255,255,255)
Dim GValUP: GValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255,255,255,255,255)
Dim BValUP: BValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255,255,255,255,255)
Dim RValDN: RValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim GValDN: GValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim BValDN: BValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim FValUP: FValUP=Array (35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130)
Dim FValDN: FValDN=Array (100,85,80,75,70,65,60,55,50,45,40,35,30)
Dim MVSAdd: MVSAdd=Array (0.9,0.9,0.8,0.8,0.7,0.7,0.6,0.6,0.5,0.5,0.4,0.3,0.2,0.1,0.05,0.001,0.005,0.0001)
Dim ReflDN: ReflDN=Array (60,55,50,45,40,35,30,28,26,24,22,20,19,18,16,15,14,13,12,11,10)
Dim DarkUP: DarkUP=Array (1,2,3,4,5,6,6,6,6,6,6,6,6,6,6,6,6,6)

Dim DivLevel: DivLevel = 35
Dim DNSVal: DNSVal = Round(DNS/10)
Dim DNShift: DNShift = 1

' PLAYFIELD GENERAL OPERATIONAL and LOCALALIZED GI ILLUMINATION
Dim aAllFlashers: aAllFlashers=Array(Flasher1,Flasher2,Flasher3,Flasher4,Flasher5,Flasher6,Flasher7,Flasher8,Flasher9,_
Flasher10,Flasher11,Flasher16,Flasher19,Flasher20,Flasher21,_
FlLi4a,FlLi4b,FlLi12b,FlLi12a, FlLi12a1,FlLi12a2,FlLi12a3,FlLi12a4,FlLi12a5,FlLi12a6,FlLi12a7,FlLi12a8,FlLi12a9,FlLi12a10,FlLi12a11,_
FlLi12a12,FlLi12a13,FlLi12a14,FlLi12a15,FlLi12a16,FlLi12a17,FlLi12a18,FlLi12a19,FlLi12b1,FlLi12b2,FlLi25,FlLi12a20,_
FlHHLB10,FlHHLB11,FlHHLB12,FlHHLB20,FlHHLB21,FlHHLB22,FlHHLB30,FlHHLB31,FlHHLB3, FlHHLB13,FlHHLB14, FlHHLB23,FlHHLB24,FlHHLB33,_
hhbgwallleft, hhbgwallright)
'Dim aGiLights: aGiLights=array(gi1,gi2,gi3,gi4,gi5,gi6,gi7,gi8,gi9,giLL1,giLL2,giLL3,giLL4,giLL5,giLL6,l21)
Dim BloomLights: BloomLights=array(L21b,Light1,Light2,Light3,Light4,Light5,_
bLight1,bLight1b,bLight2,bLight2b,bLight3,bLight3b,bLight4)
Dim TargetDropGi: TargetDropGi = array()

' PLAYFIELD GLOBAL INTENSITY ILLUMINATION FLASHERS
Flasher12.opacity = OPSValues(DNSVal + DNShift) / DivValues(DNSVal)
Flasher12.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher13.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher13.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher14.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher14.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
'Flasher4.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
'Flasher4.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)


'BACKBOX & BACKGLASS ILLUMINATION
hhBGDark.ModulateVsAdd = MVSAdd(DNSVal)
hhBGDark.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
hhBGDark.Amount = FValUP(DNSVal) / DivValues(DNSVal)
hhBGHigh.Color = RGB(RValDN(DNSVal),GValDN(DNSVal),BValDN(DNSVal))
hhBGHigh.Amount = FValDN(DNSVal)  / DivValues(DNSVal)
hhBGHigh1.Color = RGB(RValDN(DNSVal),GValDN(DNSVal),BValDN(DNSVal))
hhBGHigh1.Amount = FValDN(DNSVal) / DivValues(DNSVal)

hhBGframe.ModulateVsAdd = MVSAdd(DNSVal+4) * 0.2
hhBGframe.Color = RGB(RValUP(DNSVal+4),GValUP(DNSVal+4),BValUP(DNSVal+4))
hhBGframe.Amount = FValUP(DNSVal+4) * DivValues(DNSVal)

Flasher23.Amount = FValUP(DNSVal) / DivValues(DNSVal)
Flasher23.Color = RGB(RValUP(DNSVal)-25,GValUP(DNSVal)-25,BValUP(DNSVal)-25)
Flasher24.Amount = FValUP(DNSVal) / DivValues(DNSVal)
Flasher24.Color = RGB(RValUP(DNSVal)-25,GValUP(DNSVal)-25,BValUP(DNSVal)-25)
Flasher25.Amount = FValUP(DNSVal) / DivValues(DNSVal)
Flasher25.Color = RGB(RValUP(DNSVal)-25,GValUP(DNSVal)-25,BValUP(DNSVal)-25)

Flasher17.Amount = FValUP(DNSVal+4) / DivValues(DNSVal)
Flasher17.Color = RGB(RValUP(DNSVal+4),GValUP(DNSVal+4),BValUP(DNSVal+4))
Flasher18.Amount = FValUP(DNSVal+4) / DivValues(DNSVal)
Flasher18.Color = RGB(RValUP(DNSVal+4),GValUP(DNSVal+4),BValUP(DNSVal+4))


hhBGHigh.intensityscale = 1.0
hhBGHigh1.intensityscale = 1.0
hhBGDark.intensityscale = 1.0
hhBGframe.intensityscale = 1.0

HH.PlayfieldReflectionStrength = ReflDN(DNSVal + 4)

For each nxx in aGiLights:nxx.intensity = nxx.intensity * SysDNSVal(DNSVal) /DivValues3(DNSVal) :Next
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues3(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues(DNSVal) / DivLevel:Next
For each nxx in BloomLights:nxx.intensity = nxx.intensity *SysDNSVal(DNSVal)/DivValues3(DNSVal):Next
'For each nxx in TargetDropGi:nxx.intensity = nxx.intensity *SysDNSVal(DNSVal)/DivValues3(DNSVal):Next

Sub SetSMLiDNS(object, enabled)
	If enabled > 0 then
	object.intensity = enabled * SysDNSVal(DNSVal) /DivValues2(DNSVal)
	Else
	object.intensity =0
	end if	
End Sub

Sub SetSMFlDNS(object, enabled)

	If enabled > 0 then
	object.opacity = enabled / DivValues2(DNSVal) 
	else 
	object.opacity = 0
	end if
End Sub


Sub SetSLiDNS(object, enabled)
	If enabled then
	object.intensity = 1 * SysDNSVal(DNSVal) /DivValues2(DNSVal)
	Else
	object.intensity =0
	end if	
End Sub

Sub SetSFlDNS(object, enabled)

	If enabled then
	object.opacity = 1 * OPSValues(DNSVal) / DivLevel
	else 
	object.opacity = 0
	end if
End Sub


Sub SetDNSFlash(nr, object)
    Select Case LightState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                LightState(nr) = -1 'completely off, so stop the fading loop
            End if
            Object.IntensityScale = FlashLevel(nr) * SysDNSVal(DNSVal) /DivValues2(DNSVal)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                LightState(nr) = -1 'completely on, so stop the fading loop
            End if
            Object.IntensityScale = FlashLevel(nr) * SysDNSVal(DNSVal) /DivValues2(DNSVal)
    End Select
End Sub


Sub SetDNSFlashm(nr, object) 'multiple flashers, it just sets the intensity
    Object.IntensityScale = FlashLevel(nr) * SysDNSVal(DNSVal) /DivValues2(DNSVal)
End Sub

Sub SetDNSFlashex(object, value) 'it just sets the intensityscale for non system lights
    Object.IntensityScale = value * SysDNSVal(DNSVal) /DivValues2(DNSVal)
End Sub









Dim BGArr 
BGArr=Array (FlLi4a,FlLi4b,FlLi12b,FlLi12a, FlLi12a1,FlLi12a2,FlLi12a3,FlLi12a4,FlLi12a5,FlLi12a6,FlLi12a7,FlLi12a8,FlLi12a9,_
FlLi12a10,FlLi12a11,FlLi12a12,FlLi12a13,FlLi12a14,FlLi12a15,FlLi12a16,FlLi12a17,FlLi12a18,FlLi12a19,FlLi12b1,FlLi12b2,FlLi25,_
FlLi12a20, FlHHLB10,FlHHLB11,FlHHLB12,FlHHLB20,FlHHLB21,FlHHLB22,FlHHLB30,FlHHLB31,FlHHLB3, FlHHLB13,FlHHLB14, FlHHLB23,FlHHLB24,FlHHLB33,_
hhbgwallleft, hhbgwallright)

Sub center_graphix()
Dim xx,yy,yfact,xfact,xobj
zscale = 0.0000001

xcen =(1211 /2) - (108 / 2)
ycen = (1120 /2 ) + (128 /2)


yfact =0 'y fudge factor (ycen was wrong so fix)
xfact =0

	For Each xobj In BGArr
	xx =xobj.x 
		
	xobj.x = (xoff -xcen) + xx +xfact
	yy = xobj.y ' get the yoffset before it is changed
	xobj.y =yoff 

		If(yy < 0.) then
		yy = yy * -1
		end if

	
	xobj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
	
	xobj.rotx = xrot
	xobj.visible =0 ' for testing
	Next
end sub

' ***************************************************************************
'BASIC FSS(SS TYPE1) 1-4 player,credit,bip,+extra 7 segment SETUP CODE
' ****************************************************************************

Sub center_digits()
Dim ix, xx, yy, yfact, xfact, xobj

zscale = 0.0000001

xcen =(1211 /2) - (108 / 2)
ycen = (1120 /2 ) + (128 /2)
yfact =0 'y fudge factor (ycen was wrong so fix)
xfact =0


for ix =0 to 27
	For Each xobj In Digits(ix)

	'if obj NOT n then

	xx =xobj.x 
		
	xobj.x = (xoff -xcen) + xx +xfact
	yy = xobj.y ' get the yoffset before it is changed
	xobj.y =yoff 

		If(yy < 0.) then
		yy = yy * -1
		end if

	xobj.height =( zoff - ycen) + yy - (yy * (zscale)) + yfact
	
	xobj.rotx = xrot

	xobj.visible = 0
	'end if
	Next
	Next
end sub

Sub center_digits7()
Dim ix, xx, yy, yfact, xfact, xobj

zscale = 0.0000001

xcen =(1211 /2) - (108 / 2)
ycen = (1120 /2 ) + (128 /2)
yfact =0 'y fudge factor (ycen was wrong so fix)
xfact =0


for ix =0 to 31
	For Each xobj In Digits7(ix)

	'if obj NOT n then

	xx =xobj.x 
		
	xobj.x = (xoff -xcen) + xx +xfact
	yy = xobj.y ' get the yoffset before it is changed
	xobj.y =yoff 

		If(yy < 0.) then
		yy = yy * -1
		end if

	xobj.height =( zoff - ycen) + yy - (yy * (zscale)) + yfact
	
	xobj.rotx = xrot

	xobj.visible = 0
	'end if
	Next
	Next
end sub


Sub HH_Exit()
	Controller.Stop
End Sub

Sub HH_Paused
	Controller.Pause = True
End Sub
Sub HH_UnPaused
	Controller.Pause = False
End Sub

' ****************************************************************************
' FLASHER ANIMATION SEQUENCER FOR ROMLESS TABLES, OR NON-ROM CONTROLLED LIGHTS 
' ****************************************************************************

' ****************************************************************************
' ****** THIS SEQUENCER CAN CONTROL 20(banks)x10(flashers) = 200 lights ******
' ****************************************************************************

' anim array 0
Dim BGAni(14)
BGAni(0) = Array(FlHHLB10,hhbgwallright,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(1) = Array(FlHHLB11,hhbgwallright,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(2) = Array(FlHHLB12,hhbgwallright,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 

BGAni(3) = Array(FlHHLB20,hhbgwallleft,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(4) = Array(FlHHLB21,hhbgwallleft,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(5) = Array(FlHHLB22,hhbgwallleft,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(6) = Array(FlHHLB30,hhbgwallleft,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(7) = Array(FlHHLB31,hhbgwallleft,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 

BGAni(8) = Array(FlHHLB3,FlHHLB33,hhbgwallright,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(9) = Array(FlLi25,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 
BGAni(10) = Array(FlHHLB13,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)
BGAni(11) = Array(FlHHLB14,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)
BGAni(12) = Array(FlHHLB23,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)
BGAni(13) = Array(FlHHLB24,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)



' anim/gfx array 1
Dim BGFx1(1)
BGFx1(0) = Array(Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty) 

' anim/gfx array 2
Dim BGFx2(1)
BGFx2(0) = Array(Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty)



' FlHHLB10,FlHHLB11,FlHHLB12,FlHHLB20,FlHHLB21,FlHHLB22,FlHHLB30,FlHHLB31,FlHHLB3
' setup animation sequencer slave/master

Sub setup_sequencer()

Dim cnt: cnt =0
aniplay =0 ' global anim control default OFF

	tdelay = 0 ' delay time in m/s
	tstep =0  ' current anim iteration
	tplay =0 ' current playing anim stack
	tanims =0
	tanim =0
	tdone =0
	tskip = 0
	
	' **********  FILL WITH ANIMATION BANKS 0-9 ***********
		' *********** THIS IS AN EXAMPLE ONLY *************
		
	tanims =12 ' tatal anim elements in stack
	BGAnimList(0) = BGAni(0) ' anim 1 sequence
	BGAnimList(1) = BGAni(1) ' anim 2 sequence
	BGAnimList(2) = BGAni(2) ' anim 3 sequence
	BGAnimList(3) = BGAni(3)
	BGAnimList(4) = BGAni(4)
	BGAnimList(5) = BGAni(5)
	BGAnimList(6) = BGAni(6)
	BGAnimList(7) = BGAni(7)
	BGAnimList(8) = BGAni(8)
	BGAnimList(9) = BGAni(10)
	BGAnimList(10) = BGAni(11)
	BGAnimList(11) = BGAni(12)
	BGAnimList(12) = BGAni(13)
	
	' ********  FILL WITH ANIMATION STEPS 0-99 ********
	' *********** THIS IS AN EXAMPLE ONLY *************
	
	'  setup push / play anim list
	tqueue = 64 ' total anim events in queue
	'Animplay(0) = Array(0,0) ' list 0, rand
	'Animplay(1) = Array(0,1) ' list 0, flash on/off
	'Animplay(2) = Array(0,2) ' list 0, all on
	'Animplay(3) = Array(0,3)  'list 0, all off
	'Animplay(4) = Array(0,4)  'list 0, pulse down
	'Animplay(5) = Array(0,5)  'list 0, pulse up
	'Animplay(6) = Array(0,-1) ' delay 350ms

	' SKY ON

	Animplay(cnt) = Array(9,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(10,2,10,1) : cnt = cnt + 1
'Lightning part 1-A top right first branch
	Animplay(cnt) = Array(0,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(1,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(0,3,30,150): cnt = cnt + 1
'Lightning part 1-B top right second branch
	Animplay(cnt) = Array(0,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(2,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(0,3,30,150) : cnt = cnt + 1
	' SKY OFF
	Animplay(cnt) = Array(9,3,30,150) : cnt = cnt + 1
	Animplay(cnt) = Array(10,3,30,150) : cnt = cnt + 1

	'10
	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1 ' delay 350ms
	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1 ' delay 350ms

	' SKY ON
	Animplay(cnt) = Array(11,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(12,2,10,1) : cnt = cnt + 1
'Lightning part 2-A top left first branch
	Animplay(cnt) = Array(3,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(4,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(3,3,30,150) : cnt = cnt + 1
'Lightning part 2-B top left second brach
	Animplay(cnt) = Array(3,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(5,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(3,3,30,150) : cnt = cnt + 1
	'20
	' SKY OFF
	Animplay(cnt) = Array(11,3,30,150) : cnt = cnt + 1
	Animplay(cnt) = Array(12,3,30,150) : cnt = cnt + 1

	Animplay(cnt) = Array(0,-1,350,350)  : cnt = cnt + 1' delay 350ms


' SKY ON
	Animplay(cnt) = Array(11,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(12,2,10,1) : cnt = cnt + 1
'Lightning part 3-A mid left first brach
	Animplay(cnt) = Array(6,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(7,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(6,3,30,150) : cnt = cnt + 1
'Lightning part 3-B mid left second branch
	Animplay(cnt) = Array(6,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(7,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(6,3,30,150): cnt = cnt + 1
	' SKY OFF
	Animplay(cnt) = Array(11,3,30,150) : cnt = cnt + 1
	Animplay(cnt) = Array(12,3,30,150) : cnt = cnt + 1

	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1' delay 350ms
	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1' delay 350ms
	'35
'************************************

' SKY ON

	Animplay(cnt) = Array(9,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(10,2,10,1) : cnt = cnt + 1
'Lightning part 1-A  top right first branch
	Animplay(cnt) = Array(0,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(1,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(0,3,30,150): cnt = cnt + 1
' SKY OFF
	Animplay(cnt) = Array(9,3,30,150) : cnt = cnt + 1
	Animplay(cnt) = Array(10,3,30,150) : cnt = cnt + 1

' SKY ON
	Animplay(cnt) = Array(11,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(12,2,10,1) : cnt = cnt + 1
'Lightning part 2-A top left first branch
	Animplay(cnt) = Array(3,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(4,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(3,3,30,150): cnt = cnt + 1
' SKY OFF
	Animplay(cnt) = Array(11,3,30,150) : cnt = cnt + 1
	Animplay(cnt) = Array(12,3,30,150) : cnt = cnt + 1
' SKY ON
	Animplay(cnt) = Array(11,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(12,2,10,1) : cnt = cnt + 1
'Lightning part 3-A  mid left first branch
	Animplay(cnt) = Array(6,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(7,1,250,20) : cnt = cnt + 1
	Animplay(cnt) = Array(6,3,30,150): cnt = cnt + 1
' SKY OFF
	Animplay(cnt) = Array(11,3,30,150) : cnt = cnt + 1
	Animplay(cnt) = Array(12,3,30,150) : cnt = cnt + 1
'56
Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1' delay 350ms

'Lightning part 4-A bottom right
	Animplay(cnt) = Array(8,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(8,3,30,150) : cnt = cnt + 1

	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1' delay 350ms

'Lightning part 4-B bottom right
	Animplay(cnt) = Array(8,2,10,1) : cnt = cnt + 1
	Animplay(cnt) = Array(8,3,30,150) : cnt = cnt + 1


	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1' delay 350ms
	Animplay(cnt) = Array(0,-1,350,350) : cnt = cnt + 1' delay 350ms

	'64
	' **************  ENABLE ANIMATION ****************
	

	'start_sequencer()
	'stop_sequencer()
end Sub

Sub start_sequencer()

hide_elem(BGArr)

if HH.ShowFSS = true then

aniplay =1 ' enable anim
	AnimPlayerTimer.enabled = 0 ' slave
	AnimPlayerTimer.interval = 1
	AnimSequencerTimer.enabled = 1 ' master 
	AnimSequencerTimer.interval = 1
end if 
end Sub

Sub stop_sequencer()
if HH.ShowFSS = true then
aniplay =0
AnimSequencerTimer.enabled = 0
AnimPlayerTimer.enabled = 0

tdelay = 0 ' delay time in m/s
tstep =0  ' current anim iteration
tplay =0 ' current playing anim stack
tanims =0
tanim =0
tdone =0
tskip = 0


end if
hide_elem(BGArr)

end sub


Sub hide_elem(elem)
Dim objx, nx
nx =0

	For Each objx In elem ' hide all elements
		if Not IsEmpty(elem(nx)) then
		objx.visible =0
		end if
	nx = nx+1
	Next
end sub


Sub show_elem(elem)
Dim objx, nx
nx =0

	For Each objx In elem ' hide all elements
		if Not IsEmpty(elem(nx)) then
		objx.visible =1
		end if
	nx = nx+1
	Next
end sub


' ****************************************************************************
' ANIMATION SEQUENCER MASTER TIMER - TIME SLICING & TRIGGERS FX
' ****************************************************************************

' animation sequencer for (dead tables/backglass) without rom control 

Dim BGAnimList(20) ' animation stack array
Dim Animwait(20) ' animation on/off wait times
Dim Animlist(20) ' animation push(on)/pop(off) elements
Dim Animplay(100) ' animation sequence to play Maximum 100
Dim pulsewait(10,10)
Dim pulselist(10,10)
' flasher animations routines

' ------------------------- private global values do not use! ------------------
Dim aniplay, tdelay, tlast, tstep, tanim, tanims, tplay, tqueue, tskip,tpulse,tpulses
Dim aniobj, aniwait, tstart, tfinish
tpulse =0
tpulses =0
' animation sequencer

Sub AnimSequencerTimer_Timer

	if aniplay = 1 then

	if AnimPlayerTimer.enabled = 0 then ' if current animation has ended

	If tdelay = 0 then  ' has delay expired

	aniobj= Animplay(tplay)
	tstep =aniobj(1)
	tstart =aniobj(2)
	tfinish =aniobj(3)
	Select case tstep

		case -1 ' delay
		Animwait(0) = Array(0,0,tstart) '350) ' wait/wait/delay
		Animwait(1) = Array(0,0,tfinish)'350) ' wait/wait/delay
		tanims = 2
		tanim = 0
		tdone = 0
		'init delay
		aniwait =Animwait(tanim)
		tdelay = aniwait(2) ' get delay
		 tskip = 1

		Case 0  ' flash random flashers on/off 250ms step delay 
		
		Animwait(0) = Array(1,0,tstart)'250) ' wait/wait/delay
		Animwait(1) = Array(2,2,tstart)'250)
		Animwait(2) = Array(1,0,tstart)'250)
		Animwait(3) = Array(0,1,tstart)'250)
		Animwait(4) = Array(1,1,tstart)'250)
		Animwait(5) = Array(1,0,tstart)'250)
		Animwait(6) = Array(0,1,tstart)'250)
		Animwait(7) = Array(1,1,tstart)'250)
		Animwait(8) = Array(1,1,tfinish)'250)
		Animwait(9) = Empty

		Animlist(0) = Array( 1, 6, 7, 8, 9,10,-1,-1,-1,-1,  8, 6, 7, 8, 9,10,-1,-1,-1,-1) ' 10 on / 10 off  / -1 skip
		Animlist(1) = Array( 3, 7,-1,-1,-1,-1,-1,-1,-1,-1,  1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(2) = Array(10,-1,-1,-1,-1,-1,-1,-1,-1,-1,  7,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(3) = Array( 6, 4,-1,-1,-1,-1,-1,-1,-1,-1,  3,10,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(4) = Array( 8, 2,-1,-1,-1,-1,-1,-1,-1,-1,  6,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(5) = Array( 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,  4, 2,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(6) = Array( 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,  8,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(7) = Array( 1, 5,-1,-1,-1,-1,-1,-1,-1,-1,  9, 3,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(8) = Array( 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 5,-1,-1,-1,-1,-1,-1,-1,-1)
		Animlist(9) = Empty
		'Animlist(0) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		'Animlist(1) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 2, 3, 4, 5, 6, 7, 8, 9,10)
		'Animlist(2) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		'Animlist(3) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 2, 3, 4, 5, 6, 7, 8, 9,10)
		'Animlist(4) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		'Animlist(5) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 2, 3, 4, 5, 6, 7, 8, 9,10)
		'Animlist(6) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		'Animlist(7) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 2, 3, 4, 5, 6, 7, 8, 9,10)
		'Animlist(8) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

		tanims = 9
		tanim = 0
		tdone = 0
		'init delay
		aniwait =Animwait(tanim)
		tdelay = aniwait(2) ' get delay
		tskip = 1
		'TextBox1.text = " "
		'TextBox2.text =" "

		' ************** (1) ******************
		Case 1 ' flash all on/off animation sequence 350ms step dalay (fast flash sequence)

		Animwait(0) = Array(1,0,tstart)'250)  ' wait/wait/delay
		Animwait(1) = Array(0,1,tfinish)'20)

		Animlist(0) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1) ' 10 on / 10 off  / -1 skip
		Animlist(1) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 2, 3, 4, 5, 6, 7, 8, 9,10)
		
		tanims = 2
		tanim = 0
		tdone = 0
		'init delay
		aniwait =Animwait(tanim)
		tdelay = aniwait(2) ' get delay
		 tskip = 1

		' ************** (2) ******************
		Case 2 ' illuminate all - delay 1 seconds
		Animwait(0) = Array(1,0,tstart)'10)  ' wait/wait/delay
		Animwait(1) = Array(0,1,tfinish)'1)
		Animlist(0) = Array( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1) ' 10 on / 10 off  / -1 skip
		Animlist(1) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1) 

		tanims = 2
		tanim = 0
		tdone = 0
		'init delay
		aniwait =Animwait(tanim)
		tdelay = aniwait(2) ' get delay
		 tskip = 1

		' ************** (3) ******************
		Case 3 ' extinguise all - delay 1 seconds
		Animwait(0) = Array(0,1,tstart)'30)  ' wait/wait/delay
		Animwait(1) = Array(0,1,tfinish)'150)
		Animlist(0) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,  1, 2, 3, 4, 5, 6, 7, 8, 9,10)
		Animlist(1) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1) 

		tanims = 2
		tanim = 0
		tdone = 0
		'init delay
		aniwait =Animwait(tanim)
		tdelay = aniwait(2) ' get delay
		 tskip = 1

		Case 4 ' pulse down

		Pulsewait(tpulses,0) = Array(1,0,tstart)'350)  ' wait/wait/delay
		Pulsewait(tpulses,1) = Array(1,0,tstart)'350)
		Pulsewait(tpulses,2) = Array(1,0,tstart)'350)  
		Pulsewait(tpulses,3) = Array(1,0,tstart)'350)
		Pulsewait(tpulses,4) = Array(1,0,tfinish)'350)  

		' five pulses down
		Pulselist(tpulses,0) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10) 
		Pulselist(tpulses,1) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10) 
		Pulselist(tpulses,2) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10) 
		Pulselist(tpulses,3) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10) 
		Pulselist(tpulses,4) = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10) 
		
		tpulse =1
		tpulses = tpulses + 1
		tanims = 0 ' don't play 'automated' process 
		tanim = 0
		tskip =0
		Case 5 ' pulse up

		Pulsewait(tpulses,0) = Array(0,1,tstart)'350)  ' wait/wait/delay
		Pulsewait(tpulses,1) = Array(0,1,tstart)'350)
		Pulsewait(tpulses,2) = Array(0,1,tstart)'350)  
		Pulsewait(tpulses,3) = Array(0,1,tstart)'350)
		Pulsewait(tpulses,4) = Array(0,1,tfinish)'350)  

		'five pulses up
		Pulselist(tpulses,0) = Array(1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Pulselist(tpulses,1) = Array(1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Pulselist(tpulses,2) = Array(1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Pulselist(tpulses,3) = Array(1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		Pulselist(tpulses,4) = Array(1, 2, 3, 4, 5, 6, 7, 8, 9,10, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
		
		tpulse =1
		tpulses = tpulses + 1
		tanims = 0 ' don't play 'automated' process 
		tanim = 0
		tskip =0
		Case 6 'glow increase
		MsgBox "glow inc. animation feature is not supported..."
		Case 7 'glow decrease
		MsgBox "glow dec. animation feature is not supported yet..."
	End Select

	tplay = tplay +1 'increment animation sequence
	
		If tplay = tqueue Then ' check the queue 
		tplay =0
		tpulse = 0
		tpulses = 0
		end If

	AnimPlayerTimer.enabled = 1 ' pulse anim player
	
	end if ' delay expired ?
	end if ' player done ?
	end if ' activated ?
End Sub


' ****************************************************************************
' ANIMATION PLAYER SLAVE TIMER - EXECUTES FX
' ****************************************************************************


' ------------------------- private global values do not use! ------------------
Dim await, alist, tdone, ncnt, ti, tp, np, nt, nobj, nobjs, nobjx, anio, dbg, nnn
dbg =0
nnn=0 
tp =0 
np =0 
' animation sequence player  
Sub AnimPlayerTimer_Timer


	If tdelay = 0 or tskip = 1 then  ' has delay expired
	tskip = 0


	If tanim < tanims Then

		If tdone = 0 then ' get the queue
		
		await = Animwait(tanim) 
		alist = Animlist(tanim)
		
			for ti =0 to 19

				If ti < 10 then ' ON state(s)
				nt = alist(ti)
					if nt <> -1 Then

					nobjs =BGAnimList(aniobj(0))
					nt = nt-1 ' one based - so decrement
						
						If Not IsEmpty(nobjs(nt)) then: nobjs(nt).visible =1

					end if
				Else 'OFF state(s)
				nt =alist(ti)
					if nt <> -1 Then

					nobjs =BGAnimList(aniobj(0))
					nt = nt-1
						If Not IsEmpty(nobjs(nt)) then: nobjs(nt).visible =0

					end if
				end if
			Next

		tanim = tanim + 1

			If tanim < tanims Then
			aniwait =Animwait(tanim)
			tdelay = aniwait(2) ' get the next delay
			else
			tdelay = 0
			end if

		'TextBox1.text = " "


		' do pulse animation every loop
			If tpulse = 1 and tpulses > 0  then
				if np < tpulses then
		
				anio =Animplay(np)
				nobjx =BGAnimList(anio(0)) 

					If tp = 0 then
						If np + 1 < tpulses Then
						np = np + 1 ' increment np
						Else
						np = 0 ' reset np	
						end If
					end if

					If Not IsEmpty(nobjx)  then
					
						if tp < 4 then ' fixed 5 sequences for pulsations

						alist = pulselist(np, tp)
						await = pulsewait(np, tp) 

							If tp + 1 < 4 Then
							tp = tp + 1 ' increment tp
							Else
							tp = 0 ' reset tp		
							end If

							for ti =0 to 19 ' 19 sequences
							
								If ti < 10 then 
								nt =alist(ti)
									if nt <> -1 Then	
									nt = nt-1
									
										
										If Not IsEmpty(nobjx(nt)) then
											if	(nobjx(nt).visible = true) then
											nobjx(nt).intensityscale = nobjx(nt).intensityscale + 0.2
											end if
										End if
									End if
								Else 'ti >= 10

								nt =alist(ti)
									if nt <> -1 Then	
									nt = nt-1

										If Not IsEmpty(nobjx(nt))  then
											If(nobjx(nt).visible = true) then
											nobjx(nt).intensityscale = nobjx(nt).intensityscale - 0.2
											End if
										end if

									End if

								end if
							Next ' 0- 19
						Else
						tp =0
						end if ' 0-4 pulsations
					End if 'nobjs <> Null
				Else
				np =0
				end if ' tpulses
			end if ' tpulse = 1
		end if
	'nnn = nnn +1
	Else
	tdone =0
	tanim = 0
	tanims =0
	tdelay = 0
	me.enabled = 0
	
	end if

	Else
		If((tdelay - 1) < 0)  then
		tdelay = 0
		else
		tdelay = tdelay - 1
		end if
	end If

End Sub

''''''''''''''''''''''''''''''''
''  Flippers
''''''''''''''''''''''''''''''''

Sub SolLFlipper(Enabled)
	If Enabled Then
		If Controller.Lamp(17) Then
			PlaySoundAt SoundFX("FlipperUpLeft",DOFFlippers),FlipperLL
			FlipperLL.RotateToEnd
		Else
		PlaySoundAt SoundFX("FlipperUpLeft",DOFFlippers),LeftFlipper
			LeftFlipper.RotateToEnd
			LeftFlipper2.RotateToEnd
			If FlipperKeyMod = 2 then
				FlipperUL.RotateToEnd:PlaySoundAt SoundFX("FlipperUpLeft",DOFFlippers),FlipperUL
			End If
		End If
	Else
		PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),LeftFlipper
		LeftFlipper.RotateToStart
		LeftFlipper2.RotateToStart
		PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),FlipperLL
		FlipperLL.RotateToStart
			If FlipperKeyMod = 2 then
				FlipperUL.RotateToStart:PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),FlipperUL
			End If
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		If Controller.Lamp(17) Then
			PlaySoundAt SoundFX("FlipperUpRight",DOFFlippers),FlipperLR
			FlipperLR.RotateToEnd
		Else
			PlaySoundAt SoundFX("FlipperUpRight",DOFFlippers),RightFlipper
			RightFlipper.RotateToEnd
			RightFlipper2.RotateToEnd
			If FlipperKeyMod = 2 then
				FlipperUR.RotateToEnd:PlaySoundAt SoundFX("FlipperUpRight",DOFFlippers),FlipperUR
			End If
		End If
	Else
		PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),RightFlipper
		RightFlipper.RotateToStart
		RightFlipper2.RotateToStart
		PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),FlipperLR
		FlipperLR.RotateToStart
			If FlipperKeyMod = 2 then
				FlipperUR.RotateToStart:PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),FlipperUR
			End If
	End If
End Sub


'''Keys

Sub HH_KeyDown(ByVal keycode)

If FlipperKeyMod = 1 then
	If keycode = LeftMagnaSave and GameInPlay then FlipperUL.RotateToEnd:PlaySoundAt SoundFX("FlipperUpLeft",DOFFlippers),FlipperUL
	If keycode = RightMagnaSave and GameInPlay then FlipperUR.RotateToEnd:PlaySoundAt SoundFX("FlipperUpRight",DOFFlippers),FlipperUR
End If
	'If keycode = LeftTiltKey Then LeftNudge 80, 1, 20
    'If keycode = RightTiltKey Then RightNudge 280, 1, 20
    'If keycode = CenterTiltKey Then CenterNudge 0, 1, 25
	If keycode = PlungerKey Then PlungerPull
'    If keycode = ToggleMechSoundsKey Then 
'		ToggleMechSounds = (ToggleMechSounds + 1) mod 2
'		'debug.print ToggleMechSounds
'	End if

    '************************   Start Ball Control 1/3
        if keycode = 46 then                ' C Key
            If contball = 1 Then
                contball = 0
            Else
                contball = 1
            End If
        End If
        if keycode = 48 then                'B Key
            If bcboost = 1 Then
                bcboost = bcboostmulti
            Else
                bcboost = 1
            End If
        End If
        if keycode = 203 then bcleft = 1        ' Left Arrow
        if keycode = 200 then bcup = 1          ' Up Arrow
        if keycode = 208 then bcdown = 1        ' Down Arrow
        if keycode = 205 then bcright = 1       ' Right Arrow
    '************************   End Ball Control 1/3
	
	If vpmKeyDown(keycode) Then Exit Sub


	If keycode = 21 then  ''''''''''''''''''''y Key used for testing
		Set cBall2 = testkick.Createball
		testkick.kick 180,5
	End If

	If keycode = 22 then  ''''''''''''''''''''u Key used for testing

	End If



End Sub

Sub PlungerPull
	Plunger.Pullback
	PlaySound "plungerpull",0,1,0.25,0.25
End Sub

Sub HH_KeyUp(ByVal keycode)
	If FlipperKeyMod = 1 Then
		If keycode = LeftMagnaSave and GameInPlay then FlipperUL.RotateToStart:PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),FlipperUL
		If keycode = RightMagnaSave and GameInPlay then FlipperUR.RotateToStart:PlaySoundAt SoundFX("Flipper_Down",DOFFlippers),FlipperUR
	End If
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then PlungerRelease

    '************************   Start Ball Control 2/3
    if keycode = 203 then bcleft = 0        ' Left Arrow
    if keycode = 200 then bcup = 0          ' Up Arrow
    if keycode = 208 then bcdown = 0        ' Down Arrow
    if keycode = 205 then bcright = 0       ' Right Arrow
    '************************   End Ball Control 2/3

End Sub

Sub PlungerRelease
	Plunger.Fire
	PlaySound "plunger",0,1,0.25,0.25
End Sub


'************************   Start Ball Control 3/3
Sub StartControl_Hit()
    Set ControlBall = ActiveBall
    contballinplay = true
End Sub
 
Sub StopControl_Hit()
    contballinplay = false
End Sub
 
Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1     'Do Not Change - default setting
bcvel = 4       'Controls the speed of the ball movement
bcyveloffset = -0.01    'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3    'Boost multiplier to ball veloctiy (toggled with the B key)
 
Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If bcright = 1 Then
            ControlBall.velx = bcvel*bcboost
        ElseIf bcleft = 1 Then
            ControlBall.velx = - bcvel*bcboost
        Else
            ControlBall.velx=0
        End If
 
        If bcup = 1 Then
            ControlBall.vely = -bcvel*bcboost
        ElseIf bcdown = 1 Then
            ControlBall.vely = bcvel*bcboost
        Else
            ControlBall.vely= bcyveloffset
        End If
    End If
End Sub
'************************   End Ball Control 3/3


'******************************
'  Setup Desktop
'******************************

Sub Backdrop_Init
	Dim bdl
	If DesktopMode = True then
		l24.Y = 1060
		l25.Y = 1060
		l26.Y = 1060
		l25b.Y = 1141.842
		l23.Y = 1184.212
		l26b.Y = 1303.198
		l45.Y = 1415.443
		For each bdl in BackdropLights: bdl.visible = true:Next
	Else
		l24.Y = 1066
		l25.Y = 1066
		l26.Y = 1066
		l25b.Y = 1153.842
		l23.Y = 1196.212
		l26b.Y = 1317.198
		l45.Y = 1428.776
		For each bdl in BackdropLights: bdl.visible = false:Next
	End If
End Sub


Dim xxGIColor, Red, RedFull, RedI, RedIa, Pink, PinkFull, PinkI, PinkIa, White, WhiteFull, WhiteI, WhiteIa, Blue, BlueFull, BlueI, BlueIa, Yellow, YellowFull, YellowI, YellowIa, Orange, OrangeFull, OrangeI, OrangeIa, Green, GreenFull, GreenI, GreenIa, Purple, PurpleFull, PurpleI, PurpleIa, AmberFull, Amber, AmberI, AmberIa

RedFull = rgb(255,0,0) 
Red = rgb(255,128,128)
RedI = 2
RedIa = 20
PinkFull = rgb(255,0,225)
Pink = rgb(255,0,255)
PinkI = 10
PinkIa = 20
WhiteFull = rgb(255,255,128) 
White = rgb(255,255,255)
WhiteI = 7
WhiteIa = 7
BlueFull = rgb(0,100,255)
Blue = rgb(0,255,255)
BlueI = 20
BlueIa = 30
YellowFull = rgb(255,255,128)
Yellow = rgb(255,255,0)
YellowI = 20
YellowIa = 30
OrangeFull = rgb(255,128,64)
Orange = rgb(128,128,0)
OrangeI = 20
OrangeIa = 30
GreenFull = rgb(0,255,0)
Green = rgb(0,128,0)
GreenI = 5
GreenIa = 15
PurpleFull = rgb(128,0,255)
Purple = rgb(64,0,128)
PurpleI = 2
PurpleIa = 10
AmberFull = rgb(255,197,143)
Amber = rgb(255,197,143)
AmberI = 10
AmberIa = 2


'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
'   Options
'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Dim BLPlasticsType, xxUnderApronLights, BumperCapLightsType, Instruction_CardsType, FlipperColorType, xxLowerFlashers, xxLowerGI

Sub CheckOptions()


If HideRails = 1 then

Ramp16.Visible = false
Ramp15.Visible = false

End If


 ''' BlackLight Plastics
	If BLPlastics = 0 Then
		BLPlasticsType = Int(Rnd*4)+1
	Else
		BLPlasticsType = BLPlastics
	End If

	If BLPlasticsType = 1 Then
		pPlasticU1B.Material = "Plastic 99"
		pPlasticU1B.DisableLighting = 0
		pPlasticU2B.Material = "Plastic 99"
		pPlasticU2B.DisableLighting = 0
		pPlasticU3B.Material = "Plastic 99"
		pPlasticU3B.DisableLighting = 0
		pPlasticU4B.Material = "Plastic 99"
		pPlasticU4B.DisableLighting = 0

		pPlastic1B.Material = "Plastic 99"
		pPlastic1B.DisableLighting = 0
		pPlastic2B.Material = "Plastic 99"
		pPlastic2B.DisableLighting = 0
		pPlastic3B.Material = "Plastic 99"
		pPlastic3B.DisableLighting = 0
		pPlastic4B.Material = "Plastic 99"
		pPlastic4B.DisableLighting = 0
		pPlastic5B.Material = "Plastic 99"
		pPlastic5B.DisableLighting = 0
		pPlastic6B.Material = "Plastic 99"
		pPlastic6B.DisableLighting = 0

		Flasher23.imageA = "hhmetalsframe1Clear"

		pPlasticsLowerB.Material = "Plastic 99"
		pPlasticsLowerB.DisableLighting = 0
	End If

	If BLPlasticsType = 2 Then
		pPlasticU1B.Material = "AcrylicBLRed"
		pPlasticU1B.DisableLighting = 1
		pPlasticU2B.Material = "AcrylicBLRed"
		pPlasticU2B.DisableLighting = 1
		pPlasticU3B.Material = "AcrylicBLRed"
		pPlasticU3B.DisableLighting = 1
		pPlasticU4B.Material = "AcrylicBLRed"
		pPlasticU4B.DisableLighting = 1

		pPlastic1B.Material = "AcrylicBLRed"
		pPlastic1B.DisableLighting = 1
		pPlastic2B.Material = "AcrylicBLRed"
		pPlastic2B.DisableLighting = 1
		pPlastic3B.Material = "AcrylicBLRed"
		pPlastic3B.DisableLighting = 1
		pPlastic4B.Material = "AcrylicBLRed"
		pPlastic4B.DisableLighting = 1
		pPlastic5B.Material = "AcrylicBLRed"
		pPlastic5B.DisableLighting = 1
		pPlastic6B.Material = "AcrylicBLRed"
		pPlastic6B.DisableLighting = 1

		Flasher23.imageA = "hhmetalsframe1Red"

		pPlasticsLowerB.Material = "AcrylicBLRed"
		pPlasticsLowerB.DisableLighting = 1
	End If

	If BLPlasticsType = 3 Then
		pPlasticU1B.Material = "AcrylicBLGreen"
		pPlasticU1B.DisableLighting = 1
		pPlasticU2B.Material = "AcrylicBLGreen"
		pPlasticU2B.DisableLighting = 1
		pPlasticU3B.Material = "AcrylicBLGreen"
		pPlasticU3B.DisableLighting = 1
		pPlasticU4B.Material = "AcrylicBLGreen"
		pPlasticU4B.DisableLighting = 1

		pPlastic1B.Material = "AcrylicBLGreen"
		pPlastic1B.DisableLighting = 1
		pPlastic2B.Material = "AcrylicBLGreen"
		pPlastic2B.DisableLighting = 1
		pPlastic3B.Material = "AcrylicBLGreen"
		pPlastic3B.DisableLighting = 1
		pPlastic4B.Material = "AcrylicBLGreen"
		pPlastic4B.DisableLighting = 1
		pPlastic5B.Material = "AcrylicBLGreen"
		pPlastic5B.DisableLighting = 1
		pPlastic6B.Material = "AcrylicBLGreen"
		pPlastic6B.DisableLighting = 1

		Flasher23.imageA = "hhmetalsframe1Green"

		pPlasticsLowerB.Material = "AcrylicBLGreen"
		pPlasticsLowerB.DisableLighting = 1
	End If

	If BLPlasticsType = 4 Then
		pPlasticU1B.Material = "AcrylicBLRed"
		pPlasticU1B.DisableLighting = 1
		pPlasticU2B.Material = "AcrylicBLRed"
		pPlasticU2B.DisableLighting = 1
		pPlasticU3B.Material = "AcrylicBLRed"
		pPlasticU3B.DisableLighting = 1
		pPlasticU4B.Material = "AcrylicBLRed"
		pPlasticU4B.DisableLighting = 1

		pPlastic1B.Material = "AcrylicBLGreen"
		pPlastic1B.DisableLighting = 1
		pPlastic2B.Material = "AcrylicBLGreen"
		pPlastic2B.DisableLighting = 1
		pPlastic3B.Material = "AcrylicBLGreen"
		pPlastic3B.DisableLighting = 1
		pPlastic4B.Material = "AcrylicBLGreen"
		pPlastic4B.DisableLighting = 1
		pPlastic5B.Material = "AcrylicBLGreen"
		pPlastic5B.DisableLighting = 1
		pPlastic6B.Material = "AcrylicBLGreen"
		pPlastic6B.DisableLighting = 1

		Flasher23.imageA = "hhmetalsframe1Green"

		pPlasticsLowerB.Material = "AcrylicBLRed"
		pPlasticsLowerB.DisableLighting = 1
	End If


	If BlacklightTargets = 1 Then
		psw05.DisableLighting = .1
		pSecretTarget.DisableLighting = .1
		psw15.DisableLighting = .1
		psw55.DisableLighting = .1
		psw03.DisableLighting = .25
		psw13.DisableLighting = .25
		psw23.DisableLighting = .25
		psw33.DisableLighting = .25

		psw05.image = "TargetTexture_on"
		pSecretTarget.image = "TargetTexture_on"
		psw15.image = "TargetTexture_on"
		psw55.image = "TargetTexture_on"
		psw03.image = "TargetTexture_on"
		psw13.image = "TargetTexture_on"
		psw23.image = "TargetTexture_on"
		psw33.image = "TargetTexture_on"
	End If

'Flipper Colors

	If FlipperColor = 0 Then
		FlipperColorType = Int(Rnd*4)+1
	Else
		FlipperColorType = FlipperColor
	End If

	If FlipperColorType = 1 Then
		pLittleFlipper_Rubber.Material = "Red Rubber"
		If CMSFS = 1 Then
			pLittleFlipper_screw.Material = "Red Rubber"
		End If
		PFlipperUR.image = "flipper_red_left"

		PFlipperLR.image = "flipper_red_right"
		PFlipperLL.image = "flipper_red_left"

		PRightFlipper.image = "flipper_red_right"
		PLeftFlipper.image = "flipper_red_left"
		PLeftFlipper2.image = "flipper_red_left"
		PRightFlipper2.image = "flipper_red_right"
	End If

	If FlipperColorType = 2 Then
		pLittleFlipper_Rubber.Material = "Green Rubber"
		If CMSFS = 1 Then
			pLittleFlipper_screw.Material = "Green Rubber"
		End If
		PFlipperUR.image = "flipper_Green_left"

		PFlipperLR.image = "flipper_Green_right"
		PFlipperLL.image = "flipper_Green_left"

		PRightFlipper.image = "flipper_Green_right"
		PLeftFlipper.image = "flipper_Green_left"
		PLeftFlipper2.image = "flipper_Green_left"
		PRightFlipper2.image = "flipper_Green_right"
	End If

	If FlipperColorType = 3 Then
		pLittleFlipper_Rubber.Material = "Green Rubber"
		If CMSFS = 1 Then
			pLittleFlipper_screw.Material = "Green Rubber"
		End If
		PFlipperUR.image = "flipper_Green_left"

		PFlipperLR.image = "flipper_Green_right"
		PFlipperLL.image = "flipper_Green_left"

		PRightFlipper.image = "flipper_red_right"
		PLeftFlipper.image = "flipper_red_left"
		PLeftFlipper2.image = "flipper_red_left"
		PRightFlipper2.image = "flipper_red_right"
	End If

	If FlipperColorType = 4 Then
		pLittleFlipper_Rubber.Material = "Red Rubber"
		If CMSFS = 1 Then
			pLittleFlipper_screw.Material = "Red Rubber"
		End If
		PFlipperUR.image = "flipper_red_left"

		PFlipperLR.image = "flipper_red_right"
		PFlipperLL.image = "flipper_red_left"

		PRightFlipper.image = "flipper_Green_right"
		PLeftFlipper.image = "flipper_Green_left"
		PLeftFlipper2.image = "flipper_Green_left"
		PRightFlipper2.image = "flipper_Green_right"
	End If

' Instruction Cards

	If Instruction_Cards = 0 Then
		Instruction_CardsType = Int(Rnd*4)+1
	Else
		Instruction_CardsType = Instruction_Cards
	End If

	If Instruction_CardsType = 1 Then
		LeftInstuctionCard.Image = "HHICLeft3ball"
		RightInstuctionCard.Image = "HHICRight1"
	End If

	If Instruction_CardsType = 2 Then
		LeftInstuctionCard.Image = "HHICLeft5ball"
		RightInstuctionCard.Image = "HHICRight1"
	End If

	If Instruction_CardsType = 3 Then
		LeftInstuctionCard.Image = "HHICLeftC1"
		RightInstuctionCard.Image = "HHICRightC1"
	End If

	If Instruction_CardsType = 4 Then
		LeftInstuctionCard.Image = "HHICLeftC2"
		RightInstuctionCard.Image = "HHICRightC2"
	End If



'''''Game Info Card

	If GameInfoCard = 1 Then
		pInfoCard.Visible = True
	Else
		pInfoCard.Visible = False
	End If


'''''Window Color

	If WindowColor = 0 Then
		pWindow.Image = "Window_clear"
	End If

	If WindowColor = 1 Then
		pWindow.Image = "window_green_texture"
	End If


'''''Window Fome

	If Windowfome = 1 Then
		pWindowFome.image = "WindowsFome"
	End If

	If Windowfome = 2 Then
		pWindowFome.image = "WindowsFome_green"
	End If

	If Windowfome = 3 Then
		pWindowFome.image = "WindowsFome_red"
	End If



'Under Apron Lights

	If UnderApronLights = 1 Then
		for each xxUnderApronLights in cUnderApronLights
			xxUnderApronLights.state = 1
			xxUnderApronLights.Color = rgb(0,128,0)
			xxUnderApronLights.ColorFull = rgb(0,255,0)
			next
	End If

	If UnderApronLights = 2 Then
		for each xxUnderApronLights in cUnderApronLights
			xxUnderApronLights.state = 1
			xxUnderApronLights.Color = rgb(255,0,0)
			xxUnderApronLights.ColorFull = rgb(255,100,100)
			next
	End If

'BumperCap Lights

	If BumperCapLights = 0 Then
		BumperCapLightsType = Int(Rnd*5)+1
	Else
		BumperCapLightsType = BumperCapLights
	End If

	If BumperCapLightsType = 1 Then
		'Main
		bLight1.Color = rgb(255,128,0)
		bLight1.ColorFull = rgb(255,228,202)
		bLight1b.Color = rgb(255,128,0)
		bLight1b.ColorFull = rgb(255,228,202)
		bLight2.Color = rgb(255,128,0)
		bLight2.ColorFull = rgb(255,228,202)
		bLight2b.Color = rgb(255,128,0)
		bLight2b.ColorFull = rgb(255,228,202)
		'Upper
		bLight3.Color = rgb(255,128,0)
		bLight3.ColorFull = rgb(255,228,202)
		bLight3b.Color = rgb(255,128,0)
		bLight3b.ColorFull = rgb(255,228,202)
		'Lower
		bLight4.Color = rgb(255,128,0)
		bLight4.ColorFull = rgb(255,228,202)
	End If

	If BumperCapLightsType = 2 Then
		'Main
		bLight1.Color = rgb(128,0,0)
		bLight1.ColorFull = rgb(255,9,9)
		bLight1b.Color = rgb(128,0,0)
		bLight1b.ColorFull = rgb(255,9,9)
		bLight2.Color = rgb(128,0,0)
		bLight2.ColorFull = rgb(255,9,9)
		bLight2b.Color = rgb(128,0,0)
		bLight2b.ColorFull = rgb(255,9,9)
		'Upper
		bLight3.Color = rgb(128,0,0)
		bLight3.ColorFull = rgb(255,9,9)
		bLight3b.Color = rgb(128,0,0)
		bLight3b.ColorFull = rgb(255,9,9)
		'Lower
		bLight4.Color = rgb(128,0,0)
		bLight4.ColorFull = rgb(255,9,9)
	End If

	If BumperCapLightsType = 3 Then
		'Main
		bLight1.Color = rgb(0,128,0)
		bLight1.ColorFull = rgb(0,255,0)
		bLight1b.Color = rgb(0,128,0)
		bLight1b.ColorFull = rgb(0,255,0)
		bLight2.Color = rgb(0,128,0)
		bLight2.ColorFull = rgb(0,255,0)
		bLight2b.Color = rgb(0,128,0)
		bLight2b.ColorFull = rgb(0,255,0)
		'Upper
		bLight3.Color = rgb(0,128,0)
		bLight3.ColorFull = rgb(0,255,0)
		bLight3b.Color = rgb(0,128,0)
		bLight3b.ColorFull = rgb(0,255,0)
		'Lower
		bLight4.Color = rgb(0,128,0)
		bLight4.ColorFull = rgb(0,255,0)
	End If

	If BumperCapLightsType = 4 Then
		'Main
		bLight1.Color = rgb(128,0,0)
		bLight1.ColorFull = rgb(255,9,9)
		bLight1b.Color = rgb(128,0,0)
		bLight1b.ColorFull = rgb(255,9,9)
		bLight2.Color = rgb(128,0,0)
		bLight2.ColorFull = rgb(255,9,9)
		bLight2b.Color = rgb(128,0,0)
		bLight2b.ColorFull = rgb(255,9,9)
		'Upper
		bLight3.Color = rgb(0,128,0)
		bLight3.ColorFull = rgb(0,255,0)
		bLight3b.Color = rgb(0,128,0)
		bLight3b.ColorFull = rgb(0,255,0)
		'Lower
		bLight4.Color = rgb(0,128,0)
		bLight4.ColorFull = rgb(0,255,0)
	End If

	If BumperCapLightsType = 5 Then
		'Main
		bLight1.Color = rgb(0,128,0)
		bLight1.ColorFull = rgb(0,255,0)
		bLight1b.Color = rgb(0,128,0)
		bLight1b.ColorFull = rgb(0,255,0)
		bLight2.Color = rgb(0,128,0)
		bLight2.ColorFull = rgb(0,255,0)
		bLight2b.Color = rgb(0,128,0)
		bLight2b.ColorFull = rgb(0,255,0)
		'Upper
		bLight3.Color = rgb(128,0,0)
		bLight3.ColorFull = rgb(255,9,9)
		bLight3b.Color = rgb(128,0,0)
		bLight3b.ColorFull = rgb(255,9,9)
		'Lower
		bLight4.Color = rgb(128,0,0)
		bLight4.ColorFull = rgb(255,9,9)
	End If

	If ExtraFlashers = 1 Then
		Primitive22.DisableLighting = 0
		Flasher21.Visible = true
		Flasher20.Visible = true
		Flasher19.Visible = true
		Flasher18.Visible = true
		Flasher17.Visible = true
		Flasher16.Visible = true
		Flasher12.Visible = true
		Flasher13.Visible = true
		Flasher14.Visible = true
		Flasher15.Visible = true
	Else
		Primitive22.DisableLighting = .4
		Flasher21.Visible = False
		Flasher20.Visible = False
		Flasher19.Visible = False
		Flasher18.Visible = False
		Flasher17.Visible = False
		Flasher16.Visible = False
		Flasher12.Visible = False
		Flasher13.Visible = False
		Flasher14.Visible = False
		Flasher15.Visible = False
	End If

''''GiColor

	If GIColorMod = 0 Then
		GIColorModType = Int(Rnd*6)
	Else
		GIColorModType = GIColorMod
	End If


	If GIColorModType = 1 then
		for each xxGIColor in GIMainA
			xxGIColor.Color=Amber
			xxGIColor.ColorFull=AmberFull
			xxGIColor.Intensity = AmberI
			next
		for each xxGIColor in GIMainAa
			xxGIColor.Intensity = AmberI
			next
		for each xxGIColor in GIMainB
			xxGIColor.Color=Amber
			xxGIColor.ColorFull=AmberFull
			xxGIColor.Intensity = AmberI
			next
		for each xxGIColor in GIMainBa
			xxGIColor.Intensity = AmberIa
			next
		If CCGI = 0 then
		for each xxGIColor in GIMainC
			xxGIColor.Color=White
			xxGIColor.ColorFull=WhiteFull
			xxGIColor.Intensity = WhiteI
			next
		for each xxGIColor in GIMainCa
			xxGIColor.Intensity = WhiteIa
			next
		End If
		for each xxGIColor in GIUpperA
			xxGIColor.Color=Amber
			xxGIColor.ColorFull=AmberFull
			xxGIColor.Intensity = AmberI
			next
		for each xxGIColor in GIUpperAa
			xxGIColor.Intensity = AmberIa
			next
		for each xxGIColor in GIUpperB
			xxGIColor.Color=Amber
			xxGIColor.ColorFull=AmberFull
			xxGIColor.Intensity = AmberIa
			next
		for each xxGIColor in GILowerL
			xxGIColor.Color=Amber
			xxGIColor.ColorFull=AmberFull
			xxGIColor.Intensity = AmberI
			next
		for each xxGIColor in LowerFlashers
			xxGIColor.Color=Amber
			next
	End If


	If GIColorModType = 2 then
		for each xxGIColor in GIMainA
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainAa
			xxGIColor.Intensity = RedIa
			next
		for each xxGIColor in GIMainB
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainBa
			xxGIColor.Intensity = RedIa
			next
		If CCGI = 0 then
		for each xxGIColor in GIMainC
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainCa
			xxGIColor.Intensity = RedIa
			next
		End If
		for each xxGIColor in GIUpperA
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIUpperAa
			xxGIColor.Intensity = RedIa
			next
		for each xxGIColor in GIUpperB
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedIa
			next
		for each xxGIColor in GILowerL
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in LowerFlashers
			xxGIColor.Color=Red
			next
	End If


	If GIColorModType = 3 then
		for each xxGIColor in GIMainA
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIMainAa
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GIMainB
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIMainBa
			xxGIColor.Intensity = GreenIa
			next
		If CCGI = 0 then
		for each xxGIColor in GIMainC
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIMainCa
			xxGIColor.Intensity = GreenIa
			next
		End If
		for each xxGIColor in GIUpperA
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIUpperAa
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GIUpperB
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GILowerL
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in LowerFlashers
			xxGIColor.Color=Green
			next
	End If

	If GIColorModType = 4 then
		for each xxGIColor in GIMainA
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainAa
			xxGIColor.Intensity = RedIa
			next
		for each xxGIColor in GIMainB
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainBa
			xxGIColor.Intensity = RedIa
			next
		If CCGI = 0 then
		for each xxGIColor in GIMainC
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainCa
			xxGIColor.Intensity = RedIa
			next
		End If
		for each xxGIColor in GIUpperA
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIUpperAa
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GIUpperB
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GILowerL
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in LowerFlashers
			xxGIColor.Color=Green
			next
	End If

	If GIColorModType = 5 then
		for each xxGIColor in GIMainA
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIMainAa
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GIMainB
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainBa
			xxGIColor.Intensity = RedIa
			next
		If CCGI = 0 then
		for each xxGIColor in GIMainC
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainCa
			xxGIColor.Intensity = RedIa
			next
		End If
		for each xxGIColor in GIUpperA
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIUpperAa
			xxGIColor.Intensity = PurpleIa
			next
		for each xxGIColor in GIUpperB
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GILowerL
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in LowerFlashers
			xxGIColor.Color=Green
			next
	End If

	If GIColorModType = 6 then
		for each xxGIColor in GIMainA
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GIMainAa
			xxGIColor.Intensity = GreenIa
			next
		for each xxGIColor in GIMainB
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainBa
			xxGIColor.Intensity = RedIa
			next
		If CCGI = 0 then
		for each xxGIColor in GIMainC
			xxGIColor.Color=Red
			xxGIColor.ColorFull=RedFull
			xxGIColor.Intensity = RedI
			next
		for each xxGIColor in GIMainCa
			xxGIColor.Intensity = RedIa
			next
		End If
		for each xxGIColor in GIUpperA
			xxGIColor.Color=Purple
			xxGIColor.ColorFull=PurpleFull
			xxGIColor.Intensity = PurpleIa
			next
		for each xxGIColor in GIUpperAa
			xxGIColor.Intensity = PurpleIa
			next
		for each xxGIColor in GIUpperB
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in GILowerL
			xxGIColor.Color=Green
			xxGIColor.ColorFull=GreenFull
			xxGIColor.Intensity = GreenI
			next
		for each xxGIColor in LowerFlashers
			xxGIColor.Color=Green
			next

	End If


''''Lower Lighting

	If LowerLighting = 0 Then

		for each xxLowerFlashers in LowerFlashers
			xxLowerFlashers.Color=White
			next

		for each xxLowerGI in LowerGI
			xxLowerGI.Color=Amber
			xxLowerGI.ColorFull=AmberFull
			xxLowerGI.Intensity = AmberIa
			next
		Flasher22.Color = white
	End If

	If LowerLighting = 1 Then

		for each xxLowerFlashers in LowerFlashers
			xxLowerFlashers.Color=Green
			next

		for each xxLowerGI in LowerGI
			xxLowerGI.Color=Green
			xxLowerGI.ColorFull=GreenFull
			xxLowerGI.Intensity = GreenI
			next
		Flasher22.Color = rgb(0,100,0)
	End If

	If CCGI = 1 Then
		CCGI2.Enabled = True
	End If

	IF CCSD = 1 Then
		CCGI2.Enabled = True
	End If

End Sub


'ColorChangingGI 2

'Dim R, G, B
Dim CCGI2Step, xxGiCC
Dim Red1RGB, Green1RGB, Blue1RGB, Red2RGB, Green2RGB, Blue2RGB, Red3RGB, Green3RGB, Blue3RGB

Red1RGB = 255
Green1RGB = 0
Blue1RGB = 0

Red2RGB = 0
Green2RGB = 255
Blue2RGB = 0

Red3RGB = 0
Green3RGB = 0
Blue3RGB = 255


Sub CCGI2_timer ()



	If Red1RGB < 0 then Red1RGB = 0 end If
	If Red2RGB < 0 then Red2RGB = 0 end If
	If Red3RGB < 0 then Red3RGB = 0 end If
	If Green1RGB < 0 then Green1RGB = 0 End If
	If Green2RGB < 0 then Green2RGB = 0 End If
	If Green3RGB < 0 then Green3RGB = 0 End If
	If Blue1RGB < 0 then Blue1RGB = 0 End If
	If Blue2RGB < 0 then Blue2RGB = 0 End If
	If Blue3RGB < 0 then Blue3RGB = 0 End If
	If Red1RGB > 255 then Red1RGB = 255 End If
	If Red2RGB > 255 then Red2RGB = 255 End If
	If Red3RGB > 255 then Red3RGB = 255 End If
	If Green1RGB > 255 then Green1RGB = 255 End If
	If Green2RGB > 255 then Green2RGB = 255 End If
	If Green3RGB > 255 then Green3RGB = 255 End If
	If Blue1RGB > 255 then Blue1RGB = 255 End If
	If Blue2RGB > 255 then Blue2RGB = 255 End If
	If Blue3RGB > 255 then Blue3RGB = 255 End If
	
	If CCGI2Step > 0 and CCGI2Step < 255 Then
		Green1RGB = Green1RGB + 1
		Red3RGB = Red3RGB + 1
		Blue2RGB = Blue2RGB + 1
	End If
		If CCGI2Step > 255 and CCGI2Step < 510 Then
		Red1RGB = Red1RGB - 1
		Blue3RGB = Blue3RGB - 1
		Green2RGB = Green2RGB - 1

	End If
	If CCGI2Step > 510 and CCGI2Step < 765 Then
		Blue1RGB = Blue1RGB + 1
		Green3RGB = Green3RGB + 1
		Red2RGB = Red2RGB + 1

	End If
	If CCGI2Step > 765 and CCGI2Step < 1020 Then
		Green1RGB = Green1RGB - 1
		Red3RGB = Red3RGB - 1
		Blue2RGB = Blue2RGB - 1

	End If
	If CCGI2Step > 1020 and CCGI2Step < 1275 Then
		Red1RGB = Red1RGB + 1
		Blue3RGB = Blue3RGB + 1
		Green2RGB = Green2RGB + 1

	End If
	If CCGI2Step > 1275 and CCGI2Step < 1530 Then
		Blue1RGB = Blue1RGB - 1
		Green3RGB = Green3RGB - 1
		Red2RGB = Red2RGB - 1
	End If

If CCGI2Step = 1530 then CCGI2Step = 0 End If


CCGI2Step = CCGI2Step + 1

If CCGI = 1 then
	for each xxGiCC in GIMainC
			xxGiCC.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)
			xxGiCC.ColorFull = rgb(Red1RGB,Green1RGB,Blue1RGB)
			next
End If

If CCSD = 1 Then
	for each xxGiCC in AllLEDs
			xxGiCC.Color = rgb(Red2RGB,Green2RGB,Blue2RGB)
			xxGiCC.ColorFull = rgb(Red2RGB,Green2RGB,Blue2RGB)
			next
End If

End Sub


'#############################
'  Rotate Primitive Things
'#############################

Sub Timer_Timer()
	p_gate1.Rotx = gate1.CurrentAngle + 120
	pLittleFlipper_Bat.RotY = FlipperUL.CurrentAngle + 180
	pLittleFlipper_Rubber.RotY = FlipperUL.CurrentAngle+ 180
	pLittleFlipper_Screw.RotY = FlipperUL.CurrentAngle+ 180
	PLeftFlipper.RotZ = LeftFlipper.CurrentAngle
	PLeftFlipper2.RotZ = LeftFlipper2.CurrentAngle
	PRightFlipper.RotZ = RightFlipper.CurrentAngle
	PRightFlipper2.RotZ = RightFlipper2.CurrentAngle
	PFlipperUR.RotZ = FlipperUR.CurrentAngle
	PFlipperLL.RotZ = FlipperLL.CurrentAngle
	PFlipperLR.RotZ = FlipperLR.CurrentAngle
    p_gate2.Rotx = gate2.CurrentAngle + 120
    p_gate3.Rotx = gate3.CurrentAngle + 90
    p_gate4.Rotx = gate4.CurrentAngle + 90
	pScoreSpring2.TransX = sin( (gate3.CurrentAngle+180) * (2*PI/360)) * 5
	pScoreSpring2.TransY = sin( (gate3.CurrentAngle- 90) * (2*PI/360)) * 5
	pScoreSpring.TransX = sin( (gate4.CurrentAngle+180) * (2*PI/360)) * 5
	pScoreSpring.TransY = sin( (gate4.CurrentAngle- 90) * (2*PI/360)) * 5
End Sub

''''''''''''''''''''''''''''''''''
''''''''''''''Rubbers
''''''''''''''''''''''''''''''''''

Sub sw66b_hit()
	vpmTimer.PulseSw(66)
	DOF 105, DOFPulse
End Sub

''''''''''''''''''''''''''''''''''
''''''''''''Switches
''''''''''''''''''''''''''''''''''

Sub Sw04_Hit()
	Switch04dir = 1
	Sw04Move = 1
	Me.TimerEnabled = true
	PlaySoundAt "sensor",sw04
	Controller.Switch(04)=1
End Sub

Sub Sw04_UnHit()
	Switch04dir = -1
	Sw04Move = 5
	Me.TimerEnabled = true
	Controller.Switch(04)=0
End Sub


Sub Sw21_Hit()
	Switch21dir = 1
	Sw21Move = 1
	Me.TimerEnabled = true
	PlaySoundAt "sensor",sw21
	Controller.Switch(21)=1
End Sub

Sub Sw21_UnHit()
	Switch21dir = -1
	Sw21Move = 5
	Me.TimerEnabled = true
	Controller.Switch(21)=0
End Sub


Sub Sw34_Hit()
	Switch34dir = 1
	Sw34Move = 1
	Me.TimerEnabled = true
	PlaySoundAt "sensor",sw34
	Controller.Switch(34)=1
End Sub

Sub Sw34_UnHit()
	Switch34dir = -1
	Sw34Move = 5
	Me.TimerEnabled = true
	Controller.Switch(34)=0
End Sub

Sub Sw54_Hit()
	Switch54dir = 1
	Sw54Move = 1
	Me.TimerEnabled = true
	PlaySoundAt "sensor",sw54
	Controller.Switch(54)=1
End Sub

Sub Sw54_UnHit()
	Switch54dir = -1
	Sw54Move = 5
	Me.TimerEnabled = true
	Controller.Switch(54)=0
End Sub

Sub Sw64_Hit()
	Controller.Switch(64)=1:PlaySoundAt "sensor",sw64:Gate3.Damping = .85:sw64step = 0:me.timerenabled = True
End Sub
Sub Sw64_UnHit()
	Controller.Switch(64)=0
End Sub

Sub Sw35_Hit()
	Controller.Switch(35)=1:PlaySoundAt "sensor",sw35:Gate4.Damping = .85:sw35step = 0:me.timerenabled = True
End Sub
Sub Sw35_UnHit()
	Controller.Switch(35)=0
End Sub



''''''''''''Spring Gate animations

Dim sw64step, sw35step

Sub Sw64_timer()
	Select case sw64step
	
		Case 0:'Gate3.Damping = .9:'Gate3.GravityFactor = 5

		Case 1:Gate3.Damping = .9:Gate3.GravityFactor = 3

		Case 2:Gate3.Damping = .95

		Case 3:Gate3.Damping = .99999

		Case 4:

		Case 35: me.timerenabled = false:sw64step = 0:Gate3.Damping = .85:Gate3.GravityFactor = 5

	End Select

	sw64step = sw64step + 1

End Sub

Sub Sw35_timer()
	Select case sw35step
	
		Case 0:'Gate4.Damping = .9:'Gate4.GravityFactor = 5

		Case 1:Gate4.Damping = .9:Gate4.GravityFactor = 3

		Case 2:Gate4.Damping = .95

		Case 3:Gate4.Damping = .99999

		Case 4:

		Case 35: me.timerenabled = false:sw35step = 0:Gate4.Damping = .85:Gate4.GravityFactor = 5

	End Select

	sw35step = sw35step + 1

End Sub


'''''''switches for lower throughs

Sub sw06_Hit():Controller.Switch(6) = 1:PlaySoundAt "sensor",sw06:End Sub
Sub sw06_UnHit():Controller.Switch(6) = 0:End Sub

Sub sw16_Hit():Controller.Switch(16) = 1:PlaySoundAt "sensor",sw16:End Sub
Sub sw16_UnHit():Controller.Switch(16) = 0:End Sub

Sub sw26_Hit():Controller.Switch(26) = 1:SecretTargetStep = 8:PlaySoundAt "sensor",sw26:End Sub
Sub sw26_UnHit():Controller.Switch(26) = 0:End Sub

Sub sw36_Hit():Controller.Switch(36) = 1:PlaySoundAt "sensor",sw36:End Sub
Sub sw36_UnHit():Controller.Switch(36) = 0:End Sub



'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'    Switch Animations
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Dim Switch04dir, SW04Move

Sub sw04_timer()
Select case Sw04Move

	Case 0:me.TimerEnabled = false:pRollover3.RotX = 79

	Case 1:pRollover3.RotX = 84

	Case 2:pRollover3.RotX = 89

	Case 3:pRollover3.RotX = 94

	Case 4:pRollover3.RotX = 99

	Case 5:pRollover3.RotX = 104

	Case 6:me.TimerEnabled = false:pRollover3.RotX = 109

End Select

SW04Move = SW04Move + Switch04dir

End Sub


Dim Switch21dir, SW21Move

Sub sw21_timer()
Select case Sw21Move

	Case 0:me.TimerEnabled = false:pRollover4.RotX = 90

	Case 1:pRollover4.RotX = 95

	Case 2:pRollover4.RotX = 100

	Case 3:pRollover4.RotX = 105

	Case 4:pRollover4.RotX = 110

	Case 5:pRollover4.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover4.RotX = 120

End Select

SW21Move = SW21Move + Switch21dir

End Sub


Dim Switch34dir, SW34Move

Sub sw34_timer()
Select case Sw34Move

	Case 0:me.TimerEnabled = false:pRollover2.RotX = 90

	Case 1:pRollover2.RotX = 95

	Case 2:pRollover2.RotX = 100

	Case 3:pRollover2.RotX = 105

	Case 4:pRollover2.RotX = 110

	Case 5:pRollover2.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover2.RotX = 120

End Select

SW34Move = SW34Move + Switch34dir

End Sub



Dim Switch54dir, SW54Move

Sub sw54_timer()
Select case Sw54Move

	Case 0:me.TimerEnabled = false:pRollover1.RotX = 90

	Case 1:pRollover1.RotX = 95

	Case 2:pRollover1.RotX = 100

	Case 3:pRollover1.RotX = 105

	Case 4:pRollover1.RotX = 110

	Case 5:pRollover1.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover1.RotX = 120

End Select

SW54Move = SW54Move + Switch54dir

End Sub


''''''''''''''''''''''''''''''''''
'''''''''''' Kickers
''''''''''''''''''''''''''''''''''

''''  Up Kick


Sub sw45_hit():Playsoundat ("kicker_enter_center"),sw45:Controller.Switch(45) = 1:End Sub


Sub UpKick(enabled)
	sw45.timerenabled = 1
	Controller.Switch(45) = 0
End Sub

Dim sw45step

Sub sw45_timer()
	Select Case sw45step
		Case 0:
		Case 1:
		Case 2:
		Case 3:	Playsoundat SoundFX("Kicker_Release",DOFContactors),sw45:DOF 103,DOFPulse
		Case 4: sw45.Kick 0,35,1.56
		Case 5:	
		Case 6:
		Case 7:
		Case 8:
		Case 9:
		Case 20:sw45.timerEnabled = 0:sw45step = 0
	End Select
	sw45step = sw45step + 1
End Sub

''''''''''''''' Kick 46

Dim SW46Step, sw46ark

Sub sw46_hit():Playsoundat ("kicker_enter_center"),sw46:Controller.Switch(46) = 1:End Sub

Sub sw46_unhit():Playsoundat SoundFX("Kicker_Release",DOFContactors),sw46:Controller.Switch(46) = 0:End Sub

Sub Kick46(enabled)
	If sw46ark = 1 Then
	Else
		sw46ark = 1
		SW46Step = 0
		PlaySound ""
		sw46.timerenabled = true
	End If
End Sub

Sub SW46_Timer()
	Select Case SW46Step
		Case 0:	TWKicker1.transY = 2
		Case 1: TWKicker1.transY = 5:cBall1.velz = 10:cBall1.vely = 5:'cBall1.velx = 2:'cBall1.vely = 10
		Case 2: TWKicker1.transY = 5:cBall1.velx = 2
		Case 3: TWKicker1.transY = 2
		Case 4:	TWKicker1.transY = 0:
		Case 5:
		Case 9: Controller.Switch(46) = 0
		Case 10:Me.TimerEnabled = 0:SW46Step = 0:sw46ark = 0
	End Select
	SW46Step = SW46Step + 1
End Sub


'''''''''''' Kick 65

Sub sw65_hit():Controller.Switch(65) = 1:End Sub

Sub sw65b_hit():Playsoundat ("kicker_enter_center"),LockNut3:End Sub

 
Sub sw65_unhit():Controller.Switch(65) = 0:End Sub
 
Sub sw65b_unhit():Playsoundat SoundFX("Kicker_Release",DOFContactors),LockNut3:End Sub

'Sub sw65b_unhit():Playsoundat ("Kicker_Release"),LockNut3:End Sub
 
Sub Kick65()
PlungerIM.AutoFire
End Sub

'''' Basement Up Kick


Sub sw31_hit():Playsoundat ("kicker_enter_center"),sw31:Controller.Switch(31) = 1:End Sub


Sub BasementUpKick()
	sw31.timerenabled = 1
	Controller.Switch(31) = 0
End Sub

Dim sw31step

Sub sw31_timer()
	Select Case sw31step
		Case 0:
		Case 1:
		Case 2:
		Case 3:	Playsoundat SoundFX("Kicker_Release",DOFContactors),sw31:DOF 101, DOFPulse
		Case 4: sw31.Kick 0,55,1.50
		Case 5:	
		Case 6:
		Case 7:
		Case 8:
		Case 9:
		Case 20:sw31.timerEnabled = 0:sw31step = 0
	End Select
	sw31step = sw31step + 1
End Sub


' Basement Special

Sub sw41_hit():Playsoundat ("kicker_enter_center"),sw41:Controller.Switch(41) = 1:End Sub


Sub BS(enabled)
	sw41.timerenabled = 1
	Controller.Switch(41) = 0
End Sub

Dim sw41step

Sub sw41_timer()
	Select Case sw41step
		Case 0:
		Case 1:
		Case 2:
		Case 3: Playsoundat SoundFX("Kicker_Release",DOFContactors),sw41
		Case 4: sw41.Kick 90,15
		Case 5:	
		Case 6:
		Case 7:
		Case 8:
		Case 9:
		Case 20:sw41.timerEnabled = 0:sw41step = 0
	End Select
	sw41step = sw41step + 1
End Sub


''''''''''''''''
'Targets
''''''''''''''''

'Upper


' Drop Targets

Sub sw02_Hit:udtDrop.Hit 1:sw02.isDropped = 1:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw02:End Sub			

Sub sw12_Hit:udtDrop.Hit 2:sw12.isDropped = 1:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw12:End Sub			

Sub sw22_Hit:udtDrop.Hit 3:sw22.isDropped = 1:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw22:End Sub			

Sub sw32_Hit:udtDrop.Hit 4:sw32.isDropped = 1:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw32:End Sub			

	
Sub UpperDropsUp(enabled)
	If enabled then
		DelayDropsUp.enabled = true
	End If
End Sub

Dim DDUStep		

Sub DelayDropsUp_Timer()
	Select Case DDUStep
		Case 5:	PlaySoundAt SoundFX("fx_DropTargetUp",DOFTargets),sw12:sw02.isDropped = 0:sw12.isDropped = 0:sw22.isDropped = 0:sw32.isDropped = 0:udtDrop.DropSol_On
		Case 10:DelayDropsUp.Enabled = 0:DDUStep = 0
	End Select
	DDUStep = DDUStep + 1
End Sub

Sub CheckDropShadows()
'	'gion target dropped
	If LampState(100) = 1 and sw02.isdropped = 1 then 
		gi5at02.state = 1
		gi7at02.state = 1  
	Else
		gi5at02.state = 0
		gi7at02.state = 0 
	End If

	If LampState(100) = 1 and sw12.isdropped = 1 then 
		gi5at12.state = 1
		gi7at12.state = 1  
	Else
		gi5at12.state = 0
		gi7at12.state = 0 
	End If

	If LampState(100) = 1 and sw22.isdropped = 1 then 
		gi5at22.state = 1
		gi7at22.state = 1  
	Else
		gi5at22.state = 0
		gi7at22.state = 0
	End If

	If LampState(100) = 1 and sw32.isdropped = 1 then
		gi5at32.state = 1
		gi7at32.state = 1  
	Else
		gi5at32.state = 0
		gi7at32.state = 0
	End If
End Sub


'''''Standups

Dim Target03Step, Target13Step, Target23Step, Target33Step


Sub sw03_Hit:vpmTimer.PulseSw(03):psw03.TransX = -5:Target03Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw03:End Sub			
Sub sw03_timer()
	Select Case Target03Step
		Case 1:psw03.TransX = 3
        Case 2:psw03.TransX = -2
        Case 3:psw03.TransX = 1
        Case 4:psw03.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target03Step = Target03Step + 1
End Sub


Sub sw13_Hit:vpmTimer.PulseSw(13):psw13.TransX = -5:Target13Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw13:End Sub			
Sub sw13_timer()
	Select Case Target13Step
		Case 1:psw13.TransX = 3
        Case 2:psw13.TransX = -2
        Case 3:psw13.TransX = 1
        Case 4:psw13.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target13Step = Target13Step + 1
End Sub

Sub sw23_Hit:vpmTimer.PulseSw(23):psw23.TransX = -5:Target23Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw23:End Sub			
Sub sw23_timer()
	Select Case Target23Step
		Case 1:psw23.TransX = 3
        Case 2:psw23.TransX = -2
        Case 3:psw23.TransX = 1
        Case 4:psw23.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target23Step = Target23Step + 1
End Sub

Sub sw33_Hit:vpmTimer.PulseSw(33):psw33.TransX = -5:Target33Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw33:End Sub			
Sub sw33_timer()
	Select Case Target33Step
		Case 1:psw33.TransX = 3
        Case 2:psw33.TransX = -2
        Case 3:psw33.TransX = 1
        Case 4:psw33.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target33Step = Target33Step + 1
End Sub


'Main

'''''Standups

Dim Target05Step, Target15Step, Target55Step


Sub sw05_Hit:vpmTimer.PulseSw(05):psw05.TransX = -5:Target05Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw05:End Sub			
Sub sw05_timer()
	Select Case Target05Step
		Case 1:psw05.TransX = 3
        Case 2:psw05.TransX = -2
        Case 3:psw05.TransX = 1
        Case 4:psw05.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target05Step = Target05Step + 1
End Sub


Sub sw15_Hit:vpmTimer.PulseSw(15):psw15.TransX = -5:Target15Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw15:End Sub			
Sub sw15_timer()
	Select Case Target15Step
		Case 1:psw15.TransX = 3
        Case 2:psw15.TransX = -2
        Case 3:psw15.TransX = 1
        Case 4:psw15.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target15Step = Target15Step + 1
End Sub


Sub sw55_Hit:vpmTimer.PulseSw(55):psw55.TransX = -5:Target55Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw55:End Sub			
Sub sw55_timer()
	Select Case Target55Step
		Case 1:psw55.TransX = 3
        Case 2:psw55.TransX = -2
        Case 3:psw55.TransX = 1
        Case 4:psw55.TransX = 0:Me.TimerEnabled = 0:
     End Select
	Target55Step = Target55Step + 1
End Sub


''''''''Kicking Standups

Sub Tsw14_hit()
	pSW14.ObjRotX = 5:pSW14.TransX = -5
	pSW14b.ObjRotX = 5:pSW14b.TransX = -5
End Sub

Sub Tsw14_unhit()
	pSW14.ObjRotX = 0:pSW14.TransX = 0
	pSW14b.ObjRotX = 0:pSW14b.TransX = 0
End Sub

Sub sw14_slingshot()
	pSW14.ObjRotX = 10:pSW14.TransX = -10
	pSW14b.ObjRotX = 10:pSW14b.TransX = -10
	Playsoundat SoundFX("left_slingshot",DOFContactors),psw14
	vpmTimer.PulseSw(14)
End Sub


Sub Tsw24_hit()
	pSW24.ObjRotX = 5:pSW24.TransX = -5
	pSW24b.ObjRotX = 5:pSW24b.TransX = -5
End Sub

Sub Tsw24_unhit()
	pSW24.ObjRotX = 0:pSW24.TransX = 0
	pSW24b.ObjRotX = 0:pSW24b.TransX = 0
End Sub

Sub sw24_slingshot()
	pSW24.ObjRotX = 10:pSW24.TransX = -10
	pSW24b.ObjRotX = 10:pSW24b.TransX = -10
	Playsoundat SoundFX("left_slingshot",DOFContactors),psw24
	vpmTimer.PulseSw(24)
End Sub

Sub Tsw25_hit()
	pSW25.ObjRotX = 5:pSW25.TransX = -5
	pSW25b.ObjRotX = 5:pSW25b.TransX = -5
End Sub

Sub Tsw25_unhit()
	pSW25.ObjRotX = 0:pSW25.TransX = 0
	pSW25b.ObjRotX = 0:pSW25b.TransX = 0
End Sub

Sub sw25_slingshot()
	pSW25.ObjRotX = 10:pSW25.TransX = -10
	pSW25b.ObjRotX = 10:pSW25b.TransX = -10
	Playsoundat SoundFX("left_slingshot",DOFContactors),psw25
	vpmTimer.PulseSw(25)
End Sub

'Animated Rubbers UPF

Sub UPFAniRubber1_Hit:Rubber28.visible = false:Rubber28a.visible = true:me.timerenabled = True:End Sub

Sub UPFAniRubber1_timer:Rubber28a.visible = false:Rubber28.visible = true:me.timerenabled = True: End Sub

Sub UPFAniRubber2_Hit:Rubber28.visible = false:Rubber28b.visible = true:me.timerenabled = True:End Sub

Sub UPFAniRubber2_timer:Rubber28b.visible = false:Rubber28.visible = true:me.timerenabled = True: End Sub

Sub UPFAniRubber3_Hit:Rubber28.visible = false:Rubber28c.visible = true:me.timerenabled = True:End Sub

Sub UPFAniRubber3_timer:Rubber28c.visible = false:Rubber28.visible = true:me.timerenabled = True: End Sub

Sub UPFAniRubber4_Hit:Rubber29.visible = false:Rubber29a.visible = true:me.timerenabled = True:End Sub

Sub UPFAniRubber4_timer:Rubber29a.visible = false:Rubber29.visible = true:me.timerenabled = True: End Sub

Sub UPFAniRubber5_Hit:Rubber30.visible = false:Rubber30a.visible = true:me.timerenabled = True:End Sub

Sub UPFAniRubber5_timer:Rubber30a.visible = false:Rubber30.visible = true:me.timerenabled = True: End Sub





'Scoring Rubbers

''upper

Sub ScorringRubber1_Hit:vpmTimer.PulseSw(42):End Sub

Sub ScorringRubber2_Hit:vpmTimer.PulseSw(42):End Sub

''Lower

Sub ScorringRubber4_Hit:vpmTimer.PulseSw(51):End Sub

Sub ScorringRubber5_Hit:vpmTimer.PulseSw(51):Rubber23.visible = false:Rubber24.visible = true:me.timerenabled = True:End Sub

Sub ScorringRubber5_timer:Rubber24.visible = false:Rubber23.visible = true:me.timerenabled = True: End Sub

'Slingshots
''Upper
Sub UpperSlingShot_Slingshot:UpperSlingshota.visible = false:pSlingU.TransZ = -8:UpperSlingshotb.visible = true:PlaySoundAt SoundFX("Right_slingshot",DOFContactors),pSlingU:vpmTimer.PulseSw 66:UpperSlingshotStep = 0:Me.TimerEnabled = 1:DOF 104, DOFPulse:End Sub
Sub UpperSlingshot_Timer
    Select Case UpperSlingshotStep
        Case 0:UpperSlingshotb.visible = false:pSlingU.TransZ = -16:UpperSlingshotc.visible = true
        Case 1:UpperSlingshotc.visible = false:pSlingU.TransZ = -24:UpperSlingshotd.visible = true
        Case 2:UpperSlingshotd.visible = false:pSlingU.TransZ = -16:UpperSlingshotc.visible = true
        Case 3:UpperSlingshotc.visible = false:pSlingU.TransZ = -8:UpperSlingshotb.visible = true
        Case 4:UpperSlingshotb.visible = false:pSlingU.TransZ = 0:UpperSlingshota.visible = true:Me.TimerEnabled = 0 '
    End Select

    UpperSlingshotStep = UpperSlingshotStep + 1
End Sub

''Lower
Dim LowerSlingshotStep, UpperSlingShotStep

Sub LowerSlingShot_Slingshot:LowerSlingshota.visible = false:pSlingL.TransZ = -8:LowerSlingshotb.visible = true:PlaySoundAt SoundFX("left_slingshot",DOFContactors),pSlingL:vpmTimer.PulseSw 51:LowerSlingshotStep = 0:Me.TimerEnabled = 1:DOF 102, DOFPulse:End Sub
Sub LowerSlingshot_Timer
    Select Case LowerSlingshotStep
        Case 0:LowerSlingshotb.visible = false:pSlingL.TransZ = -16:LowerSlingshotc.visible = true
        Case 1:LowerSlingshotc.visible = false:pSlingL.TransZ = -24:LowerSlingshotd.visible = true
        Case 2:LowerSlingshotd.visible = false:pSlingL.TransZ = -16:LowerSlingshotc.visible = true
        Case 3:LowerSlingshotc.visible = false:pSlingL.TransZ = -8:LowerSlingshotb.visible = true
        Case 4:LowerSlingshotb.visible = false:pSlingL.TransZ = 0:LowerSlingshota.visible = true:Me.TimerEnabled = 0 '
    End Select

    LowerSlingshotStep = LowerSlingshotStep + 1
End Sub


' Drop Targets

Sub sw00_Hit:ldtDrop.Hit 1:sw00.isDropped = true:psw00.Z = 220:me.TimerEnabled = True:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw00:End Sub			
Sub sw00_timer:psw00.Z = 195:me.TimerEnabled = False: End Sub

Sub sw10_Hit:ldtDrop.Hit 2:sw10.isDropped = true:psw10.Z = 215:me.TimerEnabled = True:PlaySoundAt SoundFX("dfx_DropTargetDown",DOFTargets),sw10:End Sub			
Sub sw10_timer:psw10.Z = 190:me.TimerEnabled = False: End Sub

Sub sw20_Hit:ldtDrop.Hit 3:sw20.isDropped = true:psw20.Z = 210:me.TimerEnabled = True:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw20:End Sub			
Sub sw20_timer:psw20.Z = 185:me.TimerEnabled = False: End Sub

Sub sw30_Hit:ldtDrop.Hit 4:sw30.isDropped = true:psw30.Z = 205:me.TimerEnabled = True:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw30:End Sub	
Sub sw30_timer:psw30.Z = 180:me.TimerEnabled = False: End Sub

Sub sw40_Hit:ldtDrop.Hit 5:sw40.isDropped = true:psw40.Z = 200:me.TimerEnabled = True:PlaySoundAt SoundFX("fx_DropTargetDown",DOFTargets),sw40:End Sub			
Sub sw40_timer:psw40.Z = 175:me.TimerEnabled = False: End Sub
	
Sub LowerDropsUp()
	DDU2Step = 0
	DelayDropsUp2.Enabled = 1
End Sub
	
Dim DDU2Step	

Sub DelayDropsUp2_Timer()
	Select Case DDU2Step
		Case 5:	PlaySoundAt SoundFX("fx_DropTargetUp",DOFTargets),sw20:sw00.isDropped = False:psw00.Z = 250:sw10.isDropped = False:psw10.Z = 245:sw20.isDropped = False:psw20.Z = 240:sw30.isDropped = False:psw30.Z = 235:sw40.isDropped = False:psw40.Z = 230:ldtDrop.DropSol_On
		Case 10:DelayDropsUp2.Enabled = 0:DDU2Step = 0
	End Select
	DDU2Step = DDU2Step + 1
End Sub


' Standups

Dim Target50Step, Target60Step


Sub sw50_Hit:vpmTimer.PulseSw(50):Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw50:End Sub			
Sub sw50_timer()
	Select Case Target50Step
		Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
     End Select
	Target50Step = Target50Step + 1
End Sub

Sub sw60_Hit:vpmTimer.PulseSw(60):Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw60:End Sub			
Sub sw60_timer()
	Select Case Target60Step
		Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
     End Select
	Target60Step = Target60Step + 1
End Sub

'  Kicking target

Sub Tsw11_hit()
	pSW11.ObjRotX = 10:pSW11.TransX = -10
End Sub

Sub Tsw11_unhit()
	pSW11.ObjRotX = 0:pSW11.TransX = 0
End Sub

Sub sw11_slingshot()
	Playsoundat SoundFX("Right_slingshot",DOFContactors),pSW11
	vpmTimer.PulseSw(11)
End Sub

'''''''''''''''''''''''''''''''
''''''''''Bumpers
'''''''''''''''''''''''''''''''

Sub Bumper2_hit() 'Main Top
	vpmTimer.PulseSw(44)
	PlaySoundAtBumperVol SoundFX("fx_bumper2",DOFContactors),Bumper2,1
End Sub

Sub Bumper1_hit() 'Main Bottom
	vpmTimer.PulseSw(44)
	PlaySoundAtBumperVol SoundFX("fx_bumper1",DOFContactors),Bumper1,1
End Sub

Sub Bumper3_hit() 'Upper
	vpmTimer.PulseSw(43)
	PlaySoundAtBumperVol SoundFX("fx_bumper3",DOFContactors),Bumper3,1
End Sub

Sub Bumper4_hit() 'Lower
	vpmTimer.PulseSw(1)
	PlaySoundAtBumperVol SoundFX("fx_bumper4",DOFContactors),Bumper4,1
End Sub


''''Knocker

Sub PlayKnocker(Enabled)
	DelayKnocker.enabled = 1
End Sub

Dim DelayKnockerStep

Sub DelayKnocker_timer()
	Select Case DelayKnockerStep
		Case 0:
		Case 1:	Playsoundat SoundFX("Knocker",DOFKnocker),Plunger
		Case 2:
		Case 3:
		Case 4: 
		Case 5:	
		Case 6:
		Case 7:
		Case 8:
		Case 9:
		Case 20:DelayKnocker.Enabled = 0:DelayKnockerStep = 0
	End Select
	DelayKnockerStep = DelayKnockerStep + 1
End Sub


''''TrapDoor

Dim TrapDoorDir
Const TrapDoorMin = 0
Const TrapDoorMax = 30


Sub TrapDoor_timer ()
	pTrapDoor.RotZ = pTrapDoor.RotZ + TrapDoorDir
	If pTrapDoor.RotZ = TrapDoorMax then
		TrapDoor.TimerEnabled = 0
		pTrapDoor.RotZ = TrapDoorMax
	End If
	If pTrapDoor.RotZ = TrapDoorMin then
		TrapDoor.TimerEnabled = 0
		pTrapDoor.RotZ = TrapDoorMin
	End If
End Sub


'''''''''''  Reflected balls on Metal Cage

Sub TriggerRelection0_hit()
	Flasher25.ImageA = "hhmetalsframe2"
	Flasher25.ImageB = "hhmetalsframe2"
End Sub
Sub TriggerRelection1_hit()
	Select Case BallMod
		Case 0:Flasher25.ImageA = "haunted house metals frame2a":Flasher25.ImageB = "haunted house metals frame2a"
		Case 1:Flasher25.ImageA = "haunted house metals frame2a-m":Flasher25.ImageB = "haunted house metals frame2a-m"
		Case 2:Flasher25.ImageA = "haunted house metals frame2a-my":Flasher25.ImageB = "haunted house metals frame2a-my"
	End Select
End Sub
Sub TriggerRelection2_hit()
	Select Case BallMod
		Case 0:Flasher25.ImageA = "haunted house metals frame2b":Flasher25.ImageB = "haunted house metals frame2b"
		Case 1:Flasher25.ImageA = "haunted house metals frame2b-m":Flasher25.ImageB = "haunted house metals frame2b-m"
		Case 2:Flasher25.ImageA = "haunted house metals frame2b-my":Flasher25.ImageB = "haunted house metals frame2b-my"
	End Select
End Sub
Sub TriggerRelection3_hit()
	Select Case BallMod
		Case 0:Flasher25.ImageA = "haunted house metals frame2c":Flasher25.ImageB = "haunted house metals frame2c"
		Case 1:Flasher25.ImageA = "haunted house metals frame2c-m":Flasher25.ImageB = "haunted house metals frame2c-m"
		Case 2:Flasher25.ImageA = "haunted house metals frame2c-my":Flasher25.ImageB = "haunted house metals frame2c-my"
	End Select
End Sub
Sub TriggerRelection4_hit()
	Select Case BallMod
		Case 0:Flasher25.ImageA = "haunted house metals frame2d":Flasher25.ImageB = "haunted house metals frame2d"
		Case 1:Flasher25.ImageA = "haunted house metals frame2d-m":Flasher25.ImageB = "haunted house metals frame2d-m"
		Case 2:Flasher25.ImageA = "haunted house metals frame2d-my":Flasher25.ImageB = "haunted house metals frame2d-my"
	End Select
End Sub
Sub TriggerRelection5_hit()
	Select Case BallMod
		Case 0:Flasher25.ImageA = "haunted house metals frame2e":Flasher25.ImageB = "haunted house metals frame2e"
		Case 1:Flasher25.ImageA = "haunted house metals frame2e-m":Flasher25.ImageB = "haunted house metals frame2e-m"
		Case 2:Flasher25.ImageA = "haunted house metals frame2e-my":Flasher25.ImageB = "haunted house metals frame2e-my"
	End Select
End Sub
Sub TriggerRelection6_hit()
	Select Case BallMod
		Case 0:Flasher25.ImageA = "haunted house metals frame2f":Flasher25.ImageB = "haunted house metals frame2f"
		Case 1:Flasher25.ImageA = "haunted house metals frame2f-m":Flasher25.ImageB = "haunted house metals frame2f-m"
		Case 2:Flasher25.ImageA = "haunted house metals frame2f-my":Flasher25.ImageB = "haunted house metals frame2f-my"
	End Select
End Sub
Sub TriggerRelection7_hit()
	Flasher25.ImageA = "hhmetalsframe2"
	Flasher25.ImageB = "hhmetalsframe2"
End Sub



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Through system''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim BallCount, cBall1,cBall2

Sub CheckMaxBalls()
	BallCount = MaxBalls
End Sub

Sub CreatBalls()
	If BallCount > 0 then
		Set cBall1 = Drain.CreateSizedBallWithMass(BallRadius, BallMass)
		Drain.kick 80,20
		BallCount = BallCount - 1
	If BallMod = 1 Then
		cBall1.Image = "Chrome_Ball_29"	
		cBall1.FrontDecal = "marbledball RedGreen"
	End If
	If BallMod = 2 Then
		cBall1.Image = "Chrome_Ball_29"	
		cBall1.FrontDecal = "marbledball YellowGreen"
	End If
	End If
End Sub	
	
dim bstatus

Sub CheckBallStatus_timer()

	Select Case Bstatus
	Case 1:	If Kicker1active = 0 and Kicker2active = 1 then Kicker2.Kick 70,10 End If
	Case 2: If Kicker2active = 0 and Kicker3active = 1 then Kicker3.Kick 70,10 End If
	Case 3: If Kicker3active = 0 and Kicker4active = 1 then Kicker4.Kick 70,10 End If
	Case 4: If Kicker4active = 0 and Kicker5active = 1 then Kicker5.Kick 70,10 End If
	Case 5: If Kicker5active = 0 and Kicker6active = 1 then Kicker6.Kick 70,10 End If
	Case 6: bstatus = 0
	End Select
	bstatus = bstatus + 1
End Sub



Dim Kicker1active, Kicker2active, Kicker3active, Kicker4active, Kicker5active, Kicker6active


Sub Kicker1_hit()
	Kicker1active = 1
	controller.switch(67) = 1
End Sub

Sub Kicker1_unhit()

End Sub

Sub Kicker2_hit()
	Kicker2active = 1
End Sub

Sub Kicker2_unhit()
	Kicker2active = 0
End Sub

Sub Kicker3_hit()
	Kicker3active = 1
End Sub

Sub Kicker3_unhit()
	Kicker3active = 0
End Sub

Sub Kicker4_hit()
	Kicker4active = 1
End Sub

Sub Kicker4_unhit()
	Kicker4active = 0
End Sub

Sub Kicker5_hit()
	Kicker5active = 1
End Sub

Sub Kicker5_unhit()
	Kicker5active = 0
End Sub

Sub Kicker6_hit()
	Kicker6active = 1
	controller.switch(67) = 1
End Sub

Sub Kicker6_unhit()
	Kicker6active = 0
		If BallCount > 0 then
			CreatBalls
		End If
End Sub

dim DontKickAnyMoreBalls

Sub KickBallToLane(Enabled)
	If DontKickAnyMoreBalls = 0 then
		DontKickAnyMoreBalls = 1
		PlaySound SoundFX("BallRelease",DOFContactors)
		Kicker1.Kick 90,12
		bstatus = 2
		Kicker1active = 0
		iBall = iBall - 1
		fgBall = false
		UpperGIon = 1
		controller.switch(67) = 0
		DKTMstep = 1
		DontKickToMany.enabled = true
		BallsInPlay = BallsInPlay + 1
	End If
End Sub

Dim DKTMstep

Sub DontKickToMany_timer ()
	Select Case DKTMstep
	Case 1:
	Case 2:
	Case 15: DontKickAnyMoreBalls = 0:DontKickToMany.Enabled = False: DontKickAnyMoreBalls = 0
	End Select
	DKTMstep = DKTMstep + 1
End Sub


sub kisort(enabled)
	if fgBall then
		Drain.Kick 80,20
		iBall = iBall + 1
		fgBall = false

	end if

end sub


Sub Drain_hit()
	PlaySound SoundFX("drain",DOFContactors)
	Drain.Kick 80,20
	fgBall = true
	iBall = iBall + 1
	BallsInPlay = BallsInPlay - 1
End Sub
	

'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'			LL		EEEEEE	DDDD		,,	 SSSSS
'      		LL		EE		DD  DD		,,	SS
'			LL		EE		DD   DD		 ,	 SS
'			LL		EEEE	DD   DD			   SS
'			LL		EE		DD  DD			    SS
'			LLLLLL  EEEEEE	DDDD			SSSSS
'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
'		6 Didget Array
'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

 Dim LED(33)
LED(0)=Array(d121,d122,d123,d124,d125,d126,d127,LXM,d128)
LED(1)=Array(d131,d132,d133,d134,d135,d136,d137,LXM,d138)
LED(2)=Array(d141,d142,d143,d144,d145,d146,d147,LXM,d148)
LED(3)=Array(d151,d152,d153,d154,d155,d156,d157,LXM,d158)
LED(4)=Array(d161,d162,d163,d164,d165,d166,d167,LXM,d168)
LED(5)=Array(d171,d172,d173,d174,d175,d176,d177,LXM,d178)

LED(6)=Array(d221,d222,d223,d224,d225,d226,d227,LXM,d228)
LED(7)=Array(d231,d232,d233,d234,d235,d236,d237,LXM,d238)
LED(8)=Array(d241,d242,d243,d244,d245,d246,d247,LXM,d248)
LED(9)=Array(d251,d252,d253,d254,d255,d256,d257,LXM,d258)
LED(10)=Array(d261,d262,d263,d264,d265,d266,d267,LXM,d268)
LED(11)=Array(d271,d272,d273,d274,d275,d276,d277,LXM,d278)

LED(12)=Array(d321a,d322a,d323a,d324a,d325a,d326a,d327a,LXM,d328a)
LED(13)=Array(d331a,d332a,d333a,d334a,d335a,d336a,d337a,LXM,d338a)
LED(14)=Array(d341,d342,d343,d344,d345,d346,d347,LXM,d348)
LED(15)=Array(d351,d352,d353,d354,d355,d356,d357,LXM,d358)
LED(16)=Array(d361,d362,d363,d364,d365,d366,d367,LXM,d368)
LED(17)=Array(d371,d372,d373,d374,d375,d376,d377,LXM,d378)

LED(18)=Array(d421,d422,d423,d424,d425,d426,d427,LXM,d428)
LED(19)=Array(d431,d432,d433,d434,d435,d436,d437,LXM,d438)
LED(20)=Array(d441,d442,d443,d444,d445,d446,d447,LXM,d448)		
LED(21)=Array(d451,d452,d453,d454,d455,d456,d457,LXM,d458)		
LED(22)=Array(d461,d462,d463,d464,d465,d466,d467,LXM,d468)		
LED(23)=Array(d471,d472,d473,d474,d475,d476,d477,LXM,d478)	


LED(24)=Array(d511,d512,d513,d514,d515,d516,d517,LXM,d518)		
LED(25)=Array(d521,d522,d523,d524,d525,d526,d527,LXM,d528)	
LED(26)=Array(d611,d612,d613,d614,d615,d616,d617,LXM,d618)		
LED(27)=Array(d621,d622,d623,d624,d625,d626,d627,LXM,d628)	


LED(28)=Array(D281,D282,D283,D284,D285,D286,D287,LXM,D288)		
LED(29)=Array(D291,D292,D293,D294,D295,D296,D297,LXM,D298)		
LED(30)=Array(D301,D302,D303,D304,D305,D306,D307,LXM,D308)		
LED(31)=Array(D311,D312,D313,D314,D315,D316,D317,LXM,D318)		
LED(32)=Array(D321,D322,D323,D324,D325,D326,D327,LXM,D328)		
LED(33)=Array(D331,D332,D333,D334,D335,D336,D337,LXM,D338)		



Sub DisplayTimer6_Timer
Dim ChgLED, ii, num, chg, stat, obj
ChgLED = Controller.ChangedLEDs (&Hffffffff, &Hffffffff)
If Not IsEmpty (ChgLED) Then
For ii = 0 To UBound (chgLED)
num = chgLED (ii, 0) : chg = chgLED (ii, 1) : stat = chgLED (ii, 2)
For Each obj In LED (num)
If chg And 1 Then obj.State = stat And 1
chg = chg \ 2 : stat = stat \ 2
Next
Next
End If
End Sub


'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
'		7 Didget Array
'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Dim LED7(38)
LED7(0)=Array(d111,d112,d113,d114,d115,d116,d117,LXM,d118)
LED7(1)=Array(d121,d122,d123,d124,d125,d126,d127,LXM,d128)
LED7(2)=Array(d131,d132,d133,d134,d135,d136,d137,LXM,d138)
LED7(3)=Array(d141,d142,d143,d144,d145,d146,d147,LXM,d148)
LED7(4)=Array(d151,d152,d153,d154,d155,d156,d157,LXM,d158)
LED7(5)=Array(d161,d162,d163,d164,d165,d166,d167,LXM,d168)
LED7(6)=Array(d171,d172,d173,d174,d175,d176,d177,LXM,d178)

LED7(7)=Array(d211,d212,d213,d214,d215,d216,d217,LXM,d218)
LED7(8)=Array(d221,d222,d223,d224,d225,d226,d227,LXM,d228)
LED7(9)=Array(d231,d232,d233,d234,d235,d236,d237,LXM,d238)
LED7(10)=Array(d241,d242,d243,d244,d245,d246,d247,LXM,d248)
LED7(11)=Array(d251,d252,d253,d254,d255,d256,d257,LXM,d258)
LED7(12)=Array(d261,d262,d263,d264,d265,d266,d267,LXM,d268)
LED7(13)=Array(d271,d272,d273,d274,d275,d276,d277,LXM,d278)

LED7(14)=Array(d311a,d312a,d313a,d314a,d315a,d316a,d317a,LXM,d318a)
LED7(15)=Array(d321a,d322a,d323a,d324a,d325a,d326a,d327a,LXM,d328a)
LED7(16)=Array(d331a,d332a,d333a,d334a,d335a,d336a,d337a,LXM,d338a)
LED7(17)=Array(d341,d342,d343,d344,d345,d346,d347,LXM,d348)
LED7(18)=Array(d351,d352,d353,d354,d355,d356,d357,LXM,d358)
LED7(19)=Array(d361,d362,d363,d364,d365,d366,d367,LXM,d368)
LED7(20)=Array(d371,d372,d373,d374,d375,d376,d377,LXM,d378)

LED7(21)=Array(d411,d412,d413,d414,d415,d416,d417,LXM,d418)
LED7(22)=Array(d421,d422,d423,d424,d425,d426,d427,LXM,d428)
LED7(23)=Array(d431,d432,d433,d434,d435,d436,d437,LXM,d438)
LED7(24)=Array(d441,d442,d443,d444,d445,d446,d447,LXM,d448)		
LED7(25)=Array(d451,d452,d453,d454,d455,d456,d457,LXM,d458)		
LED7(26)=Array(d461,d462,d463,d464,d465,d466,d467,LXM,d468)		
LED7(27)=Array(d471,d472,d473,d474,d475,d476,d477,LXM,d478)	

LED7(28)=Array(d511,d512,d513,d514,d515,d516,d517,LXM,d518)		'was 24 -- 26
LED7(29)=Array(d521,d522,d523,d524,d525,d526,d527,LXM,d528)		'was 25 -- 27
LED7(30)=Array(d611,d612,d613,d614,d615,d616,d617,LXM,d618)		'was 26 -- 24
LED7(31)=Array(d621,d622,d623,d624,d625,d626,d627,LXM,d628)		'was 27 -- 25

LED7(32)=Array(D281,D282,D283,D284,D285,D286,D287,LXM,D288)		
LED7(33)=Array(D291,D292,D293,D294,D295,D296,D297,LXM,D298)		
LED7(34)=Array(D301,D302,D303,D304,D305,D306,D307,LXM,D308)		
LED7(35)=Array(D311,D312,D313,D314,D315,D316,D317,LXM,D318)		
LED7(36)=Array(D321,D322,D323,D324,D325,D326,D327,LXM,D328)		
LED7(37)=Array(D331,D332,D333,D334,D335,D336,D337,LXM,D338)	

Sub DisplayTimer7_Timer
Dim ChgLED, ii, num, chg, stat, obj
ChgLED = Controller.ChangedLEDs (&Hffffffff, &Hffffffff)
If Not IsEmpty (ChgLED) Then
For ii = 0 To UBound (chgLED)
num = chgLED (ii, 0) : chg = chgLED (ii, 1) : stat = chgLED (ii, 2)
For Each obj In LED7 (num)
If chg And 1 Then obj.State = stat And 1
chg = chg \ 2 : stat = stat \ 2
Next
Next
End If
End Sub




Dim Digits(34)

'Digits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6, n1, LED1x8)
Digits(0) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6, n1, LED2x8)
Digits(1) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6, n1, LED3x8)
Digits(2) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6, n1, LED4x8)
Digits(3) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6, n1, LED5x8)
Digits(4) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6, n1, LED6x8)
Digits(5) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6, n1, LED7x8)


'Digits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6, n1, LED8x8)
Digits(6) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6, n1, LED9x8)
Digits(7) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6, n1, LED10x8)
Digits(8) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6, n1, LED11x8)
Digits(9) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6, n1, LED12x8)
Digits(10) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6, n1, LED13x8)
Digits(11) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6, n1, LED14x8)

'Digits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006, n1, LED1x008)
Digits(12) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106, n1, LED1x108)
Digits(13) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206, n1, LED1x208)
Digits(14) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306, n1, LED1x308)
Digits(15) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406, n1, LED1x408)
Digits(16) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506, n1, LED1x508)
Digits(17) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606, n1, LED1x608)


'Digits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006, n1, LED2x008)
Digits(18) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106, n1, LED2x108)
Digits(19) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206, n1, LED2x208)
Digits(20) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306, n1, LED2x308)
Digits(21) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406, n1, LED2x408)
Digits(22) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506, n1, LED2x508)
Digits(23) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606, n1, LED2x608)

Digits(24) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306, n1, LEDax308)
Digits(25) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406, n1, LEDbx408)

Digits(26) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506, n1, LEDcx508)
Digits(27) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606, n1, LEDdx608)

Digits(28)=Array(D281,D282,D283,D284,D285,D286,D287,LXM,D288)		
Digits(29)=Array(D291,D292,D293,D294,D295,D296,D297,LXM,D298)		
Digits(30)=Array(D301,D302,D303,D304,D305,D306,D307,LXM,D308)		
Digits(31)=Array(D311,D312,D313,D314,D315,D316,D317,LXM,D318)		
Digits(32)=Array(D321,D322,D323,D324,D325,D326,D327,LXM,D328)		
Digits(33)=Array(D331,D332,D333,D334,D335,D336,D337,LXM,D338)		



Sub DisplayTimerFSS6_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		'If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 28) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.visible = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
				For Each obj In Digits(num)
					If chg And 1 Then obj.state = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			end if
		next
		'end if
end if
End Sub


Dim Digits7(38)

Digits7(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6, n1, LED1x8)
Digits7(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6, n1, LED2x8)
Digits7(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6, n1, LED3x8)
Digits7(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6, n1, LED4x8)
Digits7(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6, n1, LED5x8)
Digits7(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6, n1, LED6x8)
Digits7(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6, n1, LED7x8)


Digits7(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6, n1, LED8x8)
Digits7(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6, n1, LED9x8)
Digits7(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6, n1, LED10x8)
Digits7(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6, n1, LED11x8)
Digits7(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6, n1, LED12x8)
Digits7(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6, n1, LED13x8)
Digits7(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6, n1, LED14x8)

Digits7(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006, n1, LED1x008)
Digits7(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106, n1, LED1x108)
Digits7(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206, n1, LED1x208)
Digits7(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306, n1, LED1x308)
Digits7(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406, n1, LED1x408)
Digits7(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506, n1, LED1x508)
Digits7(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606, n1, LED1x608)


Digits7(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006, n1, LED2x008)
Digits7(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106, n1, LED2x108)
Digits7(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206, n1, LED2x208)
Digits7(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306, n1, LED2x308)
Digits7(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406, n1, LED2x408)
Digits7(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506, n1, LED2x508)
Digits7(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606, n1, LED2x608)

Digits7(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306, n1, LEDax308)
Digits7(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406, n1, LEDbx408)

Digits7(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506, n1, LEDcx508)
Digits7(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606, n1, LEDdx608)

Digits7(32)=Array(D281,D282,D283,D284,D285,D286,D287,LXM,D288)		
Digits7(33)=Array(D291,D292,D293,D294,D295,D296,D297,LXM,D298)		
Digits7(34)=Array(D301,D302,D303,D304,D305,D306,D307,LXM,D308)		
Digits7(35)=Array(D311,D312,D313,D314,D315,D316,D317,LXM,D318)		
Digits7(36)=Array(D321,D322,D323,D324,D325,D326,D327,LXM,D328)		
Digits7(37)=Array(D331,D332,D333,D334,D335,D336,D337,LXM,D338)		


Sub DisplayTimerFSS7_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		'If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits7(num)
					If chg And 1 Then obj.visible = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
				For Each obj In Digits7(num)
					If chg And 1 Then obj.state = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			end if
		next
		'end if
end if
End Sub



'***************************************************
'  JP's Fading Lamps & Flashers version 9 for VP921
'   Based on PD's Fading Lights
' SetLamp 0 is Off
' SetLamp 1 is On
' FadingLevel(x) = fading state
' LampState(x) = light state
' Includes the flash element (needs own timer)
' Flashers can be used as lights too
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image")
Dim TextureRedBulbArray: TextureRedBulbArray = Array("Bulb Red trans", "Bulb Red")
Dim TextureBlueBulbArray: TextureBlueBulbArray = Array("Bulb Blue trans", "Bulb Blue")
Dim TextureGreenBulbArray: TextureGreenBulbArray = Array("Bulb Green trans", "Bulb Green")
Dim TextureYellowBulbArray: TextureYellowBulbArray = Array("Bulb Yellow trans", "Bulb Yellow")
Dim BulbArray1: BulbArray1 = Array("Bulb_on_texture", "Bulb_texture")
Dim LightWhite1: LightWhite1 = Array("light_white1_on", "light_white1_66", "light_white1_33", "light_white1_off")
Dim LightWhite2: LightWhite2 = Array("light_white2_on", "light_white2_66", "light_white2_33", "light_white2_off")
Dim LightWhite3: LightWhite3 = Array("light_white3_on", "light_white3_66", "light_white3_33", "light_white3_off")
Dim LightGreen: LightGreen = Array("light_Green_on", "light_Green_66", "light_Green_33", "light_Green_off")
Dim LightGreen2: LightGreen2 = Array("light_Green2_on", "light_Green2_66", "light_Green2_33", "light_Green2_off")
Dim LightRed: LightRed = Array("light_Red_on", "light_Red_66", "light_Red_33", "light_Red_off")
Dim LightOrange: LightOrange = Array("light_Orange_on", "light_Orange_66", "light_Orange_33", "light_Orange_off")
Dim BlueLight: BlueLight = Array("Bulb_Blue_texture_on", "Bulb_Blue_texture_66", "Bulb_Blue_texture_33", "Bulb_Blue_texture_off")


Const LightHaloBrightness		= 200


FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1

' Lamp & Flasher Timers

Sub StartLampTimer
	AllLampsOff()
	LampTimer.Interval = 30 'lamp fading speed
	LampTimer.Enabled = 1
End Sub

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
			FlashState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
        Next
    End If

    UpdateLamps
	CheckDropShadows
	UpdateMultipleLamps

End Sub
 

Sub UpdateLamps

	NFadeL 3, L3
'***** START FSS BACKGLASS *****
NFadeLm 4, L4
Flashm 4, FlLi4a
Flash 4, FlLi4b
'**** END FSS BACKGLASS *****
	NFadeL 8, L8
'***** START FSS BACKGLASS *****
NFadeLm 12, L12 
Flashm 12,FlLi12b
Flashm 12,FlLi12a 
Flashm 12,FlLi12a1
Flashm 12,FlLi12a2
Flashm 12,FlLi12a3
Flashm 12,FlLi12a4
Flashm 12,FlLi12a5
Flashm 12,FlLi12a6
Flashm 12,FlLi12a7
Flashm 12,FlLi12a8
Flashm 12,FlLi12a9
Flashm 12,FlLi12a10
Flashm 12,FlLi12a11
Flashm 12,FlLi12a12
Flashm 12,FlLi12a13
Flashm 12,FlLi12a14
Flashm 12,FlLi12a15
Flashm 12,FlLi12a16
Flashm 12,FlLi12a17
Flashm 12,FlLi12a18
Flashm 12,FlLi12a19
Flashm 12,FlLi12a20
Flashm 12,FlLi12b1
Flash 12,FlLi12b2
'**** END FSS BACKGLASS *****

	NFadeL 100, L2
	NFadeL 18, L18
	NFadeL 19, L19
	NFadeL 20, L20
	NFadeLm 21, L21b
	NFadeLm 21, L21_3
	NFadeLm 21, L21_2
	NFadeL 21, L21_1
	NFadeL 22, L22
	NFadeL 27, L27
	NFadeL 28, L28
	NFadeL 29, L29
	NFadeL 30, L30
	NFadeL 31, L31
	Flash 32, FlasherL32
	NFadeL 32, L32
	Flash 33, FlasherL33
	NFadeL 33, L33
	NFadeL 34, L34
	NFadeL 35, L35
	NFadeL 36, L36
	NFadeL 37, L37
	NFadeL 38, L38
	NFadeL 39, L39
	NFadeL 40, L40
	NFadeL 41, L41
	NFadeL 42, L42
	NFadeL 44, L44
	NFadeL 46, L46
	NFadeL 47, L47
	Flash 51, FlasherL51
	NFadeL 51, L51


	FadeFlash 23, L23, LightOrange
	FadeFlash 24, L24, LightWhite1
	FadeFlashm 25, L25b, LightGreen
	FadeFlash 25, L25, LightWhite2
	Flash 25, FlLi25 '*** FSS BACKGLASS ***
	FadeFlashm 26, L26b, LightGreen2
	FadeFlash 26, L26, LightWhite3
	FadeFlash 45, L45, LightRed

	Flash2 18, Flpvl18
	Flash2 19, Flpvl19
	Flash2 20, Flpvl20
	Flash2 32, Flpvl32
	Flash2 33, Flpvl33
	Flash2 35, Flpvl35
	Flash2 36, Flpvl36
	Flash2 37, Flpvl37
	Flash2 38, Flpvl38
	Flash2 39, Flpvl39
	Flash2 51, Flpvl51

'
''''''GI
'
If LampState(111) = 1 then: SetGIOff: end if

If LampState(111) = 0 then: SetGIOn: end if

End Sub


Sub SolGi(Enabled)
    Dim obj
    If Enabled Then
	stop_sequencer() 'STOP FSS BACKGLASS ANIM 
SetLamp 111, 1
		Playsound "fx_relay_off"
    Else
	start_sequencer() 'START FSS BACKGLASS ANIM 
SetLamp 111, 0
		Playsound "fx_relay_on"
    End If
End Sub


dim GION

Sub SetGIOn()
	hhbghigh.visible = 1
	hhbghigh1.visible = 1
End Sub


Sub SetGIOff()
	hhbghigh.visible = 0
	hhbghigh1.visible = 0
End Sub



'Sindbad: You can use this instead of FadeLN
' call it this way: FadeLight lampnumber, light, Array
Sub FadeLight(nr, Light, group)
    Select Case FadingLevel(nr)
        Case 2:Light.image = group(3):FadingLevel(nr) = 0 'Off
        Case 3:Light.image = group(2):FadingLevel(nr) = 2 'fading...
        Case 4:Light.image = group(1):FadingLevel(nr) = 3 'fading...
        Case 5:Light.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub


Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

'cyberpez FadeFlash can be used to swap images on ramps or flashers

Sub FadeFlash(nr, light, group)
	Select Case FadingLevel(nr)
		Case 2:light.imageA = group(3):FadingLevel(nr) = 0
		Case 3:light.imageA = group(2):FadingLevel(nr) = 2
		Case 4:light.imageA = group(1):FadingLevel(nr) = 3
		Case 5:light.imageA = group(0):FadingLevel(nr) = 1
	End Select
End Sub

Sub FadeFlashm(nr, light, group)
	Select Case FadingLevel(nr)
		Case 2:light.imageA = group(3):
		Case 3:light.imageA = group(2):
		Case 4:light.imageA = group(1):
		Case 5:light.imageA = group(0):
	End Select
End Sub


'trxture swap	
dim itemw, itemp

Sub FadeMaterialW(nr, itemw, group)
    Select Case FadingLevel(nr)
        Case 4:itemw.TopMaterial = group(1):itemw.SideMaterial = group(1)
        Case 5:itemw.TopMaterial = group(0):itemw.SideMaterial = group(0)
    End Select
End Sub


Sub FadeMaterialP(nr, itemp, group)
    Select Case FadingLevel(nr)
        Case 4:itemp.Material = group(1)
        Case 5:itemp.Material = group(0)
    End Select
End Sub



' div lamp subs

Sub AllLampsOff()
    Dim x
    For x = 0 to 200
        LampState(x) = 0
        FadingLevel(x) = 4
    Next

UpdateLamps:UpdateLamps:Updatelamps
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' flasher subs

Sub FlasherTimer_Timer()
		Flashm 111, Flasher1
		Flashm 111, Flasher2
		Flashm 111, Flasher3
		Flashm 111, Flasher4
		Flashm 111, Flasher5
		Flashm 111, Flasher6
		Flashm 111, Flasher7
		Flashm 111, Flasher8
		Flashm 111, Flasher9
		Flashm 111, Flasher10
		Flash 111, Flasher11
End Sub

Sub FlashInit
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
        FlashLevel(i) = 0
    Next

    FlashSpeedUp = 30   ' fast speed when turning on the flasher
    FlashSpeedDown = 50 ' slow speed when turning off the flasher, gives a smooth fading
    ' you could also change the default images for each flasher or leave it as in the editor
    ' for example
    ' Flasher1.Image = "fr"
    AllFlashOff()
End Sub

Sub AllFlashOff
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
    Next
End Sub

Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub


' Flasher objects
' Uses own faster timer

Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 250 Then
                FlashLevel(nr) = 250
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
            Object.opacity = FlashLevel(nr)
        Case 1         ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub


Sub Flash2(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 25 Then
                FlashLevel(nr) = 25
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub Flash2m(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
            Object.opacity = FlashLevel(nr) 
        Case 1         ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub



Sub FlashVal(nr, object, value)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > value Then
                FlashLevel(nr) = value
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

Sub FlashValm(nr, object, value) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0 'off
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub



Sub FadeLn(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Offimage = d:FadingLevel(nr) = 0 'Off
        Case 3:Light.Offimage = c:FadingLevel(nr) = 2 'fading...
        Case 4:Light.Offimage = b:FadingLevel(nr) = 3 'fading...
        Case 5:Light.Offimage = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadeLnm(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Offimage = d
        Case 3:Light.Offimage = c
        Case 4:Light.Offimage = b
        Case 5:Light.Offimage = a
    End Select
End Sub

Sub LMapn(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Onimage = d:FadingLevel(nr) = 0 'Off
        Case 3:Light.ONimage = c:FadingLevel(nr) = 2 'fading...
        Case 4:Light.ONimage = b:FadingLevel(nr) = 3 'fading...
        Case 5:Light.ONimage = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub LMapnm(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.ONimage = d
        Case 3:Light.ONimage = c
        Case 4:Light.ONimage = b
        Case 5:Light.ONimage = a
    End Select
End Sub
' Walls

Sub FadeW(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.IsDropped = 1:FadingLevel(nr) = 0                 'Off
        Case 3:b.IsDropped = 1:c.IsDropped = 0:FadingLevel(nr) = 2 'fading...
        Case 4:a.IsDropped = 1:b.IsDropped = 0:FadingLevel(nr) = 3 'fading...
        Case 5:a.IsDropped = 0:FadingLevel(nr) = 1                 'ON
    End Select
End Sub

Sub FadeWm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.IsDropped = 1
        Case 3:b.IsDropped = 1:c.IsDropped = 0
        Case 4:a.IsDropped = 1:b.IsDropped = 0
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeW(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 1:FadingLevel(nr) = 0
        Case 5:a.IsDropped = 0:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeWm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 1
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeWi(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 0:FadingLevel(nr) = 0
        Case 5:a.IsDropped = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeWim(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 0
        Case 5:a.IsDropped = 1
    End Select
End Sub

'Lights

Sub FadeL(nr, a, b)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeLm(nr, a, b)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:a.state = 1
    End Select
End Sub

Sub LMap(nr, a, b, c) 'can be used with normal/olod style lights too
    Select Case FadingLevel(nr)
        Case 2:c.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 0:c.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:b.state = 1:FadingLevel(nr) = 3
        Case 5:b.state = 0:c.state = 0:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub LMapm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.state = 0
        Case 3:b.state = 0:c.state = 1
        Case 4:a.state = 0:b.state = 1
        Case 5:b.state = 0:c.state = 0:a.state = 1
    End Select
End Sub

'Reels

Sub FadeR(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3:FadingLevel(nr) = 0
        Case 3:a.SetValue 2:FadingLevel(nr) = 2
        Case 4:a.SetValue 1:FadingLevel(nr) = 3
        Case 5:a.SetValue 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeRm(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3
        Case 3:a.SetValue 2
        Case 4:a.SetValue 1
        Case 5:a.SetValue 1
    End Select
End Sub

'Texts

Sub NFadeT(nr, a, b)
    Select Case FadingLevel(nr)
        Case 4:a.Text = "":FadingLevel(nr) = 0
        Case 5:a.Text = b:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, a, b)
    Select Case FadingLevel(nr)
        Case 4:a.Text = ""
        Case 5:a.Text = b
    End Select
End Sub

' Flash a light, not controlled by the rom

Sub FlashL(nr, a, b)
    Select Case FadingLevel(nr)
        Case 1:b.state = 0:FadingLevel(nr) = 0
        Case 2:b.state = 1:FadingLevel(nr) = 1
        Case 3:a.state = 0:FadingLevel(nr) = 2
        Case 4:a.state = 1:FadingLevel(nr) = 3
        Case 5:b.state = 1:FadingLevel(nr) = 4
    End Select
End Sub

' Light acting as a flash. C is the light number to be restored

Sub MFadeL(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0:SetLamp c, FadingLevel(c)
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub MFadeLm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:SetLamp c, FadingLevel(c)
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:a.state = 1
    End Select
End Sub

'Alpha Ramps used as fading lights
'ramp is the name of the ramp
'a,b,c,d are the images used for on...off
'r is the refresh light

Sub FadeAR(nr, ramp, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:ramp.image = d:FadingLevel(nr) = 0 'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:ramp.image = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadeARm(nr, ramp, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:ramp.image = d
        Case 3:ramp.image = c
        Case 4:ramp.image = b
        Case 5:ramp.image = a
    End Select
End Sub

Sub FlashFO(nr, ramp, a, b, c)                                   'used for reflections when there is no off ramp
    Select Case FadingLevel(nr)
        Case 2:ramp.IsVisible = 0:FadingLevel(nr) = 0                'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2                'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3                'fading...
        Case 5:ramp.image = a:ramp.IsVisible = 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FlashAR(nr, ramp, a, b, c)                                   'used for reflections when there is no off ramp
    Select Case FadingLevel(nr)
        Case 2:ramp.alpha = 0:FadingLevel(nr) = 0                'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2                'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3                'fading...
        Case 5:ramp.image = a:ramp.alpha = 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FlashARm(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:ramp.alpha = 0
        Case 3:ramp.image = c
        Case 4:ramp.image = b
        Case 5:ramp.image = a:ramp.alpha = 1
    End Select
End Sub

Sub NFadeAR(nr, ramp, a, b)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:FadingLevel(nr) = 0 'off
        Case 5:ramp.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeARm(nr, ramp, a, b)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b
        Case 5:ramp.image = a
    End Select
End Sub

Sub MNFadeAR(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:FadingLevel(nr) = 0:SetLamp c, FadingLevel(c) 'off
        Case 5:ramp.image = a:FadingLevel(nr) = 1                           'on
    End Select
End Sub

Sub MNFadeARm(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:SetLamp c, FadingLevel(c) 'off
        Case 5:ramp.image = a                           'on
    End Select
End Sub

' Flashers using PRIMITIVES
' pri is the name of the primitive
' a,b,c,d are the images used for on...off

Sub FadePri(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0 'Off
        Case 3:pri.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePri3m(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 3:pri.image = group(2)
        Case 4:pri.image = group(1)
        Case 5:pri.image = group(0)
    End Select
End Sub

Sub FadePri3(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 3:pri.image = group(2):FadingLevel(nr) = 0 'Off
        Case 4:pri.image = group(1):FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub



Sub FadePri2(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 4:pri.image = group(1) 'off
        Case 5:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri2m(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 4:pri.image = group(1):FadingLevel(nr) = 0 'off
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub


Sub FadePri4m(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(3) 'Off
        Case 3:pri.image = group(2) 'Fading...
        Case 4:pri.image = group(1) 'Fading...
        Case 5:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri4(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 2:pri.image = group(3):FadingLevel(nr) = 0 'Off
        Case 3:pri.image = group(2):FadingLevel(nr) = 2 'Fading...
        Case 4:pri.image = group(1):FadingLevel(nr) = 3 'Fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub



Sub FadePriC(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:For each xx in pri:xx.image = d:Next:FadingLevel(nr) = 0 'Off
        Case 3:For each xx in pri:xx.image = c:Next:FadingLevel(nr) = 2 'fading...
        Case 4:For each xx in pri:xx.image = b:Next:FadingLevel(nr) = 3 'fading...
        Case 5:For each xx in pri:xx.image = a:Next:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePrih(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:SetFlash nr, 0:FadingLevel(nr) = 0 'Off
        Case 3:pri.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = a:SetFlash nr, 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d
        Case 3:pri.image = c
        Case 4:pri.image = b
        Case 5:pri.image = a
    End Select
End Sub

Sub NFadePri(nr, pri, a, b)
    Select Case FadingLevel(nr)
        Case 4:pri.image = b:FadingLevel(nr) = 0 'off
        Case 5:pri.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadePrim(nr, pri, a, b)
    Select Case FadingLevel(nr)
        Case 4:pri.image = b
        Case 5:pri.image = a
    End Select
End Sub

'Fade a collection of lights

Sub FadeLCo(nr, a, b) 'fading collection of lights
    Dim obj
    Select Case FadingLevel(nr)
        Case 2:vpmSolToggleObj b, Nothing, 0, 0:FadingLevel(nr) = 0
        Case 3:vpmSolToggleObj b, Nothing, 0, 1:FadingLevel(nr) = 2
        Case 4:vpmSolToggleObj a, Nothing, 0, 0:FadingLevel(nr) = 3
        Case 5:vpmSolToggleObj a, Nothing, 0, 1:FadingLevel(nr) = 1
    End Select
End Sub

'only for this table

' Alternate two lights made with walls

Sub AlternateW(nr, a, b)
    Select Case LampState(nr)
        Case 4:SetLamp a, 0:SetLamp b, 0:LampState(nr) = 0   'both off
        Case 5:SetLamp a, 1:SetLamp b, 0:LampState(nr) = 6   'On - Off
        Case 6:LampState(nr) = 7                             'wait
        Case 7:LampState(nr) = 8                             'wait
        Case 8:LampState(nr) = 9                             'wait
        Case 9:LampState(nr) = 10                            'wait
        Case 10:SetLamp a, 0:SetLamp b, 1:LampState(nr) = 11 'Off - On
        Case 11:LampState(nr) = 12                           'wait
        Case 12:LampState(nr) = 13                           'wait
        Case 13:LampState(nr) = 14                           'wait
        Case 14:LampState(nr) = 15                           'wait
        Case 15:LampState(nr) = 5
    End Select
End Sub


	'Secret Target

Dim SecretTargetHit,SecretTargetStep

Sub SecretTargetTimer_timer()
	Select Case SecretTargetStep
		Case 1:pSecretTarget.ObjRotX = 15:pSecretTarget.TransY = 5
        Case 2:pSecretTarget.ObjRotX = 30:pSecretTarget.TransY = 10
        Case 3:pSecretTarget.ObjRotX = 45:pSecretTarget.TransY = 15
        Case 4:pSecretTarget.ObjRotX = 60:pSecretTarget.TransY = 20
        Case 5:pSecretTarget.ObjRotX = 75:pSecretTarget.TransY = 25
        Case 6:pSecretTarget.ObjRotX = 90:pSecretTarget.TransY = 30
		Case 7:SecretTargetStep = 6
        Case 8:pSecretTarget.ObjRotX = 75:pSecretTarget.TransY = 25:Playsoundat ("SecretPassageSound"),pSecretTarget'PlaySoundAtBall "SecretPassageSound"
        Case 9:pSecretTarget.ObjRotX = 60:pSecretTarget.TransY = 20
        Case 10:pSecretTarget.ObjRotX = 45:pSecretTarget.TransY = 15
		Case 11:pSecretTarget.ObjRotX = 30:pSecretTarget.TransY = 10
		Case 12:pSecretTarget.ObjRotX = 15:pSecretTarget.TransY = 5
		Case 13:pSecretTarget.ObjRotX = 0:pSecretTarget.TransY = 0
		Case 14:pSecretTarget.ObjRotX = -5
		Case 15:pSecretTarget.ObjRotX = 5
        Case 16:pSecretTarget.ObjRotX = 0:Me.Enabled = 0:SecretTargetStep = 0
     End Select
	SecretTargetStep = SecretTargetStep + 1
End Sub


 '*****************************************************************
 'Functions
 '*****************************************************************

'*** PI returns the value for PI

Function PI()

	PI = 4*Atn(1)

End Function



'*****************************************
'			BALL SHADOW
'*****************************************
Dim BallShadow,BallShadowA,BallShadowB
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3)
BallShadowA = Array (BallShadowA1,BallShadowA2,BallShadowA3)
'BallShadowB = Array (BallShadowB1)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < HH.Width/2 Then
           BallShadow(b).X = BOT(b).X - (HH.Width/2 - BOT(b).X)/30
           BallShadowA(b).X = BOT(b).X - (HH.Width/2 - BOT(b).X)/30
        Else
            BallShadow(b).X = BOT(b).X - (HH.Width/2 - BOT(b).X)/30
            BallShadowA(b).X = BOT(b).X - (HH.Width/2 - BOT(b).X)/30
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        ballShadow(b).Height = BOT(b).Z 
        ballShadowA(b).Y = BOT(b).Y' + 12
        ballShadowA(b).Height = BOT(b).Z - 23
        If BOT(b).Z < 600 and BOT(b).Z > 450 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If

        If BOT(b).Z < 425 and BOT(b).Z > 0 Then
            BallShadowA(b).visible = 1
        Else
            BallShadowA(b).visible = 0
        End If
    Next
End Sub

'535

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 200)*1.2
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / HH.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / HH.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 3 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub CollisionTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

       ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
				If OnWireRamp = 1 Then
						StopSound("fx_ballrolling" & b)
						PlaySound("metalrolling" & b), -1, Vol(BOT(b) )*25, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) ' vol was *.7
				Else
						StopSound("metalrolling" & b)
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )  ' vol was *.5
					
				End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

'Ball Drop Hits
		If BOT(b).VelZ < 0 and BOT(b).z < 445 and BOT(b).z > 440 Then
			If InRect(BOT(b).x,BOT(b).y,(pDummyPrim1.x-60),(pDummyPrim1.y-60),(pDummyPrim1.x+60),(pDummyPrim1.y-60),(pDummyPrim1.x+60),(pDummyPrim1.y+60),(pDummyPrim1.x-60),(pDummyPrim1.y+60)) Then
			Else
				PlaySoundAtBOTBallZ "BallBounceThrough", BOT(b)
			End If
		ElseIf BOT(b).VelZ < 0 and BOT(b).z < 510 and BOT(b).z > 490 Then
			PlaySoundAtBOTBallZ "BallDrop", BOT(b)
		ElseIf BOT(b).VelZ > 530 Then
			PlaySoundAtBOTBallZ "glass_hit", BOT(b)
		Elseif BOT(b).VelZ < 0 and BOT(b).z < (0.193 * BOT(b).Y - 23.6 + 60) and BOT(b).z > (0.193 * BOT(b).Y - 23.6 + 40) Then
			PlaySoundAtBOTBallZ "BallDrop", BOT(b)
'		End If
		End If


	' *********	Kicker Code
	If InRect(BOT(b).x,BOT(b).y,133,421,198,421,198,482,133,482) Then
		If ABS(BOT(b).velx) < 0.05 and ABS(BOT(b).vely) < 0.05 Then
			sw46.enabled = True
		End If
	End If
	' *********	End Kicker Code		

	' *********	Secret Target Code
	If InRect(BOT(b).x,BOT(b).y,483,664,543,664,543,724,483,724) And BOT(b).z > 450 Then
		SecretTargetTimer.enabled = 1
		SecretTargetHit = 1
	Elseif SecretTargetHit = 1 Then
		SecretTargetStep = 8
		SecretTargetHit = 0
	End If
	' *********	End Target Code		


    Next


End Sub


'RampHelpers

Dim OnWireRamp

Sub tRampHelper1a_hit()
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End Sub

Sub tRampHelper1b_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

Sub tRampHelper2a_hit()
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End Sub

Sub tRampHelper2b_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

Sub Ramp4HelperA_hit()
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End Sub

Sub Ramp4HelperB_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

Sub Ramp5HelperA_hit()
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End Sub

Sub Ramp5HelperB_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

Sub Ramp1HelperA_hit()
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End Sub

Sub Ramp1HelperB_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

Sub Ramp6HelperA_hit()
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End Sub

Sub Ramp6HelperB_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub



Sub Ramp3HelperA_hit()
If OnWireRamp = 1 Then
	OnWireRamp = 0
    StopSound("metalrolling")
Else
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End If
End Sub

Sub Ramp3HelperB_hit()
If OnWireRamp = 1 Then
	OnWireRamp = 0
    StopSound("metalrolling")
Else
	OnWireRamp = 1
    StopSound("fx_ballrolling")
End If
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub



'******************************************************
' 				JP's Sound Routines
'******************************************************

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub Posts_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, 100
End Sub


Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub LeftFlipper2_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub FlipperLL_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub FlipperLR_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub FlipperUL_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub

Sub FlipperUR_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 100
End Sub


'**************************************************************************
'                 Positional Sound Playback Functions by DJRobX
'**************************************************************************

'Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(sound, tableobj)
		PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'Set all as per ball position & speed.

Sub PlaySoundAtBall(sound)
		PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub


'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
		PlaySound sound, 0, VolZ(BOT), Pan(BOT), 0, Pitch(BOT), 1, 1, AudioFade(BOT)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
		PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub


'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*****************************
'Random Ramp and Orbit Sounds
'*****************************

Dim NextOrbitHit:NextOrbitHit = 0 

Sub WireRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump3 .1, Pitch(ActiveBall)+5
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .4 + (Rnd * .2)
	end if 
End Sub

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .4 and Timer > NextOrbitHit then
		RandomBump 5, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


Sub MetalGuideBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump2 2, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

Sub MetalWallBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, 20000 'Increased pitch to simulate metal wall
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "fx_rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires metalguidebump1 to 2 in Sound Manager
Sub RandomBump2(voladj, freq)
	dim BumpSnd:BumpSnd= "metalguidebump" & CStr(Int(Rnd*2)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires WireRampBump1 to 5 in Sound Manage
Sub RandomBump3(voladj, freq)
	dim BumpSnd:BumpSnd= "WireRampBump" & CStr(Int(Rnd*5)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub



' Stop Bump Sounds
Sub BumpSTOP1_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP2_Hit ()
dim i:for i=1 to 4:StopSound "PlasticRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub


'Inkochnito Dip switches
Set vpmShowDips = GetRef("editDips")

Sub editDips
	Dim vpmDips:Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700, 400, "Haunted House - DIP switches"
		.AddFrame 2, 2, 190, "Maximum credits", 49152, Array("8 credits", 0, "10 credits", 32768, "15 credits", &H00004000, "25 credits", 49152)                                                                                   'dip 15&16
		.AddFrame 2, 78, 190, "Coin chute 1 and 2 control", &H00002000, Array("Seperate", 0, "Same", &H00002000)                                                                                                                   'dip 14
		.AddFrame 2, 124, 190, "Playfield special", &H00200000, Array("Replay", 0, "Extra Ball", &H00200000)                                                                                                                       'dip 22
		.AddFrame 2, 170, 190, "Tilt penalty", &H10000000, Array("game over", 0, "ball in play only", &H10000000)                                                                                                                  'dip 29
		.AddFrame 2, 216, 190, "High score to date awards", &H00C00000, Array("not displayed and no award", 0, "displayed and no award", &H00800000, "displayed and 2 credits", &H00400000, "displayed and 3 credits", &H00C00000) 'dip 23&24
		.AddFrameExtra 205, 2, 190, "Attract Sound", &H000c, Array("Off", 0, "every 10 seconds", &H0004, "every 2 minutes", &H0008, "every 4 minutes", &H000C)                                                                     'sounddip 3&4
		.AddFrame 205, 78, 190, "Balls per game", &H00010000, Array("5 balls", 0, "3 balls", &H00010000)                                                                                                                           'dip 17
		.AddFrame 205, 124, 190, "Replay limit", &H00040000, Array("No limit", 0, "One per ball", &H00040000)                                                                                                                      'dip 19
		.AddFrame 205, 170, 190, "Novelty mode", &H00080000, Array("Normal game mode", 0, "50,000 points per special/extra ball", &H00080000)                                                                                      'dip 20
		.AddFrame 205, 216, 190, "Game mode", &H00100000, Array("Replay", 0, "Extra ball", &H00100000)                                                                                                                             'dip 21
		.AddFrame 205, 262, 190, "3rd coin chute credits control", &H00001000, Array("No effect", 0, "Add 9", &H00001000)                                                                                                          'dip 13
		.AddChk 7, 292, 100, Array("Background sound32", &H80000000)                                                                                                                                                               'dip 32
		.AddChk 7, 308, 100, Array("Match feature", &H00020000)                                                                                                                                                                    'dip 18
		.AddChk 7, 328, 100, Array("Credits displayed", &H08000000)                                                                                                                                                                'dip 28
		.AddChk 7, 348, 100, Array("Coin switch tune", &H04000000)                                                                                                                                                                 'dip 27
		.AddChk 207, 308, 100, Array("Attract features", &H20000000)                                                                                                                                                               'dip 30
		.AddChkExtra 207, 328, 140, Array("Background sound", &H0010)                                                                                                                                                              'sounddip 5
		.AddChk 207, 348, 100, Array("Must be on", &H03000000)                                                                                                                                                                     'dip 25&26
		.AddLabel 50, 370, 300, 20, "After hitting OK, press F3 to reset game with new settings."
	End With
	Dim extra:extra = Controller.Dip(4) + Controller.Dip(5) * 256
	extra = vpmDips.ViewDipsExtra(extra)
	Controller.Dip(4) = extra And 255:Controller.Dip(5) = (extra And 65280) \ 256 And 255
End Sub