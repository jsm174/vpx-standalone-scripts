Option Explicit
Randomize

ExecuteGlobal GetTextFile("core.vbs") : If Err Then MsgBox "Can't open ""core.vbs"""

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim VRroom, B2Son, form1_off

Dim TB1missionon, TB3missionon, TB4missionon, vgameon: vgameon=0
TB1missionon=0: TB3missionon=0: TB4missionon=0
Dim loadeverything(588), loadwallsall(38), loadsidewallsall(38), loadrampsall(11), loadbumpersall(3)
Dim loadlampsall(48), loadglobesall(12), loadglobesmeshall(12)
Dim  LUTImage,fade 'fade true - fade to black: , fade false - fade to light: 
Lutimage = 1: fade = true

'----- DMD Options -----
Dim FlexDMD			'This is the FlexDMD object
Dim UseFlexDMD:				UseFlexDMD = 1				'0 = no FlexDMD, 1 = enable FlexDMD
Dim FlexONPlayfield

FlexONPlayfield = true		'False = off, True=DMD on playfield

Dim FlexMode:FlexMode=2			'This is use for specifying the state of the DMD
Dim FlexFrame:FlexFrame=0		'This is the current Frame count. It increments every time DMDTimer_Timer is run
Dim FontScoreInactive
Dim FontScoreActive
Dim FontBig1
Dim FontBig2
Dim FontBig3
Dim DMDTextOnScore
Dim DMDTextDisplayTime
Dim DMDTextEffect
Dim DMDFire
Dim DMDBGFlash
Dim flexgroup, flexvideo, flexvideofile, fleximage, fleximagefile, splashframevisible, splashframesdelay
splashframevisible=88
splashframesdelay=110
flexgroup="Welcome": flexvideo="homepin": flexvideofile="homepin.gif": fleximage="clear": fleximagefile="clear.png"
Dim videomission
Dim videomissionon: videomissionon = 0

'*******************************************
'  User Options
'*******************************************

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'/////////////////////-----BallRoll Sound Amplification -----/////////////////////
Const BallRollAmpFactor = 0 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)


'-----RampRoll Sound Amplification -----/////////////////////
Const RampRollAmpFactor = 0 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'----- Phsyics Mods -----
Const RubberizerEnabled = 1			'0 = normal flip rubber, 1 = more lively rubber for flips, 2 = a different rubberizer
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

Dim gamemode: gamemode = 1	'1 - VR, 2 - cabinet, 3 - desktop, 4 - video game only

Dim Videogameonly: Videogameonly = 0 'set to 1 if just VR games wanted

Dim DesktopMode:DesktopMode = Table1.ShowDT
If DesktopMode then gamemode = 3
If RenderingMode=2 Then gamemode = 1
If RenderingMode<>2 and Not DesktopMode Then gamemode = 2
If Videogameonly = 1 then gamemode = 4

Const BallsPerGame = 5				'3 or 5

Dim Replay1:Replay1 = 2000000
Dim Replay2:Replay2 = 3000000
Dim Replay3:Replay3 = 4000000
Dim Replay4:Replay4 = 5000000


'****** TB mission variables 	****

Dim TB1difficulty, TB4difficulty

videomission = 0	' set to 0 to disable video mission breakout during game

TB1difficulty = 2	' game completes when reaching level 7 or time runs out. 3 is hard 
TB4difficulty = 1	' 1 for easy, 1.5 still possible but hard. 2 real hard bordering impossible, 3 for impossible


'*******************************************

select case gamemode

case 1:
	VRroom = 1
	B2Son = 0
	form1_off = 1
case 2:
	VRroom = 0
	B2Son = 1
	form1_off = 0
	FlexONPlayfield = false
'	table1.showDT = false
	table1.BackglassMode = 1
	videomission = 0
case 3:
	VRroom = 0
	B2Son = 0
	form1_off = 1
'	table1.showDT = true
	table1.BackglassMode = 0
	videomission = 0
case 4
	VRroom = 1
	B2Son = 0
	form1_off = 1
	FlexONPlayfield = false
	DMDTimer.enabled = 0
	FrameTimer.enabled = 0
	GameTimer.enabled = 0
	UseFlexDMD = 0
	disappear 0
	Lutimage = 22: fade = false
	TB3missionon=1:TB3missionsetup()
	videomission = 1
end select

if form1_off=1 Then
	Set Controller = CreateObject("b2s.server")
	controller.launchbackglass=0
	b2son=0
end if

If B2Son=1 then Set Controller = CreateObject("b2s.server") end if
If B2Son=0 then Set Controller = CreateObject("VPinMAME.Controller") end if

'*******************************************
' VR Room / VR Cabinet
'*******************************************

DIM VRThings

If gamemode = 1 then
	DMDPlayfield.dmd=False
	DMDPlayfield.visible=False
	DMDbackbox.DMD=True
	DMDbackbox.visible=true  ' flasher dmd
	for each VRThings in VR_Cab:VRThings.visible = 1:Next
elseif gamemode = 3 then
	DMDPlayfield.dmd=True
	DMDPlayfield.visible=True
	DMDbackbox.DMD=false
	DMDbackbox.visible=false  ' flasher dmd
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
Elseif gamemode = 2 then
	DMDPlayfield.dmd=False
	DMDPlayfield.visible=False
	DMDbackbox.DMD=True
	DMDbackbox.visible=true  ' flasher dmd
	PinCab_Rails.visible = 0
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
End If

'*******************************************
'  Constants and Global Variables
'*******************************************

Const UsingROM = False				'The UsingROM flag is to indicate code that requires ROM usage. Mostly for instructional purposes only.

Dim ballsize:  BallSize = 50					'Ball size must be 50
Dim ballmass: BallMass = 1					'Ball mass must be 1
Const tnob = 5						'Total number of balls
Const lob = 1						'Locked balls
Const cGameName = "thunderbirds"
Const B2STableName = "thunderbirds"

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False				'Ball in plunger lane
Dim PlayerScore(4)
Dim CurrentPlayer
Dim GameOn:GameOn = 0
Dim InProgress, gameplayed
Dim ballinplay, credit
Dim Players, HighPlayer, highscore
Dim MaxPlayers:MaxPlayers = 4
Dim PrevScore(4)
Dim MBIP:MBIP=0
Dim Multiballon
Dim shootagain, ballsave
Dim TableTilted, killtable
Dim TiltCount, Tiltsens
Dim NGCount: NGCount = 0
dim TextStr
Dim temp1s, temp2s, temp3s, temp4s, temp5s, temp6s
Dim Lstates(4,41)
Dim xxx
Dim captiveball

'*******************************************
'  FlexDMD
'*******************************************
'
' DMDTimer @17ms
' StartFlex with the intro @ Table1_Init
' Startgame ( KeyDown : PlungerKey ) calls the Game DMD to start if intro is on
' Make a copy of VPWExampleTableDMD folder, rename and paste into Visual Pinball\Tables\"InsertTableNameDMD"
' Update .ProjectFolder = ".\VPWExampleTableDMD\" to DMD folder name from previous step
' Update DMDTimer_Timer Sub to allow DMD to update remaining balls and credit: find ("Ball") and ("Credit")

' Commands :
' 	DMDBigText "LAUNCH",77,1  : display text instead of score : "text",frames(x17ms),effect  0=solid 1=blink
'	DMDBGFlash=15 : will light up background with new image for xx frames
'	DMDFire=Flexframe+50  : will animate the font for 50 frames
'
' For another demo, see the following from the FlexDMD developer:
'   https://github.com/vbousquet/flexdmd/tree/master/FlexDemo

Const 	FlexDMD_RenderMode_DMD_GRAY = 0, _
		FlexDMD_RenderMode_DMD_GRAY_4 = 1, _
		FlexDMD_RenderMode_DMD_RGB = 2, _
		FlexDMD_RenderMode_SEG_2x16Alpha = 3, _
		FlexDMD_RenderMode_SEG_2x20Alpha = 4, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9, _
		FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10, _
		FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11, _
		FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12, _
		FlexDMD_RenderMode_SEG_4x7Num10 = 13, _
		FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14, _
		FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15, _
		FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const 	FlexDMD_Align_TopLeft = 0, _
		FlexDMD_Align_Top = 1, _
		FlexDMD_Align_TopRight = 2, _
		FlexDMD_Align_Left = 3, _
		FlexDMD_Align_Center = 4, _
		FlexDMD_Align_Right = 5, _
		FlexDMD_Align_BottomLeft = 6, _
		FlexDMD_Align_Bottom = 7, _
		FlexDMD_Align_BottomRight = 8

Sub Flex_Init()
	If UseFlexDMD = 0 Then Exit Sub
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
    If FlexDMD is Nothing Then
        MsgBox "No FlexDMD found. This table will NOT run without it."
        Exit Sub
    End If
	SetLocale(1033)
	With FlexDMD
		.GameName = cGameName
		.TableFile = Table1.Filename & ".vpx"
		.Color = RGB(255, 88, 32)
		.RenderMode = FlexDMD_RenderMode_DMD_RGB
		.Width = 128
		.Height = 32
		.ProjectFolder = "./thunderbirdsDMD/"
		.Clear = True
		.Run = True
	End With

End Sub

Sub createsplash(group,video,videofile,image,imagefile, framevisible, framesdelay, duration)

FlexFrame=0
flexgroup = group
flexvideo = video
flexvideofile = videofile
fleximage = image
fleximagefile = imagefile
splashframevisible=FlexFrame + framevisible
splashframesdelay=FlexFrame + framesdelay

	If UseFlexDMD = 0 Then Exit Sub

	Dim Scene 
	Set Scene = FlexDMD.NewGroup(group)

	With Scene	
		.AddActor FlexDMD.Newvideo (video,videofile)
		.Getvideo(video).visible = True	
		.AddActor FlexDMD.NewImage(image,imagefile)
		.Getimage(image).visible = False
	End With

	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = FlexDMD_RenderMode_DMD_RGB
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor Scene
'	If gamemode = 2 or FlexONPlayfield Then FlexDMD.Show = False Else FlexDMD.Show = True
	If FlexONPlayfield Then FlexDMD.Show = False Else FlexDMD.Show = True
	FlexDMD.UnlockRenderThread
	
	FlexMode=2
	if duration>0 then
		vpmtimer.addtimer duration, "FlexMode=1: CreateSternStyle'"
	end if

end sub

Sub CreateSternStyle
	If UseFlexDMD = 0 Then Exit Sub

	Dim i

	Set FontScoreActive = FlexDMD.NewFont("TeenyTinyPixls5.fnt", vbWhite, vbWhite, 0)
	Set FontScoreInactive = FlexDMD.NewFont("TeenyTinyPixls5.fnt", RGB(100, 100, 100), vbWhite, 0)
	Set FontBig1 = FlexDMD.NewFont("sys80.fnt", vbWhite, vbBlack, 0)
	Set FontBig2 = FlexDMD.NewFont("sys80_1.fnt", vbWhite, vbBlack, 0)
	Set FontBig3 = FlexDMD.NewFont("sys80.fnt", RGB ( 10,10,10) ,vbBlack, 0)
	Dim scene
	Set scene = FlexDMD.NewGroup("Score")
	
	With Scene
		.AddActor FlexDMD.NewImage("bg","bgdarker.png")
		.Getimage("bg").visible = True ' False
		.AddActor FlexDMD.NewImage("bg2","bg.png")
		.Getimage("bg2").visible = False

		For i = 1 to 4
			.AddActor FlexDMD.NewLabel("Score_" & i, FontScoreInactive, "0")
		Next
		.AddActor FlexDMD.NewFrame("VSeparator")
		.GetFrame("VSeparator").Thickness = 1
		.GetFrame("VSeparator").SetBounds 45, 0, 1, 32
		.AddActor FlexDMD.NewGroup("Content")
		.GetGroup("Content").Clip = True
		.GetGroup("Content").SetBounds 47, 0, 81, 32
	End With

	Dim title : Set title = FlexDMD.NewLabel("TitleScroller", FontScoreActive, "THUNDERBIRDS")
	Dim af : Set af = title.ActionFactory
	Dim list : Set list = af.Sequence()
	list.Add af.MoveTo(128, 2, 0)
	list.Add af.Wait(0.5)
	list.Add af.MoveTo(-128, 2, 5.0)
	list.Add af.Wait(3.0)
	title.AddAction af.Repeat(list, -1)
	scene.GetGroup("Content").AddActor title


	Set title = FlexDMD.NewLabel("Title2", FontBig3, " ")
	title.SetAlignedPosition 42, 16, FlexDMD_Align_Center
	scene.GetGroup("Content").AddActor title
	Set title = FlexDMD.NewLabel("Title", FontBig1, " ")
	title.SetAlignedPosition 42, 16, FlexDMD_Align_Center
	scene.GetGroup("Content").AddActor title


	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Ball", FontScoreActive, "Ball 1")
	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Credit", FontScoreActive, "Credit 5")


	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = FlexDMD_RenderMode_DMD_RGB
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor scene
'	If gamemode = 2 or FlexONPlayfield Then FlexDMD.Show = False Else FlexDMD.Show = True
	If FlexONPlayfield Then FlexDMD.Show = False Else FlexDMD.Show = True
	FlexDMD.UnlockRenderThread

	FlexMode=1

End Sub

Sub DMDBigText ( tex, time,effect)
	If UseFlexDMD = 0 Then Exit Sub
	FlexFrame=0
	DMDTextOnScore=tex
	DMDTextDisplayTime = time

	DMDTextEffect=effect
	If FlexMode=2 Then CreateSternStyle
End Sub

' Flex on vrroom and playfield runs this one
Sub FlexFlasher
		Dim DMDp
		DMDp = FlexDMD.DmdColoredPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDColoredPixels = DMDp
		End If
End Sub

' Main FlexDMD Timer

Sub DMDTimer_Timer
	If UseFlexDMD = 0 Then Exit Sub
'	If gamemode = 2 Or FlexONPlayfield then FlexFlasher
	If FlexONPlayfield then FlexFlasher
	
	Dim i, n, x, y, label
	FlexFrame = FlexFrame + 1
	FlexDMD.LockRenderThread

	Select Case FlexMode

		Case 1 ' scoreboard 
'			If (FlexFrame Mod 64) = 0 Then CurrentPlayer = 1 + (CurrentPlayer Mod 4)
			If (FlexFrame Mod 16) = 0 Then
				For i = 1 to 4
					Set label = FlexDMD.Stage.GetLabel("Score_" & i)
					If i = CurrentPlayer Then
						label.Font = FontScoreActive
					Else
						label.Font = FontScoreInactive
					End If
					label.Text = FormatNumber(PlayerScore(i), 0)
					label.SetAlignedPosition 45, 1 + (i - 1) * 6, FlexDMD_Align_TopRight
				Next
			End If

			If DMDBGFlash>0 Then DMDBGFlash=DMDBGFlash-1 : FlexDMD.Stage.GetImage("bg2").visible=True : Else :  FlexDMD.Stage.GetImage("bg2").visible=False

			If DMDfire> 0 And (FlexFrame mod 8)>3 Then
				FlexDMD.Stage.GetLabel("Title").font = FontBig2
				DMDfire=DMDfire-1
			Else 
				FlexDMD.Stage.GetLabel("Title").font = FontBig1	
				DMDfire=DMDfire-1
			End If

			If DMDTextDisplayTime> 0  Then
				If DMDTextEffect=1 And (FLEXframe mod 20) >10 Then 
					FlexDMD.Stage.GetLabel("Title").Text = " "
					FlexDMD.Stage.GetLabel("Title2").Text = " "
					DMDTextDisplayTime = DMDTextDisplayTime - 1
				Else
					FlexDMD.Stage.GetLabel("Title").Text = DMDTextOnScore
					FlexDMD.Stage.GetLabel("Title2").Text = DMDTextOnScore
					DMDTextDisplayTime = DMDTextDisplayTime - 1
				End If
			Else
				FlexDMD.Stage.GetLabel("Title").Text = FormatNumber(PlayerScore(CurrentPlayer), 0)
				FlexDMD.Stage.GetLabel("Title2").Text = FormatNumber(PlayerScore(CurrentPlayer), 0)
			End If
			FlexDMD.Stage.GetLabel("Title").SetAlignedPosition 42, 16, FlexDMD_Align_Center
			FlexDMD.Stage.GetLabel("Title2").SetAlignedPosition 43, 17, FlexDMD_Align_Center

			FlexDMD.Stage.GetLabel("Ball").SetAlignedPosition 0, 33, FlexDMD_Align_BottomLeft
			FlexDMD.Stage.GetLabel("Credit").SetAlignedPosition 81, 33, FlexDMD_Align_BottomRight
 			FlexDMD.Stage.GetLabel("Ball").Text = "Ball " & ballinplay
			FlexDMD.Stage.GetLabel("Credit").Text = "Credit " & credit

		Case 2 ' dmdintro
			
			If FlexFrame=splashframevisible Then FlexDMD.Stage.Getimage(fleximage).visible = True

			If FlexFrame>splashframesdelay Then 
				If (FlexFrame mod 32) =10 Then FlexDMD.Stage.Getimage(fleximage).visible = True
				If (FlexFrame mod 32) =1 Then FlexDMD.Stage.Getimage(fleximage).visible = False
			End If

	End Select
	FlexDMD.UnlockRenderThread
End Sub

'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
'	DoDTAnim 						'handle drop target animations
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

Dim mMagnetD

'*******************************************
'  Table Initialization and Exiting
'*******************************************

Sub Table1_Init
	Dim i
	if gamemode <> 4 then
	TimerVRPlunger2.enabled = true
	gameplayed = false
	TableTilted = false
	InProgress=false
	GameOn = 0
	shootagain = 0
	ballsave=0
	ballinplay = ballspergame
	players=1
	killtable = 0
	loadem
	loadhs
	if playerscore(1)="" then playerscore(1)=1000
	if playerscore(2)="" then playerscore(2)=2000
	if playerscore(3)="" then playerscore(3)=3000
	if playerscore(4)="" then playerscore(4)=4000
	If Credit="" then Credit=0
	If highscore="" then highscore=1000
	' Turn on the GI lights
	dim xx
	For Each xx in GI
		xx.state = 1
	Next

	for each xx in lightprimitives: xx.visible = 0: next
	for each xx in alllights: xx.state = 0: next
	for currentplayer = 1 to 4: store_lstate: 	next
	CurrentPlayer=1

	'Init Captive Ball
	'CapKicker.CreateSizedBallWithMass Ballsize/2, BallMass:CapKicker.kick 0,0 :CapKicker.enabled=0
	set captiveball=CapKicker.CreateBall:CapKicker.kick 0,0 :CapKicker.enabled=0

	' Turn off the bumper lights
	FlBumperFadeTarget(1) = 0
	FlBumperFadeTarget(3) = 0
	FlBumperFadeTarget(5) = 0
	
	Set mMagnetD=New cvpmMagnet
 		With mMagnetD
 		.InitMagnet mag, 20		'
		.MagnetOn = 1
        .CreateEvents "mMagnetD"
		.GrabCenter=true
 		End With

	playsound "island", -1
	' FlexDMD
	DMDTimer.enabled = 1
	Flex_Init()
	introflex.enabled = 1

	end if
End Sub

Sub Table1_Exit
	if gamemode <> 4 then savehs
	If UseFlexDMD = 0 Then Exit Sub
'    If Not FlexDMD is Nothing Then
'		FlexDMD.Show = False
'		FlexDMD.Run = False
'		FlexDMD = NULL
'    End If
End Sub

Dim introcounter: introcounter = 0

sub introflex_timer()
introcounter = introcounter + 1

select case introcounter
case 1
	createsplash "Welcome", "homepin", "homepin.gif", "clear", "clear.png", 88, 110, 0
case 300
	createsplash "Welcome", "tbarego", "intro.gif", "clear", "clear.png", 88, 110, 0
case 600
	createsplash "Welcome", "homepin", "homepin.gif", "clear", "clear.png", 88, 110, 0
case 900
	createsplash "Welcome", "videocolour", "videocolour.gif", "clear", "clear.png", 88, 110, 0
case 1200
	createsplash "Welcome", "homepin", "homepin.gif", "clear", "clear.png", 88, 110, 0
case 1500
	createsplash "Welcome", "supermarionation", "supermarionation.gif", "clear", "clear.png", 88, 110, 0
case 1800
	introcounter = 0
end Select

if inprogress then introflex.enabled = 0: introcounter = 0: end if

end sub

'*******************************************
'  Scoring & replays
'*******************************************

Dim lightspecial: lightspecial = false

Sub Addscore (value)

	If not TableTilted Then

	if extraballtimer.enabled Then
		if l10.state = 1 then l10.state = 0 else l10.state = 1 end if
		if l11.state = 1 then l11.state = 0 else l11.state = 1 end if
	end if

	if lightspecial then

		if l9.state = 1 then l9.state = 0 else l9.state = 1 end if
		if l12.state = 1 then l12.state = 0 else l12.state = 1 end if

	end if

	PrevScore(CurrentPlayer) = playerScore(CurrentPlayer)
	PlayerScore(CurrentPlayer)=PlayerScore(CurrentPlayer)+value
	If value >999 then DMDBGFlash=15
	If value >9999 Then DMDFire=100
	CheckFreeGame
	end if 

End Sub

Sub CheckFreeGame()
	If PrevScore(CurrentPlayer) < Replay1 And playerScore(CurrentPlayer) >= Replay1 Then FreeGame: DMDBigText "REPLAY",117,1 
	If PrevScore(CurrentPlayer) < Replay2 And playerScore(CurrentPlayer) >= Replay2 Then FreeGame: DMDBigText "REPLAY",117,1 
	If PrevScore(CurrentPlayer) < Replay3 And playerScore(CurrentPlayer) >= Replay3 Then FreeGame: DMDBigText "REPLAY",117,1 
	If PrevScore(CurrentPlayer) < Replay4 And playerScore(CurrentPlayer) >= Replay4 Then FreeGame: DMDBigText "REPLAY",117,1 
End Sub

Sub FreeGame()
	AddCredit(1)
	KnockerSolenoid
End Sub

Dim plungercounter: plungercounter = 0

Sub TimerVRPlunger_Timer
	plungercounter = 0
	If PinCab_Shooter.Y < 100 then
		   PinCab_Shooter.Y = PinCab_Shooter.Y + 3
	End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = 20 + (5* Plunger.Position) -20 
	if plunger.position > 5 and plungercounter = 0 Then
			if (not inprogress) and Credit=0 then DMDBigText "ADD CREDITS",77,1 end if
			if (not inprogress) and Credit>0 then DMDBigText "START GAME",77,1 end if
			if inprogress and BIPL then DMDBigText "LAUNCH BALL",77,1 end if
			plungercounter =  plungercounter + 1
	end if
	if plunger.position <= 5 Then  plungercounter = 0

End Sub

'*******************************************
'  Key Press Handling
'*******************************************

Sub Table1_KeyDown(ByVal keycode)

	If keycode = LeftFlipperKey and InProgress and not TableTilted  Then
If gamemode <> 4 and videomissionon = 0 then 
		FlipperActivate LeftFlipper, LFPress
		SolLFlipper True						'This would be called by the solenoid callbacks if using a ROM
		RotateLaneLightsLeft()
end if
	End If

	If keycode = RightFlipperKey and InProgress and not TableTilted Then
If gamemode <> 4 and videomissionon = 0 then 
		FlipperActivate RightFlipper, RFPress
		SolRFlipper True						'This would be called by the solenoid callbacks if using a ROM
		RotateLaneLightsRight()
end if
	End If

	If keycode = leftmagnasave Then
If gamemode <> 4 and videomissionon = 0 then 

'teststeps()


end if
	end if

	If keycode = rightmagnasave Then
If gamemode <> 4 and videomissionon = 0 then 

'launchtb3timer.enabled = 1


end if
	end if



	If keycode = PlungerKey Then
		If gamemode <> 4 and videomissionon = 0 then 
				Plunger.Pullback : SoundPlungerPull
				TimerVRPlunger.Enabled = True
				TimerVRPlunger2.Enabled = False
				if (not inprogress) and Credit=0 then DMDBigText "ADD CREDITS",77,1 end if
				if (not inprogress) and Credit>0 then DMDBigText "START GAME",77,1 end if
				if inprogress and BIPL then DMDBigText "LAUNCH BALL",77,1 end if
		end if
	end if

	If keycode = LeftTiltKey and gamemode <> 4 and videomissionon = 0 Then Nudge 90, 5 : SoundNudgeLeft:checktilt
	If keycode = RightTiltKey and gamemode <> 4 and videomissionon = 0 Then Nudge 270, 5 : SoundNudgeRight:checktilt
	If keycode = CenterTiltKey and gamemode <> 4 and videomissionon = 0 Then Nudge 0, 3 : SoundNudgeCenter:checktilt

	'added below for cab users with real physical tilt mechs...
	If keycode = MechanicalTilt and gamemode <> 4 and videomissionon = 0 Then Tiltsens = 3: checktilt: End If

	If keycode = StartGameKey and Credit=0 and InProgress=false And GameOn = 0 Then 
If gamemode <> 4 and videomissionon = 0 then 
		DMDBigText "ADD CREDITS",77,1: 'If FlexMode=2 Then CreateSternStyle
end if
	end if

If gamemode <> 4 and videomissionon = 0 then 
	If keycode = StartGameKey and Credit>0 and InProgress=false And GameOn = 0 Then 
		DMDBigText "PLAYER 1 UP",117,1 : 'If FlexMode=2 Then CreateSternStyle
		SoundStartButton
		reset_table
		AddCredit(-1)
		InProgress=true
		StartNewGame.enabled = True
		Players = 1
		CurrentPlayer = 1
		ballinplay = 1
				
   elseif keycode = StartGameKey and Credit>0 and InProgress=true and Players>0 and Players<MaxPlayers and ballinplay<2 then
		DMDBigText "PLAYER ADDED",117,1 : 'If FlexMode=2 Then CreateSternStyle
		AddCredit(-1)
		Players=Players+1
	end if
end if

	If (keycode = AddCreditKey or keycode = AddCreditKey2) Then
If gamemode <> 4 and videomissionon = 0 then 
		DMDBigText "CREDIT ADDED",77,1 : 'If FlexMode=2 Then CreateSternStyle
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
		AddCredit(1)
end if
	End If

' **** mission key commands ******

	If keycode = PlungerKey Then
If gamemode = 4 or videomissionon = 1 then 
		if TB3missionon=1 then
			if not autopiloton then
				if viewpoint=1 then 
					viewpoint=0
					recentretb5
				else 
					viewpoint = 1
					recentretb5
				end if
			end if
		end if

		if TB4missionon=1 then
			explosion.enabled = 1
		end if
end if
	End If

	if keycode = startgamekey Then
If gamemode = 4 or videomissionon = 1 then 
		if TB3missionon=1 then
			if vgameon = false and start1.enabled= false then 
				intro.enabled=0
				briefing.visible=0:outro = false
				counter.imagea = "clear"
				counter.visible = 1
				startcount = 15
				start1.enabled= 1
				stopsound "space"
				PlaySound "TB are go", 0, 100, 0, 0, 0
			end if
		end if

		If TB1missionon = 1 then
			if vgameon = false and start1.enabled= false then 
				intro.enabled=0
				briefing.visible=0:outro = false
				counter.imagea = "clear"
				counter.visible = 1
				startcount = 15
				start1.enabled= 1
				stopsound "island"
				PlaySound "TB are go", 0, 100, 0, 0, 0
			end if
		end if

		If TB4missionon = 1 then
			if vgameon = false and start1.enabled= false then 
				intro.enabled=0
				briefing.visible=0:outro = false
				counter.imagea = "clear"
				counter.visible = 1
				startcount = 15
				start1.enabled= 1
				stopsound "undersea"
				PlaySound "TB are go", 0, 100, 0, 0, 0
			end if
		end if

	end if
end If

	if keycode = addcreditkey Then
If gamemode = 4 or videomissionon = 1 then
		if TB3missionon=1 then
			if vgameon = true then
					move.enabled = 1
					screen = 0:outro = false
					intro.enabled=1:
					briefing.visible=1
					resettb3mission
					vgameon = false
			end if
		end if

		if TB1missionon=1 then
			if vgameon = true then
					stopsound "jet"
					stopsound "victory"
					screen = 0:outro = false
					intro.enabled=1:
					briefing.visible=1
					updatelut.enabled = 1
					Resettb1mission.enabled=1
					vgameon = false
			end if
		end if

		If TB4missionon = 1 then
			if vgameon = true then
					stopsound "tb4drama"
					stopsound "drama"
					screen = 0:outro = false
					intro.enabled=1:
					briefing.visible=1
					updatelut.enabled = 1
					resettb4mission.enabled=1
					vgameon = false
			end if
		end if
end if
	end if

	If keycode = LeftFlipperKey Then
If gamemode = 4 or videomissionon = 1 then  
			if TB3missionon=1 or TB1missionon=1 then :LFF.enabled=1 :end if
			If TB4missionon = 1 then
				Left=true: if int(TB4yoke.objroty) = 0 and int(TB4yoke.objrotx) = 0 then LFF.enabled = 1 :RFF.enabled = 0 end if
			end if
end if
	End If

	If keycode = RightFlipperKey Then
If gamemode = 4 or videomissionon = 1 then  
			if TB3missionon=1 or TB1missionon=1 then :RFF.enabled=1 :end if
			If TB4missionon = 1 then
				Right=true: if int(TB4yoke.objroty) = 0 and int(TB4yoke.objrotx) = 0 then RFF.enabled = 1 :LFF.enabled = 0 end if
			end if
end if
	End If

	If keycode = leftmagnasave Then
If gamemode = 4 or videomissionon = 1 then  
			if TB3missionon=1 or TB1missionon=1 then :LMS.enabled=1 :end if
			If TB4missionon = 1 then
				down=true: if int(TB4yoke.objrotx) = 0 then LMS.enabled = 1 :RMS.enabled = 0 end if
			end if
end if
	End If

	If keycode = rightmagnasave Then
If gamemode = 4 or videomissionon = 1 then  
			if TB3missionon=1 or TB1missionon=1 then :RMS.enabled=1 :end if
			If TB4missionon = 1 then
				up=true: if int(TB4yoke.objrotx) = 0 then RMS.enabled = 1 :LMS.enabled = 0 end if
			end if
end if
	End If

	If keycode = LeftTiltKey Then
If gamemode = 4 and not fade then  
		 if TB1missionon=1 then TB1missionsetup() end if
		 if TB3missionon=1 then TB3missionsetup() end if
		 if TB4missionon=1 then TB4missionsetup() end if
end if
	End If
    
	If keycode = RightTiltKey Then
If gamemode = 4 and fade then  
		 if TB1missionon=1 then TB1missionend() end if
		 if TB3missionon=1 then TB3missionend() end if
		 if TB4missionon=1 then TB4missionend() end if
end if
	End If
    
	If keycode = CenterTiltKey Then
If gamemode = 4 and not fade then  
		If TB4missionon=1 then
			TB4missionon=0: TB3missionon=0: TB1missionon=1
		elseif TB3missionon=1 then
			TB4missionon=1: TB3missionon=0: TB1missionon=0
		elseif TB1missionon=1 then 
			TB1missionon=0: TB3missionon=1: TB4missionon=0
		end if
end if
	End If



End Sub

dim testing123: testing123 = 1

Sub teststeps()

select case testing123
case 1:Collection2_hit(20)
case 2:Collection2_hit(20)
case 3:Collection2_hit(20)
case 4:Collection2_hit(20)
case 5:sw14_Hit		'TB2
case 6:sw15_Hit
case 7:sw16_Hit
case 8:sw13_Hit	'TB1
case 9:sw13_Hit
case 10:sw13_Hit
case 11:sw13_Hit
case 12:sw13_Hit
case 13:sw6_Hit 'TB4
case 14:sw7_Hit
case 15:sw8_Hit
case 16:sw17_Hit	'mole
case 17:sw17_Hit
case 18:sw17_Hit
case 19:sw17_Hit
case 20:testing123 = 0

end select

testing123 = testing123 + 1
end sub

Sub Table1_KeyUp(ByVal keycode)



	If KeyCode = PlungerKey and InProgress=true Then
If gamemode <> 4 and videomissionon = 0 then 
		Plunger.Fire
		TimerVRPlunger.Enabled = False
		TimerVRPlunger2.Enabled = True
		if l36.state = 1 and shootagain = 0 then 	
			l36.state = 0
		end if
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		gameon=1
end if
	End If

	If KeyCode = PlungerKey and InProgress=false Then
If gamemode <> 4 and videomissionon = 0 then 
		Plunger.Fire
		TimerVRPlunger.Enabled = False
		TimerVRPlunger2.Enabled = True
end if
	end if

	If keycode = LeftFlipperKey and InProgress=true and not TableTilted Then
If gamemode <> 4 and videomissionon = 0 then 
		FlipperDeActivate LeftFlipper, LFPress
		SolLFlipper False	
end if					'This would be called by the solenoid callbacks if using a ROM
	End If

	If keycode = RightFlipperKey and InProgress=true and not TableTilted Then
If gamemode <> 4 and videomissionon = 0 then 
		FlipperDeActivate RightFlipper, RFPress
		SolRFlipper False						'This would be called by the solenoid callbacks if using a ROM
end if
	End If


' ******* mission key commands ******



	If keycode = LeftFlipperKey Then
If gamemode = 4 or videomissionon = 1 then  
		if TB3missionon=1 then
			LFF.enabled=0
			playthrust
		end if

		if TB1missionon=1 then LFF.enabled = 0:moveLR = 0
		If TB4missionon = 1 then
			Left=false: if int(TB4yoke.objroty) = -10 then LFF.enabled = 1 end if
		end if
end if
	End If

	If keycode = RightFlipperKey Then
If gamemode = 4 or videomissionon = 1 then  
		if TB3missionon=1 then
			RFF.enabled=0
			playthrust
		end if

		if TB1missionon=1 then RFF.enabled = 0:moveLR = 0
		If TB4missionon = 1 then
			Right=false: if int(TB4yoke.objroty) = 10 then RFF.enabled = 1 end if
		end if
end if
	End If

	If keycode = leftmagnasave Then
If gamemode = 4 or videomissionon = 1 then 
		if TB3missionon=1 then
			LMS.enabled=0
			playthrust

		end if

		if TB1missionon=1 then LMS.enabled = 0: moveUD = 0
		If TB4missionon = 1 then
			down=false: if int(TB4yoke.objrotx) = 5 then LMS.enabled = 1 end if
		end if
end if
	End If

	If keycode = rightmagnasave Then
If gamemode = 4 or videomissionon = 1 then 
		if TB3missionon=1 then
			RMS.enabled=0
			playthrust
		end if

		if TB1missionon=1 then RMS.enabled = 0: moveUD = 0
		If TB4missionon = 1 then
			up=false: if int(TB4yoke.objrotx) = -5 then RMS.enabled = 1 end if
		end if
end if
	End If


End Sub

Sub disappear (visiblility)

if visiblility = 0 then

for xxx = 0 to 587: if everything(xxx).visible then loadeverything(xxx) = 1 else loadeverything(xxx) = 0 end if: everything(xxx).visible = 0: next
'for xxx = 0 to 37: if wallsall(xxx).visible then loadwallsall(xxx) = 1 else loadwallsall(xxx) = 0 end if: wallsall(xxx).visible = 0: next
for xxx = 0 to 37: if wallsall(xxx).sidevisible then loadsidewallsall(xxx) = 1 else loadsidewallsall(xxx) = 0 end if: wallsall(xxx).sidevisible = 0: next
'for xxx = 0 to 10: if rampsall(xxx).visible then loadrampsall(xxx) = 1 else loadrampsall(xxx) = 0 end if: rampsall(xxx).visible = 0: next
for xxx = 0 to 2: if bumpersall(xxx).ringVisible then loadbumpersall(xxx) = 1 else loadbumpersall(xxx) = 0 end if: bumpersall(xxx).ringVisible = 0: next
for xxx = 0 to 47: loadlampsall(xxx) = lampsall(xxx).state: lampsall(xxx).state = 0: next
for xxx = 0 to 11: loadglobesmeshall(xxx) = globesall(xxx).ShowBulbMesh: globesall(xxx).ShowBulbMesh = 0: next
for xxx = 0 to 11: loadglobesall(xxx) = globesall(xxx).state: globesall(xxx).state = 0: next
if gamemode <> 4 then captiveball.visible = false
Else

for xxx = 0 to 587: everything(xxx).visible = loadeverything(xxx): next
'for xxx = 0 to 37:  wallsall(xxx).visible = loadwallsall(xxx): next
for xxx = 0 to 37:  wallsall(xxx).sidevisible = loadsidewallsall(xxx): next
'for xxx = 0 to 10:  rampsall(xxx).visible = loadrampsall(xxx): next
for xxx = 0 to 2:  bumpersall(xxx).ringVisible = loadbumpersall(xxx): next
for xxx = 0 to 47: lampsall(xxx).state = loadlampsall(xxx): next
for xxx = 0 to 11: globesall(xxx).ShowBulbMesh = loadglobesmeshall(xxx): next
for xxx = 0 to 11: globesall(xxx).state = loadglobesall(xxx): next
if gamemode <> 4 then captiveball.visible = true

end if

end sub

Sub RotateLaneLightsLeft()
	Dim TempState
	TempState = L6.State
	L6.State = L7.State
	L7.State = L8.State
	L8.State = TempState
End Sub

Sub RotateLaneLightsRight()
	Dim TempState
	TempState = L8.State
	L8.State = L7.State
	L7.State = L6.State
	L6.State = TempState
End Sub


Sub AddCredit(direction)

credit = credit + direction

if credit < 0 then credit = 0
if credit => 25 then credit = 25

End Sub

sub scorespecial
	AddCredit (1)
end sub

'******************************************************
' 					START NEW GAME
'******************************************************

Sub StartNewGame_Timer()
	start.enabled = 1
	stopsound "island"
dim i

	NGCount = NGCount + 1

	Select Case (NGCount)
		Case 1: 
			if int(TB2ramp.rotx) = 95 then rampupdown.enabled = 1: playsoundat "hydrolic2", opto  end if 'raise ramp
			playsoundat "hood mission",PtargetC
 		

		Case 12:
			for i = 1 to 4: Lstates(i,20)=2: next
			for i = 1 to 4: Lstates(i,32)=2: next
			for i = 1 to 4: Lstates(i,33)=2: next
			for i = 1 to 4: Lstates(i,34)=2: next
			for i = 1 to 4: Lstates(i,35)=2: next
			l20.state = 2:l32.state=2:l33.state=2:l34.state=2:l35.state=2

		Case 14:
			' Create a ball
			BallRelease.CreateBall
			BallRelease.Kick 90, 5
			ResetTilt
		Case 15: 
			DMDBigText "DEFEAT HOOD",144,1
			StartNewGame.enabled = false
			NGCount = 0
	End Select	

End Sub

Sub Reset_table

	dim i, j, xx

	TableTilted = false
	TiltCount=0
	InProgress=false
	GameOn = 0
	lightspecial = false
	shootagain = 0
	ballsave=0
	mbip = 0
	playing = 1
	IRcount = 1: backblade.image = "backblade1"
	for each xx in alllights: xx.state = 0: next
	for i = 1 to maxplayers
		playerScore(i)=0
		PrevScore(i)=0
		for j = 1 to 41: Lstates(i,j)=0: next
	Next
	transition
End sub

Sub start_Hit
	if ballsave = 0 then 
		l36.state=2
		ballsave=1
		ballsaver.enabled=1
	end if

	if multiballon=0 then
		playbackground playing
		playsoundat "thrust2", Plunger
		speeding
	end if

end sub

Sub saveball

	l36.state=2
	ballsave=1
	ballsaver.enabled=1

end sub

sub ballsaver_timer
	ballsaver.interval=10000
	ballsaver.enabled=0
	if shootagain=>1 then l36.state=1 else l36.state=0 end if
	ballsave=0
	start.enabled = 0

End sub

sub swauto_hit

if (ballsave = 1 or mbip> 0) then 
	playsoundat "thrust2", Plunger
	if multiballon=0 then DMDBigText "BALL SAVED",57,1
	Plunger.Pullback ': PlungerIM.Pullback
	swauto.timerenabled=1
end if

end sub

Sub swauto_Unhit()	

End Sub

sub swauto_timer
		Plunger.Fire ': PlungerIM.Fire
		swauto.timerenabled=0
end sub

dim transitioncounter: transitioncounter = 1

sub transition()

	if TB3bay.visible and l22.state = 1 then 
		updatelut.enabled = 1
		transitiontimer.enabled = 1
	end if

	if not TB3bay.visible and l22.state <> 1 then 
		updatelut.enabled = 1
		transitiontimer.enabled = 1
	end if

end sub

sub transitiontimer_timer ()

	if l22.state = 1 then 
	select case transitioncounter

		case 1: tracybuilding.visible = 1: tracypoolsurface.visible = 1:TB3bay.visible = 0	': tracypoolbase.visible = 1

		case 2: updatelut.enabled = 1: transitioncounter = 0: transitiontimer.enabled = 0
	end Select

	end if

	if l22.state <> 1 then 
	select case transitioncounter

		case 1: tracybuilding.visible = 0: tracypoolsurface.visible = 0:TB3bay.visible = 1 ' : tracypoolbase.visible = 0

		case 2: updatelut.enabled = 1: transitioncounter = 0: transitiontimer.enabled = 0
	end Select

	end if

transitioncounter = transitioncounter + 1

end sub

dim transitioncounterm: transitioncounterm = 1
dim m

Sub transitionmission(mis)
	m = mis
	updatelut.enabled = 1
	transitiontimerm.enabled = 1
end sub

sub transitiontimerm_timer ()

	extraballtimer_timer()

if videomissionon = 1 then 
	select case transitioncounterm

		case 1: 
			disappear 0
			if m = 1 then :TB1missionon=1: TB1missionsetup(): end if
			if m = 3 then :TB3missionon=1: TB3missionsetup(): end if
			if m = 4 then :TB4missionon=1: TB4missionsetup(): end if

		case 2: 
			updatelut.enabled = 1: transitioncounterm = 0: transitiontimerm.enabled = 0
	end Select

end if

if videomissionon = 0 then
	select case transitioncounterm

		case 1: 
			disappear 1
			if m = 1 then :TB1missionon=0:TB1missionend() end if
			if m = 3 then :TB3missionon=0:TB3missionend() end if
			if m = 4 then :TB4missionon=0:TB4missionend() end if

		case 2: 
			updatelut.enabled = 1: transitioncounterm = 0: transitiontimerm.enabled = 0
			start.enabled = 1
			checklockedball
			BallRelease.CreateBall
			BallRelease.Kick 90, 5
	end Select

end if

transitioncounterm = transitioncounterm + 1

end sub


'******************************************************
' 							TILT
'******************************************************

Sub CheckTilt
if not TableTilted and gameon=1 then
	If Tilttimer.Enabled Then 
		TiltSens = TiltSens + 1
		if TiltSens = 2 then DMDBigText "WARNING",154,1 end if
		if TiltSens = 3 Then 
			GameTilted
			ballsaver_timer()
'			drain.enabled = 1
		end if
	Else
		TiltSens = 1
		Tilttimer.Enabled = True
	End If

end if
End Sub

Sub Tilttimer_Timer()
	If TiltSens > 0 Then TiltSens = TiltSens - 1
	If TiltSens <= 0 Then TiltSens=1: Tilttimer.Enabled = False: end if
End Sub

Sub GameTilted
	DMDBigText "TILT",154,1
	TableTilted = true

	Bumper1.threshold = 100
	Bumper3.threshold = 100
	Bumper5.threshold = 100

	LeftSlingShot.SlingShotThreshold = 100
	RightSlingShot.SlingShotThreshold = 100

	LeftFlipper.RotateToStart
	RightFlipper.RotateToStart
End Sub

Sub ResetTilt
	TableTilted = false

	Bumper1.threshold = 1
	Bumper3.threshold = 1
	Bumper5.threshold = 1

	LeftSlingShot.SlingShotThreshold = 1.2
	RightSlingShot.SlingShotThreshold = 1.2
	
End Sub

'*******************************************
'  Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub


' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub


'*******************************************
'  Bumpers
'*******************************************
Sub Bumper1_Hit 
	Addscore 100
	RandomSoundBumperTop Bumper1
	FlBumperFadeTarget(1) = 1		'Flupper bumper demo
	Bumper1.timerenabled = True
End Sub

Sub Bumper1_Timer
	FlBumperFadeTarget(1) = 0
End Sub

Sub Bumper3_Hit '
	Addscore 100
	RandomSoundBumperBottom Bumper3
	FlBumperFadeTarget(3) = 1		'Flupper bumper demo
	Bumper3.timerenabled = True
End Sub

Sub Bumper3_Timer
	FlBumperFadeTarget(3) = 0
End Sub

Sub Bumper5_Hit 
	Addscore 100
	RandomSoundBumperMiddle Bumper5
	FlBumperFadeTarget(5) = 1		'Flupper bumper demo
	Bumper5.timerenabled = True
End Sub

Sub Bumper5_Timer
	FlBumperFadeTarget(5) = 0
End Sub

'****************************************************************
'  Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep

Sub RightSlingShot_Slingshot
	Addscore 10
	RSling1.Visible = 1
	Sling1.TransY = -20			'Sling Metal Bracket
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	'	vpmTimer.PulseSw 52			'Slingshot Rom Switch
	RandomSoundSlingshotRight Sling1
	shootingsingle sling1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:Sling1.TransY = -10
		Case 4:RSLing2.Visible = 0:Sling1.TransY = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	Addscore 10
	LSling1.Visible = 1
	Sling2.TransY = -20			'Sling Metal Bracket
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	'	vpmTimer.PulseSw 51			'Slingshot Rom Switch
	RandomSoundSlingshotLeft Sling2
	shootingsingle sling2
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:Sling2.TransY = -10
		Case 4:LSLing2.Visible = 0:Sling2.TransY = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

'*******************************************
'  Kickers, Saucers
'*******************************************
Sub Drain_Hit

dim xx, Random
	random = Int(3*Rnd)
	RandomSoundDrain Drain
	stopsound "drilling"
	stopsound "4. TB3 lift off sound"

	
If TableTilted Then
	playbackground 0
	if mbip> 0 then
		mbip = mbip - 1
		drain.DestroyBall
		exit Sub
	end If

	if mbip <= 0 then 
			mbip=0
			if multiballon=1 then 
				for each xx in alllights: xx.state = 0: next
				l20.state = 2:l32.state=2:l33.state=2:l34.state=2:l35.state=2
				stopsound "Tb4drama"
				IRcount = 1: backblade.image = "backblade1"
			end if
			multiballon=0
	end if
	reset_ball
	drain.DestroyBall
	drain.timerenabled = 1
	exit Sub

end if

if ballsave = 1 then 
	ballrelease.createball:BallRelease.Kick 90, 4 
	Me.DestroyBall
	exit Sub
end if

if ballsave = 0 and mbip = 0 then 
	select case Random
		case 0: playsoundat "drain hit 1", drain
		case 1: playsoundat "drain hit 2", drain
		case 2: playsoundat "drain hit 3", drain
	end select
	playbackground 0
	DMDBigText "BALL LOST",117,1
	reset_ball
	drain.timerenabled = 1
	drain.DestroyBall
end if

if ballsave = 0 and mbip> 0 then
	mbip = mbip - 1
	drain.DestroyBall
	if mbip <= 0 then 
		mbip=0
'		if multiballon=1 then 
			for each xx in alllights: xx.state = 0: next
			l20.state = 2:l32.state=2:l33.state=2:l34.state=2:l35.state=2
			playbackground playing
			stopsound "Tb4drama"
			IRcount = 1: backblade.image = "backblade1"
			lightspecial = true
			l9.state = Int(2*rnd): if l9.state = 2 then l9.state = 1 end if		'lite specials
			if l9.state = 1 then l12.state = 0 else l12.state = 1 end if
			createsplash "Alert", "siren", "siren.gif", "Slit", "specials is lit.png", 10, 50, 2700
			playsoundat "siren short", l36
'			DMDBigText "SPECIALS LIT",117,1
			LightSeqmultiballlights.stopPlay
'		end if
		multiballon=0
		exit sub
	end if
end if

End Sub

Sub drain_timer()
	NextBall 
	drain.timerenabled = 0
	drain.timerinterval = 2500
end sub

'***********   Reset Ball

Sub Reset_ball
	extraballtimer_timer()
	ballsave = 0
	TableTilted = false
	ResetTilt
	mbip=0
	multiballon=0
	IRcount = 1: backblade.image = "backblade1"
'	gameon=0
	l27.state = 0:l28.state = 0
	if molestage > 1 then playsoundat "opening", sw17
	l23.state = 0: molestage = 4: movemole.enabled = 1
	if int(TB2ramp.rotx) = 95 then rampupdown.enabled = 1:playsoundat "hydrolic2", opto 'raise ramp
	store_lstate

End sub

Sub NextBall()
Dim text

	If l36.state = 1 Then
		DMDBigText "SHOOT AGAIN",117,1
		shootagain=shootagain-1
		if shootagain<=0 then shootagain= 0 end if
	Elseif CurrentPlayer = Players Then
		BallInPlay = BallInPlay + 1
		CurrentPlayer = 1
		text = "PLAYER " & CurrentPlayer & " UP"
		DMDBigText text,117,1
	Else 
		CurrentPlayer = CurrentPlayer + 1
		text = "PLAYER " & CurrentPlayer & " UP"
		DMDBigText text,117,1
	End If

	If ballinplay = BallsPerGame then
		vpmtimer.addtimer 2000, "DMDBigText ""HIGH SCORE"",117,0 '"
		vpmtimer.addtimer 4000, "DMDBigText highscore,117,0 '"
	end if

	if ballinplay > BallsPerGame then
		InProgress = False
		gameon=0
		ballinplay=BallsPerGame
		DMDBigText "GAME OVER",117,1 : 'If FlexMode=2 Then CreateSternStyle
		vpmtimer.addtimer 2000, "introflex.enabled = 1 '"					' create outro scene
		playsound "island", -1							'Gameover - Game Over

		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart

		If playerScore(2) > playerScore(1) Then
			HighPlayer = 2
		Else 
			HighPlayer = 1
		End If

		If playerScore(3) > playerScore(HighPlayer) Then
			HighPlayer = 3
		End If

		If playerScore(4) > playerScore(HighPlayer) Then
			HighPlayer = 4
		End If

		If playerScore(HighPlayer) >= HighScore Then
'			FreeGame
			HighScore = playerScore(HighPlayer)
		End If

		savehs
	
		exit sub

	End If

	select case BallInPlay
		case 2: playsoundat "calling IR 1", Plunger
		case 3: playsoundat "calling IR 2", Plunger
		case 4: playsoundat "calling IR 3", Plunger
		case 5: playsoundat "receiving your call", Plunger
	end select

	ballrelease.createball:BallRelease.Kick 90, 4
	start.enabled = 1
	load_lstate
	if l29.state=2 and l30.state=2 and l31.state=2 then
		if l15.state=2 then poddoor.image = "pod4"
		if l16.state=2 then poddoor.image = "pod1"
		if l17.state=2 then poddoor.image = "pod2"
		if l19.state=2 then poddoor.image = "pod5"
		if l20.state=2 then poddoor.image = "pod3"
		if l18.state=2 then poddoor.image = "pod6"
		restoreramp.enabled = 1
	end if

	transition							' checks if the TB3 or TB1 launch bay needs to be displayed
End Sub

sub restoreramp_timer()
	if rampupdown.enabled then exit sub end if
	if int(TB2ramp.rotx) = 115 then rampupdown.enabled = 1:playsoundat "hydrolic2", opto 'lower ramp
	restoreramp.enabled = 0
end sub

Sub Ballrelease_Hit
	if mbip>0 then
		ballrelease.timerenabled = 1
	end if
End Sub

Sub Ballrelease_timer()
	ballrelease.kick  90, 5
	ballrelease.timerenabled = 0
End Sub


Sub Ballrelease_UnHit
	RandomSoundBallRelease Ballrelease
End Sub

'*******************************************
'  Triggers											Ball in plunger lane trigger
'*******************************************

Sub Trigger1_Hit
	BIPL = True
End Sub

Sub Trigger1_UnHit
	BIPL = False
End Sub

Sub LeftOutlane_hit
	if l9.state = 1 then scorespecial: lightspecial = false: l9.state = 0: createsplash "Alert", "Klaxon", "klaxon.gif", "S", "specials.png", 10, 50, 2500:playsoundat "klaxon short", l36: 'DMDBigText "SPECIALS",117,1: else playsoundat "fail1", LeftOutlane: Addscore 1000: end if
end sub

Sub LeftOutlane_unhit

end sub

Sub LeftInlane_hit
	Addscore 1000
	playsoundat "camera detector1", LeftInlane
	if l10.state = 1 then l36.state = 1 : shootagain=shootagain+1: l10.state = 0: l11.state = 0:extraballtimer.enabled = 0: createsplash "Alert", "Klaxon", "klaxon.gif", "EB", "extraball.png", 10, 50, 2500: playsoundat "klaxon short", l36: 'DMDBigText "EXTRA BALL",117,1: end if
end sub

Sub LeftInlane_unhit

end sub

Sub rightOutlane_hit
	if l12.state = 1 then scorespecial: lightspecial = false: l12.state = 0:createsplash "Alert", "Klaxon", "klaxon.gif", "S", "specials.png", 10, 50, 2500:playsoundat "klaxon short", l36: 'DMDBigText "SPECIALS",117,1: else playsoundat "fail1", rightOutlane:Addscore 1000: end if
end sub

Sub rightOutlane_unhit

end sub

Sub rightInlane_hit
	Addscore 1000
	playsoundat "camera detector1", rightInlane
	if l11.state = 1 then l36.state = 1 : shootagain=shootagain+1: l11.state = 0: l10.state = 0: extraballtimer.enabled = 0: createsplash "Alert", "Klaxon", "klaxon.gif", "EB", "extraball.png", 10, 50, 2500: playsoundat "klaxon short", l36: 'DMDBigText "EXTRA BALL",117,1: end if
end sub

Sub rightInlane_unhit

end sub

Sub Rightupper_hit
	Addscore 1000
	playsoundat "rushing", Rightupper
	speeding
end sub

Sub Rightupper_unhit

end sub

Sub sw6_hit							'T
	Addscore 1000
	stopsound "rushing"
	playsoundat "camera detector1", sw6

	if multiballon = 0 then
	if l6.state <> 1 and multiballon = 0 then
		if l7.state <>1 and l8.state<>1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text1", "TB4text T.png", 88, 110, 2900
		elseif l7.state <>1 and l8.state=1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text4", "TB4text T4.png", 88, 110, 2900
		Elseif l7.state =1 and l8.state<>1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text5", "TB4text TB.png", 88, 110, 2900
		else
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text7", "TB4text TB4.png", 88, 110, 2900
		end if
	end if

	l6.state = 1:checktb4complete
	end if
end sub

Sub sw6_unhit						'T

end sub

Sub sw7_hit							'B
	Addscore 1000
	stopsound "rushing"
	playsoundat "camera detector1", sw7

	if multiballon = 0 then
	if l7.state <> 1 and multiballon = 0 then 
		if l6.state <>1 and l8.state<>1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text2", "TB4text B.png", 88, 110, 2900
		elseif l6.state <>1 and l8.state=1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text6", "TB4text B4.png", 88, 110, 2900
		Elseif l6.state =1 and l8.state<>1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text5", "TB4text TB.png", 88, 110, 2900
		else
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text7", "TB4text TB4.png", 88, 110, 2900
		end if
	end if

	l7.state = 1:checktb4complete
	end if
end sub

Sub sw7_unhit						'B

end sub

Sub sw8_hit							'4
	Addscore 1000
	stopsound "rushing"
	playsoundat "camera detector1", sw8

	if multiballon = 0 then
	if l8.state <> 1 and multiballon = 0 then 
		if l6.state <>1 and l7.state<>1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text3", "TB4text 4.png", 88, 110, 2900
		elseif l6.state <>1 and l7.state=1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text6", "TB4text B4.png", 88, 110, 2900
		Elseif l6.state =1 and l7.state<>1 then
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text4", "TB4text T4.png", 88, 110, 2900
		else
			createsplash "TB4", "TB4VID", "TB4.gif", "TB4text7", "TB4text TB4.png", 88, 110, 2900
		end if
	end if

	l8.state = 1:checktb4complete
	end if
end sub

Sub sw8_unhit						'4

end sub

Sub opto_hit
	if rampupdown.enabled then exit sub end if
	Addscore 1000
	mMagnetD.MagnetOn = 1
	opto.timerenabled=1
	rampupdown.enabled = 1:playsoundat "hydrolic2", opto
end sub

Sub opto_timer()
	DMDBigText "BALL CAPTURED",117,1
	mag.DestroyBall
	mMagnetD.MagnetOn = 0
	MBAntiGrabD.enabled = 1
	opto.timerenabled=0
end sub

Sub MBAntiGrabD_timer()
	mMagnetD.MagnetOn = 1
	MBAntiGrabD.enabled = 0
	if l18.state=2 then thunderbirdsarego: l29.state=0:l30.state=0:l31.state=0: DMDBigText "TB ARE GO",117,1: exit sub: end if

	if l20.state=2 and videomission = 1 then playbackground 0: videomissionon = 1: transitionmission 3: exit sub: end if			'TB3 mission
	if l16.state=2 and videomission = 1 then playbackground 0: videomissionon = 1: transitionmission 1: exit sub: end if			'TB1 mission
	if l15.state=2 and videomission = 1 then playbackground 0: videomissionon = 1: transitionmission 4: exit sub: end if			'TB4 mission

	start.enabled = 1
	checklockedball
	BallRelease.CreateBall
	BallRelease.Kick 90, 5
End Sub

' john=0, scott=1, virgil=2, alan=3, gordon=4, penny=5
sub checklockedball()

Dim random: random = int(2*Rnd)

	if l19.state=2 then 
		l19.state=1: l18.state=2: l41.state = 1:l13.state = 2: L14.state = 2 				'next poddoor.image = "pod6"
		vpmtimer.addtimer 1000, "incomingsignal 0 '"
		vpmtimer.addtimer 2000, "playsoundat ""TB5 to base"", opto '"
		vpmtimer.addtimer 2000, "DMDBigText ""HIT RAMP"",117,1 '"
	end if
	if l15.state=2 then 
		l15.state=1: l19.state=2: l40.state = 1:											'next poddoor.image = "pod5"
		vpmtimer.addtimer 1000, "incomingsignal 1 '"
		vpmtimer.addtimer 2000, "playsoundat ""we are gonna need the mole"", opto '"
		vpmtimer.addtimer 2000, "DMDBigText ""START MOLE"",117,1 '"
		if molestage > 1 then playsoundat "opening", sw17
		l23.state = 0: molestage = 4: movemole.enabled = 1
	end if
	if l16.state=2 then 
		l16.state=1: l15.state=2: l39.state = 1: 											'next poddoor.image = "pod4"
		l6.state = 2:l7.state = 2:l8.state = 2
		vpmtimer.addtimer 1000, "incomingsignal 4 '"
		vpmtimer.addtimer 2000, "playsoundat ""launch TB4"", opto '"
		vpmtimer.addtimer 2000, "DMDBigText ""SPELL TB4"",117,1 '"
	end if
	if l17.state=2 then
		saveball
		l17.state=1: l16.state=2: l38.state = 1: transition
		l2.state=2:l3.state=2:l4.state=2:l5.state=2
		vpmtimer.addtimer 2000, "incomingsignal 5 '"
		vpmtimer.addtimer 4000, "playsoundat ""Lady Penelope speaking"", opto '"			'next poddoor.image = "pod1"
		if random = 0 then 
			vpmtimer.addtimer 7500, "playsoundat ""Got a hot one for you"", opto '" 
		else 
			vpmtimer.addtimer 7500, "playsoundat ""go ahead penny"", opto '"
		end if
		vpmtimer.addtimer 10000, "DMDBigText ""SPELL FAB1"",117,1 '"
	end if
	if l20.state=2 then 
		l20.state=1: l17.state=2: l37.state = 1
		l24.state = 2 : l25.state = 2 : l26.state = 2
		vpmtimer.addtimer 1000, "incomingsignal 1 '"
		vpmtimer.addtimer 2000, "playsoundat ""need TB2"", opto '"					'next poddoor.image = "pod2"
		vpmtimer.addtimer 2000, "DMDBigText ""SPELL TB2"",117,1 '"
	end if
	l29.state=0:l30.state=0:l31.state=0
end Sub

'*******************************************
'  Ramp Triggers									not used. Ramps removed
'*******************************************
Sub ramptrigger1_hit()
	if ramptrigger1.timerenabled then stopsound "rushing" : exit sub: end if
	ramptrigger1.timerenabled = 1
	WireRampOn True 'Play Plastic Ramp Sound
	playsoundat "rushing", ramptrigger1
	speeding
End Sub

Sub ramptrigger1_timer()
	stopsound "rushing"
	ramptrigger1.timerenabled = 0
End Sub


Sub ramptrigger2_hit()
	ramptrigger1.timerenabled = 0
	Addscore 1000
	if L21.state = 2 Then				'launch TB1
		vpmtimer.addtimer 4000, "openTB2 1, 1'"
		l13.state = 0: L14.state = 0: l21.state = 1: launchtb1timer.enabled = 1: saveball
	end If

	if L22.state = 2 Then				'launch TB3
		vpmtimer.addtimer 9500, "openTB2 3, 1'"
		ballsaver.interval=15000
		l13.state = 0: L14.state = 0: l22.state = 1: launchtb3timer.enabled = 1: saveball
	end If

	if L18.state = 2 and l13.state = 2 and L14.state = 2 Then				'Thunderbirds are go
		opentb2 6, 1
		l29.state=2:l30.state=2:l31.state=2
		l13.state = 0: L14.state = 0
	end If

	if Multiballon = 1 Then												'spells International Rescue
		IRdisplay
	end if

	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub openTB2 (podnum, play)
		if rampupdown.enabled then :exit sub :end if
		if play = 1 then 
			if l19.state <> 2 then playsoundat "launch TB2", opto end if
			createsplash "TB2pod", "TB2podVID", "select pod5.gif", "TB2podtext", "TB2 pod.png", 77, 110, 2000
			vpmtimer.addtimer 2000, "createsplash ""TB2launch"", ""TB2VID0"", ""TB2 launch 1.gif"", ""clear"", ""clear.png"", 100, 140, 3000 '"
		end if
	
		poddoor.image = "pod"&podnum: rampupdown.enabled = 1 :playsoundat "hydrolic2", opto
		vpmtimer.addtimer 5000, "DMDBigText ""RAMP DOWN"",117,1 '"
		l29.state=2:l30.state=2:l31.state=2
end sub

Sub ramptrigger2_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger2
End Sub

Sub balltrapped()

if int(TB2ramp.rotx) =< 95 then
	ramp15timer.enabled = 1	
end if

end sub

Dim rampup15: rampup15 = true

Sub ramp15timer_timer()

if rampup15 and TB2ramp.rotx<115 then 
	TB2ramp.rotx = TB2ramp.rotx + 2
	TB2ramp.rotz = TB2ramp.rotz - 0.5
	Ramp15.collidable = 0
end if

if not rampup15 and TB2ramp.rotx>95 then 
	TB2ramp.rotx = TB2ramp.rotx - 0.5
	TB2ramp.rotz = TB2ramp.rotz + 0.125
end if

if int(TB2ramp.rotx) => 115 then rampup15 = false: TB2ramp.rotx = 115: end if
if int(TB2ramp.rotx) =< 95 then Ramp15.collidable = 1: TB2ramp.rotx = 95:TB2ramp.rotz = -1: rampup15 = true : ramp15timer.enabled = 0: exit sub: end if

end sub

'********************************************
'  Targets
'********************************************

Sub sw11_Hit							'HB1
	Addscore 1000
	TargetBouncer Activeball, 1
	shootingdouble sw11
	if multiballon = 0 then l27.state = 1: checkHBcomplete: end if
End Sub

Sub sw12_Hit							'HB2
	Addscore 1000
	shootingdouble sw12
	TargetBouncer Activeball, 1
	
		if multiballon = 0 then l28.state = 1: checkHBcomplete: end if			
End Sub

Sub sw13_Hit							'FAB1
	Addscore 1000
	playsoundat "door chimes", sw13
	TargetBouncer Activeball, 1

	if multiballon = 0 then

	if l1.state = 2 and l16.state <> 2 then l2.state=0:l3.state=0:l4.state=0:l5.state=0:l1.state = 0: Flash2 True: bigpoints: createsplash "FAB1", "FAB1VID5", "FAB1-8.gif", "clear", "clear.png", 100, 140, 3000: exit sub: end If		'add bonus for completing
	if l1.state = 2 and l16.state = 2 then 
		bigpoints
		Flash2 True
		l1.state = 0:l2.state=0:l3.state=0:l4.state=0:l5.state=0:l1.state = 0:
		playsoundat "fab", sw13
		createsplash "FAB1", "FAB1VID5", "FAB1-8.gif", "clear", "clear.png", 100, 140, 3000
		vpmtimer.addtimer 1500, "playsoundat ""TB1 underway in 30sec"", sw13 '"
		vpmtimer.addtimer 3000, "DMDBigText ""LAUNCH TB1"",117,1 '"
		l21.state = 2: l13.state = 2: L14.state = 2
		exit sub
	end if

	if l16.state = 2 then 
		if l5.state = 2 and l4.state = 1 then l5.state = 1: l1.state = 2: createsplash "FAB1", "FAB1VID4", "FAB1-7.gif", "FAB1text4", "FAB1text4.png", 100, 140, 3000:end if
		if l3.state = 1 and l4.state = 2 then l4.state = 1:createsplash "FAB1", "FAB1VID3", "FAB1-6.gif", "FAB1text3", "FAB1text3.png", 100, 140, 3000: end if
		if l2.state = 1 and l3.state = 2 then l3.state = 1:createsplash "FAB1", "FAB1VID2", "FAB1-5.gif", "FAB1text2", "FAB1text2.png", 100, 140, 3000: end if
		if l2.state = 2 then l2.state = 1:createsplash "FAB1", "FAB1VID1", "FAB1-4.gif", "FAB1text1", "FAB1text1.png", 100, 140, 3000: end if
	end If

	if l16.state <> 2 and multiballon = 0 then 
		if l5.state = 0 and l4.state = 1 then l5.state = 1: l1.state = 2: createsplash "FAB1", "FAB1VID4", "FAB1-7.gif", "FAB1text4", "FAB1text4.png", 100, 140, 3000: end if
		if l3.state = 1 and l4.state = 0 then l4.state = 1:createsplash "FAB1", "FAB1VID3", "FAB1-6.gif", "FAB1text3", "FAB1text3.png", 100, 140, 3000: end if
		if l2.state = 1 and l3.state = 0 then l3.state = 1:createsplash "FAB1", "FAB1VID2", "FAB1-5.gif", "FAB1text2", "FAB1text2.png", 100, 140, 3000: end if
		if l2.state = 0 then l2.state = 1:createsplash "FAB1", "FAB1VID1", "FAB1-4.gif", "FAB1text1", "FAB1text1.png", 100, 140, 3000: end if
	end if
	if l1.state = 2 then vpmtimer.addtimer 3000, "DMDBigText ""FAB1 READY"",77,1 '"

	end If

End Sub

Sub sw14_Hit							'"F" but change to "T"
	Addscore 1000
	shootingdouble sw14
	TargetBouncer Activeball, 1	
	
	if multiballon = 0 then
	if l24.state <> 1 then
		if l25.state <>1 and l26.state<>1 then
			createsplash "TB2", "TB2VID1", "TB2 launch 2.gif", "TB2text1", "TB2 - T.png", 100, 140, 3000
		elseif l25.state <>1 and l26.state=1 then
			createsplash "TB2", "TB2VID2", "TB2 launch 3.gif", "TB2text4", "TB2 - T2.png", 100, 140, 3000
		Elseif l25.state =1 and l26.state<>1 then
			createsplash "TB2", "TB2VID2", "TB2 launch 3.gif", "TB2text5", "TB2 - TB.png", 100, 140, 3000
		else
			createsplash "TB2", "TB2VID3", "TB2 launch 4.gif", "TB2text7", "TB2 - TB2.png", 100, 140, 3000
		end if
	end if
	l24.state = 1: checktb2complete

	end if
End Sub

Sub sw15_Hit							'"A" but change to "B"
	Addscore 1000
	shootingdouble sw15
	TargetBouncer Activeball, 1

	if multiballon = 0 then	
	if l25.state <> 1 and multiballon = 0 then
		if l24.state <>1 and l26.state<>1 then
			createsplash "TB2", "TB2VID1", "TB2 launch 2.gif", "TB2text2", "TB2 - B.png", 100, 140, 3000
		elseif l24.state <>1 and l26.state=1 then
			createsplash "TB2", "TB2VID2", "TB2 launch 3.gif", "TB2text6", "TB2 - B2.png", 100, 140, 3000
		Elseif l24.state =1 and l26.state<>1 then
			createsplash "TB2", "TB2VID2", "TB2 launch 3.gif", "TB2text5", "TB2 - TB.png", 100, 140, 3000
		else
			createsplash "TB2", "TB2VID3", "TB2 launch 4.gif", "TB2text7", "TB2 - TB2.png", 100, 140, 3000
		end if
	end if
	l25.state = 1: checktb2complete	
	end if
End Sub

Sub sw16_Hit							'"B" but change to "2"
	Addscore 1000
	shootingdouble sw16
	TargetBouncer Activeball, 1

	if multiballon = 0 then
	if l26.state <> 1 and multiballon = 0 then
		if l24.state <>1 and l25.state<>1 then
			createsplash "TB2", "TB2VID1", "TB2 launch 2.gif", "TB2text3", "TB2 - 2.png", 100, 140, 3000
		elseif l24.state <>1 and l25.state=1 then
			createsplash "TB2", "TB2VID2", "TB2 launch 3.gif", "TB2text6", "TB2 - B2.png", 100, 140, 3000
		Elseif l24.state =1 and l25.state<>1 then
			createsplash "TB2", "TB2VID2", "TB2 launch 3.gif", "TB2text4", "TB2 - T2.png", 100, 140, 3000
		else
			createsplash "TB2", "TB2VID3", "TB2 launch 4.gif", "TB2text7", "TB2 - TB2.png", 100, 140, 3000
		end if
	end if
	l26.state = 1: checktb2complete		
	end if
End Sub

Sub sw17_Hit							' Mole captive ball
	if sw17.timerenabled then
		exit sub 
	else 
		sw17.timerenabled = 1 
	end if

	Addscore 1000
	TargetBouncer Activeball, 1
	Flash2 True	

	if multiballon = 0 then
	if l23.state = 1 then 
		bigpoints
		l23.state = 0
		movemole.enabled = 1
		playsoundat "drilling", sw17
		createsplash "Mole", "MOLEVID3", "mole stage 3.gif", "Moletext3", "Moletext3.png", 88, 140, 2800
		if l19.state = 2 then
			playsoundat "launch TB2 - need mole", sw17
			vpmtimer.addtimer 3000, "openTB2 5, 1'"


			l29.state=2:l30.state=2:l31.state=2:
		end if
	end if
	if l23.state = 2 then 
		l23.state = 1: movemole.enabled = 1:  playsoundat "opening", sw17:
		createsplash "Mole", "MOLEVID2", "mole stage 2.gif", "Moletext2", "Moletext2.png", 88, 140, 2800
	end if	
	if l23.state = 0 and molestage = 1 then 
		l23.state = 2: movemole.enabled = 1: playsoundat "the mole", sw17 : playsoundat "opening", sw17:
		createsplash "Mole", "MOLEVID1", "mole stage 1.gif", "Moletext1", "Moletext1.png", 88, 140, 2800
	end if	
	end if
End Sub

Sub sw17_timer()
	sw17.timerenabled = 0
end sub

sub checktb4complete()

dim picktrack
picktrack = Int(2*Rnd)

if l6.state = 1 and l7.state = 1 and l8.state = 1 then 
	bigpoints
	Flash2 True
	if l15.state = 2 then 
		if picktrack =0 then playsoundat "launching TB4-1", sw6 else playsoundat "launching TB4-2", sw6 : vpmtimer.addtimer 3000, "DMDBigText ""TB4 LAUNCHED"",57,1 '": end if
		vpmtimer.addtimer 4000, "openTB2 4, 1'"
		l29.state=2:l30.state=2:l31.state=2
		l6.state = 0 : l7.state = 0 : l8.state = 0
	Else
		l6.state = 0 : l7.state = 0 : l8.state = 0 					' reward for completing TB4
	end if
end If

end sub

sub checktb2complete()

if l24.state = 1 and l25.state = 1 and l26.state = 1 then 
bigpoints
Flash2 True
	if l17.state = 2 then 
		vpmtimer.addtimer 4000, "openTB2 2, 1'"
		l29.state=2:l30.state=2:l31.state=2:
		l24.state = 0 : l25.state = 0 : l26.state = 0
	Else
		l24.state = 0 : l25.state = 0 : l26.state = 0 					' reward for completing TB2
	end if

end if

end sub

sub checkHBcomplete()
	if l27.state = 1 and l28.state = 1 Then
		Flash2 True
		bigpoints
		l27.state = 0: l28.state = 0

		if l36.state <> 1 and Multiballon = 0 then
			extraballtimer.enabled=1
			l10.state = Int(2*rnd): if l10.state = 2 then l10.state = 1 end if
			if l10.state = 1 then l11.state = 0 else l11.state = 1 end if
			createsplash "Alert", "siren", "siren.gif", "EBlit", "extraball is lit.png", 10, 50, 2700
			playsoundat "siren short", l36
'			DMDBigText "EXTRABALL LIT",117,1
		end if
	end if
end sub

sub extraballtimer_timer()
		l10.state = 0
		l11.state = 0
		extraballtimer.enabled = 0
end sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
'									'2 = flasher image shadow, but it moves like ninuzzu's
'Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
'Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
'Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
'Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
'Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
'Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		rolling(b) = False
'		StopSound("BallRoll_" & b)
'	Next
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****** End Part B:  Code and Functions ******

'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next
end sub

Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For Each Source in DynamicSources
					LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
					If LSd < falloff and Source.state=1 Then	    			'If the ball is within the falloff range of a light and light is on
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = source.name
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							set AnotherSource = Eval(sourcenames(s))
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'*******************************************
'  VPW Rubberizer by Iaakki
'*******************************************

' iaakki Rubberizer
sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

' apophis Rubberizer
sub Rubberizer2(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |

'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5
	AddPt "Polarity", 2, 0.4, -5
	AddPt "Polarity", 3, 0.6, -4.5
	AddPt "Polarity", 4, 0.65, -4.0
	AddPt "Polarity", 5, 0.7, -3.5
	AddPt "Polarity", 6, 0.75, -3.0
	AddPt "Polarity", 7, 0.8, -2.5
	AddPt "Polarity", 8, 0.85, -2.0
	AddPt "Polarity", 9, 0.9,-1.5
	AddPt "Polarity", 10, 0.95, -1.0
	AddPt "Polarity", 11, 1, -0.5
	AddPt "Polarity", 12, 1.1, 0
	AddPt "Polarity", 13, 1.3, 0

	addpt "Velocity", 0, 0,         1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41,         1.05
	addpt "Velocity", 3, 0.53,         1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03,         0.945

	LF.Object = LeftFlipper        
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0000001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Dim balltrappedcount: balltrappedcount = 0

Dim ampFactor

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
	Select Case BallRollAmpFactor
		Case 0
			ampFactor = "_amp0"
		Case 1
			ampFactor = "_amp2_5"
		Case 2
			ampFactor = "_amp5"
		Case 3
			ampFactor = "_amp7_5"
		Case 4
			ampFactor = "_amp9"
		Case Else
			ampFactor = "_amp0"
	End Select
End Sub

Sub RollingUpdate()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b & ampFactor)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b & ampFactor)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If

		if multiballon = 0 then
			if BOT(b).X > 560 and BOT(b).X < 700 and BOT(b).Y > 500 and BOT(b).Y < 650 and BOT(b).Z > -1 and BOT(b).Z < 49 then
				if abs(BOT(b).Velx) < 0.5 and abs(BOT(b).Vely) < 0.5 then
					balltrappedcount = balltrappedcount + 1
					if balltrappedcount>150 then balltrapped()
				Else
					balltrappedcount = 0
				end if
			end if
		end if
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

dim rampAmpFactor

InitRampRolling

Sub InitRampRolling()
	Select Case RampRollAmpFactor
		Case 0
			rampAmpFactor = "_amp0"
		Case 1
			rampAmpFactor = "_amp2_5"
		Case 2
			rampAmpFactor = "_amp5"
		Case 3
			rampAmpFactor = "_amp7_5"
		Case 4
			rampAmpFactor = "_amp9"
		Case Else
			rampAmpFactor = "_amp0"
	End Select
End Sub

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x & rampAmpFactor)
				Else
					StopSound("RampLoop" & x & rampAmpFactor)
					PlaySound("wireloop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x & rampAmpFactor)
			StopSound("wireloop" & x & rampAmpFactor)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	PlaySoundAtLevelStatic SoundFX("highpitch1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	PlaySoundAtLevelStatic SoundFX("highpitch1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	PlaySoundAtLevelStatic SoundFX("highpitch1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.
'
' NOTE: The below timer is for flashing the inserts as a demonstration of Lampz. Should be replaced by actual lamp states.
'       In other words, delete this sub (InsertFlicker_timer) and associated timer if you are going to use Lampz with a ROM.
dim flickerX, FlickerState : FlickerState = 0
Sub InsertFlicker_timer
	'msgbox Lampz.Obj(1).name
	'	for flickerX = 0 to 140 
	'		lampz.state(flickerX)
	'lampz.state(flickerX)
	if FlickerState = 0 then
		for flickerX = 0 to 41 : lampz.state(flickerX)=false : next
		FlickerState = 1
	Else
		for flickerX = 0 to 41 : lampz.state(flickerX)=true : next
		FlickerState = 0
	end if
End Sub

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
'InitLampsNF              ' Setup lamp assignments
'LampTimer.Interval = -1
'LampTimer.Enabled = 1
'InsertFlicker.enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	if UsingROM then chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub

'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1)= l1
	Lampz.Callback(1) = "DisableLighting p1, 200,"
	Lampz.MassAssign(2)= l2
	Lampz.Callback(2) = "DisableLighting p2, 200,"
	Lampz.MassAssign(3)= l3
	Lampz.Callback(3) = "DisableLighting p3, 200,"
	Lampz.MassAssign(4)= l4
	Lampz.Callback(4) = "DisableLighting p4, 200,"
	Lampz.MassAssign(5)= l5
	Lampz.Callback(5) = "DisableLighting p5, 200,"

	Lampz.MassAssign(6)= l6
	Lampz.Callback(6) = "DisableLighting p6, 200,"
	Lampz.MassAssign(7)= l7
	Lampz.Callback(7) = "DisableLighting p7, 200,"
	Lampz.MassAssign(8)= l8
	Lampz.Callback(8) = "DisableLighting p8, 200,"
	Lampz.MassAssign(9)= l9
	Lampz.Callback(9) = "DisableLighting p9, 200,"
	Lampz.MassAssign(10)= l10
	Lampz.Callback(10) = "DisableLighting p10, 200,"

	Lampz.MassAssign(11)= l11
	Lampz.Callback(11) = "DisableLighting p11, 200,"
	Lampz.MassAssign(12)= l12
	Lampz.Callback(12) = "DisableLighting p12, 130,"
	Lampz.MassAssign(13)= l13
	Lampz.Callback(13) = "DisableLighting p13, 250,"
	Lampz.MassAssign(14)= l14
	Lampz.Callback(14) = "DisableLighting p14, 200,"
	Lampz.MassAssign(15)= l15
	Lampz.Callback(15) = "DisableLighting p15, 200,"
	Lampz.MassAssign(16)= l16
	Lampz.Callback(16) = "DisableLighting p16, 200,"
	Lampz.MassAssign(17)= l17
	Lampz.Callback(17) = "DisableLighting p17, 250,"
	Lampz.MassAssign(18)= l18
	Lampz.Callback(18) = "DisableLighting p18, 50,"
	Lampz.MassAssign(19)= l19
	Lampz.Callback(19) = "DisableLighting p19, 200,"
	Lampz.MassAssign(20)= l20
	Lampz.Callback(20) = "DisableLighting p20, 50,"
	Lampz.MassAssign(21)= l21
	Lampz.Callback(21) = "DisableLighting p21, 60,"
	Lampz.MassAssign(22)= l22
	Lampz.Callback(22) = "DisableLighting p22, 60,"
	Lampz.MassAssign(23)= l23
	Lampz.Callback(23) = "DisableLighting p23, 200,"
	Lampz.MassAssign(24)= l24
	Lampz.Callback(24) = "DisableLighting p24, 200,"
	Lampz.MassAssign(25)= l25
	Lampz.Callback(25) = "DisableLighting p25, 200,"
	Lampz.MassAssign(26)= l26
	Lampz.Callback(26) = "DisableLighting p26, 200,"
	Lampz.MassAssign(27)= l27
	Lampz.Callback(27) = "DisableLighting p27, 250,"
	Lampz.MassAssign(28)= l28
	Lampz.Callback(28) = "DisableLighting p28, 50,"
	Lampz.MassAssign(29)= l29
	Lampz.Callback(29) = "DisableLighting p29, 200,"
	Lampz.MassAssign(30)= l30
	Lampz.Callback(30) = "DisableLighting p30, 50,"
	Lampz.MassAssign(31)= l31
	Lampz.Callback(31) = "DisableLighting p31, 200,"
	Lampz.MassAssign(32)= l32
'	Lampz.Callback(32) = "DisableLighting p32, 130,"
	Lampz.MassAssign(33)= l33
'	Lampz.Callback(33) = "DisableLighting p33, 250,"
	Lampz.MassAssign(34)= l34
'	Lampz.Callback(34) = "DisableLighting p34, 200,"
	Lampz.MassAssign(35)= l35
'	Lampz.Callback(35) = "DisableLighting p35, 200,"
	Lampz.MassAssign(36)= l36
'	Lampz.Callback(36) = "DisableLighting p36, 200,"
	Lampz.MassAssign(37)= l37
'	Lampz.Callback(37) = "DisableLighting p37, 200,"
	Lampz.MassAssign(38)= l38
'	Lampz.Callback(38) = "DisableLighting p38, 200,"
	Lampz.MassAssign(39)= l39
'	Lampz.Callback(39) = "DisableLighting p39, 200,"
	Lampz.MassAssign(40)= l40
'	Lampz.Callback(40) = "DisableLighting p40, 200,"
	Lampz.MassAssign(41)= l41
'	Lampz.Callback(41) = "DisableLighting p41, 200,"

	'Flasher Assignments
'	Lampz.Callback(31)= "Flash1"
	Lampz.Callback(32)= "Flash2"
'	Lampz.Callback(33)= "Flash3"
'	Lampz.Callback(34)= "Flash4"

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub

'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************

'******************************************************
'*****   3D INSERTS
'******************************************************
'
'
' Before you get started adding the inserts to your playfield in VPX, there are a few things you need to have done to prepare:
' 	1. Cut out all the inserts on the playfield image so there is alpha transparency where they should be.
'      Make sure the playfield material has Opacity Active checkbox checked.
'	2. All the  insert text and/or images that lie over the insert plastic needs to be in its own file with 
'	   alpha transparency. Many playfields may require finding the original font and remaking the insert text.
' 
' To add the inserts:
'	1. Import all the textures (images) and materials from this file that start with the word "Insert" into your Table
'   2. Copy and past the two primitves that make up the insert you want to use. One primitive is for the on state, the other for the off state.
'   3. Alight the primitives with the associated insert light. Name the on and off primitives correctly.
'   4. Update the Lampz object array. Follow the example in this file.
'   5. You will need to manually tweak the disable lighting value and material parameters to achielve the effect you want.
'
'
' Quick Reference:  Laying the Inserts ( Tutorial From Iaakki)
' - Each insert consists of two primitives. On and Off primitive. Suggested naming convention is to use lamp number in the name. For example 
'   is lamp number is 57, the On primitive is "p57" and the Off primitive is "p57off". This makes it easier to work on script side.
' - When starting from a new table, I'd first select to make few inserts that look quite similar. Lets say there is total of 6 small triangle 
'   inserts, 4 yellow and 2 blue ones.
' - Import the insert on/off images from the image manager and the vpx materials used from the sample project first, and those should appear 
'   selected properly in the primitive settings when you paste your actual insert trays in your target table . Then open up your target project 
'   at same time as the sample project and use copy&paste to copy desired inserts to target project. 
' - There are quite many parameters in primitive that affect a lot how they will look. I wouldn't mess too much with them. Use Size options to 
'   scale the insert properly into PF hole. Some insert primitives may have incorrect pivot point, which means that changing the depth, you may 
'   also need to alter the Z-position too.
' - Once you have the first insert in place, wire it up in the script (detailed in part 3 below). Then set the light bulb's intensity to zero, 
'   so it won't harass the adjustment.
' - Start up the game with F6 and visually see if the On-primitive blinks properly. If it is too dim, hit D and open editor. Write: 
' - p57.BlendDisableLighting = 300 and hit enter
' - -> The insert should appear differently. Find good looking brightness level. Not too bright as the light bulb is still missing. Just generic good light.
'     - If you cannot find proper light color or "mood", you can also fiddle with primitive material values. Provided material should be 
'       quite ok for most of the cases.
'     - Now when you have found proper DL value (165), but that into script:
'     - Lampz.Callback(57) = " DisableLighting p57, 165,"
' - That one insert is now adjusted and you should be able to copy&paste rest of the triangle inserts in place and name them correctly. And add them 
'   into script. And fine tune their brightness and color.
'
' Light bulbs and ball reflection:
' 
' - This kind of lighted primitives are not giving you ball reflections. Also some more glow vould be needed to make the insert to bloom correctly.
' - Take the original lamp (l57), set the bulb mode enabled, set Halo Height to -3 (something that is inside the 2 insert primitives). I'd start with 
'   falloff 100, falloff Power 2-2.5, Intensity 10, scale mesh 10, Transmit 5.
' - Start the game with F6, throw a ball on it and move the ball near the blinking insert. Visually see how the reflection looks.
' - Hit D once the reflection is the highest. Open light editor and start fine tuning the bulb values to achieve realistic look for the reflection.
' - Falloff Power value is the one that will affect reflection creatly. The higher the power value is, the brighter the reflection on the ball is. 
'   This is the reason why falloff is rather large and falloff power is quite low. Change scale mesh if reflection is too small or large.
' - Transmit value can bring nice bloom for the insert, but it may also affect to other primitives nearby. Sometimes one need to set transmit to 
'   zero to avoid affecting surrounding plastics. If you really need to have higher transmit value, you may set Disable Lighting From Below to 1 
'   in surrounding primitive. This may remove the problem, but can make the primitive look worse too.

'******************************************************
'*****   END 3D INSERTS
'******************************************************

'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDoms2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below 
' Only use InitFlasher and RotateFlasher for the flasher(s) in your table
' Align the FlasherFlash object by setting TestFlashers = 1 (below in the script)
' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - The init script moves the flasherlit primitive on the exact same spot and rotation as the flasherbase
' - The flasherbase position determines the position of the flasher you will see when playing the table
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
'   (I have used animated primitives to rotate the "handles" of the flashers)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If the flasherbase is parallel to the playfield, RotZ is automatically set for the flasherflash object
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below
' - You need to manually position the VPX lights at the correct position and height (for instance just above a plastic)

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

' example script for rom based tables (non modulated):

' SolCallback(25)="FlashRed"
'
' Sub FlashRed(flstate)
'	If Flstate Then
'		Objlevel(1) = 1 : FlasherFlash1_Timer
'	End If
' End Sub

' example script for rom based tables (modulated):

' SolModCallback(25)="FlashRed"
'
' Sub FlashRed(level)
'	Objlevel(1) = level/255 : FlasherFlash1_Timer
' End Sub

' Sub Flash1(Enabled)
'	If Enabled Then
'		Objlevel(1) = 1 : FlasherFlash1_Timer
'	End If
' End Sub

 Sub Flash2(Enabled)
	If Enabled Then
		if palm1.rotz = 0 then palmtilt = false: shakepalm.enabled = 1 : end if
		Objlevel(2) = 1 : FlasherFlash2_Timer
	End If
 End Sub

 'Sub Flash3(Enabled)
	'If Enabled Then
'		Objlevel(3) = 1 : FlasherFlash3_Timer
'	End If
' End Sub

 'Sub Flash4(Enabled)
	'If Enabled Then
'		Objlevel(4) = 1 : FlasherFlash4_Timer
'	End If
' End Sub

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.3		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

'InitFlasher 1, "green"
InitFlasher 2, "red"
'InitFlasher 3, "blue"
'InitFlasher 4, "white"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

'Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
'Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
'Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 

'******************************************************
'******  END FLUPPER DOMES
'******************************************************

'******************************************************
'******  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

' Explanation of how these bumpers work:
' There are 10 elements involved per bumper:
' - the shadow of the bumper ( a vpx flasher object)
' - the bumper skirt (primitive)
' - the bumperbase (primitive)
' - a vpx light which colors everything you can see through the bumpertop
' - the bulb (primitive)
' - another vpx light which lights up everything around the bumper
' - the bumpertop (primitive)
' - the VPX bumper object
' - the bumper screws (primitive)
' - the bulb highlight VPX flasher object
' All elements have a special name with the number of the bumper at the end, this is necessary for the fading routine and the initialisation.
' For the bulb and the bumpertop there is a unique material as well per bumpertop.
' To use these bumpers you have to first copy all 10 elements to your table.
' Also export the textures (images) with names that start with "Flbumper" and "Flhighlight" and materials with names that start with "bumper".
' Make sure that all the ten objects are aligned on center, if possible with the exact same x,y coordinates
' After that copy the script (below); also copy the BumperTimer vpx object to your table
' Every bumper needs to be initialised with the FlInitBumper command, see example below; 
' Colors available are red, white, blue, orange, yellow, green, purple and blacklight.
' In a GI subroutine you can then call set the bumperlight intensity with the "FlBumperFadeTarget(nr) = value" command
' where nr is the number of the bumper, value is between 0 (off) and 1 (full on) (so you can also use 0.3 0.4 etc).

' Notes:
' - There is only one color for the disk; you can photoshop it to a different color
' - The bumpertops are angle independent up to a degree; my estimate is -45 to + 45 degrees horizontally, 0 (topview) to 70-80 degrees (frontview)
' - I built in correction for the day-night slider; this might not work perfectly, depending on your table lighting
' - These elements, textures and materials do NOT integrate with any of the lighting routines I have seen in use in many VPX tables
'   (just find the GI handling routine and insert the FlBumperFadeTarget statement)
' - If you want to use VPX native bumperdisks just copy my bumperdisk but make it invisible

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "red"
'FlInitBumper 2, "white"
FlInitBumper 3, "blue"
'FlInitBumper 4, "orange"
FlInitBumper 5, "yellow"

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub
'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************

'**********************************************************************************************************
'* InstructionCard *
'**********************************************************************************************************

Dim CardCounter, ScoreCard
Sub CardTimer_Timer
        If scorecard=1 Then
                CardCounter=CardCounter+2
                If CardCounter>50 Then CardCounter=50
        Else
                CardCounter=CardCounter-4
                If CardCounter<0 Then CardCounter=0
        End If
        InstructionCard.transX = CardCounter*6
        InstructionCard.transY = CardCounter*6
        InstructionCard.transZ = -cardcounter*2
'        InstructionCard.objRotX = -cardcounter/2
        InstructionCard.size_x = 1+CardCounter/25
        InstructionCard.size_y = 1+CardCounter/25
        If CardCounter=0 Then 
                CardTimer.Enabled=False
                InstructionCard.visible=0
        Else
                InstructionCard.visible=1
        End If 
End Sub

'***** Moving target *****
Dim movetarget, movedirection', movetargetup, movetargetupcount	:movetargetup=0: movetargetupcount=0
dim movingt, pause

	for movingt = 0 to 49
		Collection2(movingt).Visible=0
		Collection2(movingt).collidable=false
		Collection2(movingt).hashitevent=False
	Next

	CenterT50.visible=0
	CenterT50.Collidable=True
	CenterT50.Hashitevent=True

	movetarget=0

	movedirection=1
	pause = 0
	MovingTargetTimer.enabled=true
'	PtargetC.visible = 0
	PtargetC.objrotx= ((movetarget*0.66)-17)
	PtargetC.objroty= -1*((movetarget*0.66)-17)

'***********************
'     Moving Target
'***********************

sub MovingTargetTimer_timer
	if movedirection=1 Then	
		Collection2(movetarget).visible=0
		Collection2(movetarget).collidable=False
		Collection2(movetarget).hashitevent=False
		movetarget=movetarget+1
'		Collection2(movetarget).visible=1
		Collection2(movetarget).collidable=True
		Collection2(movetarget).hashitevent=True
		PtargetC.objrotx= ((movetarget*0.66)-17)
		PtargetC.objroty= -1*((movetarget*0.66)-17)

		if movetarget>48 then 
			movetarget=48
			pause = pause + 1 
				if pause = 20 Then
					pause = 0
					movetarget=49
					movedirection=0
				end if
		end If
		exit sub

	Else
		Collection2(movetarget).Visible=0
		Collection2(movetarget).collidable=False
		Collection2(movetarget).hashitevent=False
		
		movetarget=movetarget-1

'		Collection2(movetarget).visible=1
		Collection2(movetarget).collidable=True
		Collection2(movetarget).hashitevent=True
		PtargetC.objrotx= ((movetarget*0.66)-17)
		PtargetC.objroty= -1*((movetarget*0.66)-17)

		if movetarget<1 then 
			movetarget=1
			pause = pause + 1 
				if pause = 20 Then
					pause = 0
					movetarget=0
					movedirection=1
				end if
		end If
	end If

end sub 

'***********************************
dim swingstep

Sub Collection2_hit(idx)
	Addscore 1000
	shootingecho PtargetC
	PtargetC.TransY = -10	
	swingStep = 0
	swinger.Enabled = 1

if l20.state <> 2 and multiballon = 0 then
	if l34.state = 1 and l35.state = 0 then l35.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext4", "hood text-HOOD.png", 88, 110, 3000
	if l33.state = 1 and l34.state = 0 then l34.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext3", "hood text-HOO.png", 88, 110, 3000
	if l32.state = 1 and l33.state = 0 then l33.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext2", "hood text-HO.png", 88, 110, 3000
	if l32.state = 0 then l32.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext1", "hood text-H.png", 88, 110, 3000
end if

if l20.state = 2 then
	if l34.state = 1 and l35.state = 2 then l35.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext4", "hood text-HOOD.png", 88, 110, 3000
	if l33.state = 1 and l34.state = 2 then l34.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext3", "hood text-HOO.png", 88, 110, 3000
	if l32.state = 1 and l33.state = 2 then l33.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext2", "hood text-HO.png", 88, 110, 3000
	if l32.state = 2 then l32.state = 1: createsplash "HOOD", "HOODVID", "hood.gif", "hoodtext1", "hood text-H.png", 88, 110, 3000
end if

if l20.state <> 2 and l35.state = 1 then 
	bigpoints
	Flash2 True
	l32.state=0:l33.state=0:l34.state=0:l35.state=0

end If

if l20.state = 2 and l35.state = 1 then 
	bigpoints
	Flash2 True
	l32.state=0:l33.state=0:l34.state=0:l35.state=0
	saveball
	playsoundat "hood foiled", PtargetC						'add bonus for completing
	vpmtimer.addtimer 3000, "DMDBigText ""HOOD DEFEATED"",57,1 '"
	vpmtimer.addtimer 3500, "incomingsignal 3'"
	vpmtimer.addtimer 6500, "playsoundat ""tb3 launch"", PtargetC '"
	vpmtimer.addtimer 6500, "DMDBigText ""LAUNCH TB3"",117,1 '"
	l22.state = 2: l13.state = 2: L14.state = 2
end if

end sub

sub swinger_Timer

	Select Case swingStep
		Case 3:PtargetC.TransY = -5
		Case 4:PtargetC.TransY = 0:swinger.Enabled = 0
	End Select

	swingStep = swingStep + 1

End Sub

dim calling:calling = 5
dim picture, signalcounter:signalcounter=0
' john=0, scott=1, virgil=2, alan=3, gordon=4, penny=5


Sub incomingsignal (displaypicture)

calling = displaypicture

	select case calling
		case 0: picture = "john": DMDBigText "JOHN CALLING",117,1
		case 1: picture = "scott": DMDBigText "SCOTT CALLING",117,1
		case 2: picture = "virgil": DMDBigText "VIRGIL CALLING",117,1
		case 3: picture = "alan": DMDBigText "ALAN CALLING",117,1
		case 4: picture = "gordon": DMDBigText "GORDON CALLING",117,1
		case 5: picture = "penny": DMDBigText "PENNY CALLING",117,1
	end Select

incomingsignaltimer.enabled = 1

end Sub

Sub incomingsignaltimer_timer()

signalcounter = signalcounter + 1

	select Case signalcounter
		case 1: PlaySound "incoming", 0, 1, 0, 0, 0:pictureframe(calling).image= picture&"on"
		case 646: pictureframe(calling).image= picture&"off"
		case 1123: pictureframe(calling).image= picture&"on"
		case 1805: pictureframe(calling).image= picture&"off"
		case 2280: pictureframe(calling).image= picture&"on"
		case 2923: pictureframe(calling).image= picture&"off": signalcounter = 0: incomingsignaltimer.enabled = 0
	end select

end Sub

'TB2 ramp down
' RotX = 95, RotY = 14, RotZ = -1
'TB2 ramp up
' RotX = 115, RotY = 14, RotZ = -6

Dim rampup: rampup = true

sub rampupdown_timer()

if TB2updown.enabled or poddoortimer.enabled then exit sub end if

if rampup then 
	TB2ramp.rotx = TB2ramp.rotx + 1
	TB2ramp.rotz = TB2ramp.rotz - 0.25
	Ramp15.collidable = 0
end if

if not rampup then 
	TB2ramp.rotx = TB2ramp.rotx - 0.5
	TB2ramp.rotz = TB2ramp.rotz + 0.125
	
end if

if int(TB2ramp.rotx) =< 95 then 
	TB2ramp.rotx = 95: Ramp15.collidable = 1: rampupdown.enabled = 0: rampup = true: TB2updown.enabled = 1
	if palm1.rotz = 0 then palmtilt = true: shakepalm.enabled = 1: end if
end if

if int(TB2ramp.rotx) => 115 then 
	TB2ramp.rotx = 115: rampupdown.enabled = 0: rampup = false: poddoortimer.enabled = 1
	if palm1.rotz = -20 then palmtilt = true: shakepalm.enabled = 1: end if
end if

end sub

Dim TB2down: tb2down = true

sub TB2updown_timer()


'TB2body z=0 (down), z=60 (Up)
'TB2legs TB2legs.size_y = 5 (Update) = 0.1 (down)

TB2legs.visible = 1

if not tb2down then 
	TB2body.z = TB2body.z + 1
	TB2legs.size_y = TB2legs.size_y + (5/60)
end if

if tb2down then 
	TB2body.z = TB2body.z - 1
	TB2legs.size_y = TB2legs.size_y- (5/60)
end if

if int(TB2body.z) = 60 then TB2updown.enabled = 0: tb2down = true: poddoortimer.enabled = 1 : end if
if int(TB2body.z) = 0 then TB2updown.enabled = 0: tb2down = false: TB2legs.visible = 0:  end if

end sub

Dim poddooropen: poddooropen = false

sub poddoortimer_timer()

'poddoor.rotx = 90, roty = -80, rotz = 110 (open), rotz = 0 (closed),

if tb2down and not poddooropen then 
	poddoor.rotz = poddoor.rotz - 1
end If

if poddooropen then 
	
	if int(TB2body.z) < 60	then : TB2updown.enabled = 1: exit sub :end if
	poddoor.rotz = poddoor.rotz + 1
end If

if int(poddoor.rotz) = 0 then poddooropen = true: TB2updown.enabled = 1: poddoortimer.enabled = 0: exit sub: end if
if int(poddoor.rotz) = 110 then poddooropen = false: poddoortimer.enabled = 0: end if

end sub

Dim openpool: openpool = false
Dim tb1counter:tb1counter = 0

sub launchtb1timer_timer()

if tb1counter = 0 then startrocket 1

'TB1body.z = 0 (base of pool)

if not openpool and tracypoolsurface.x > 80 then 

	tracypoolsurface.x = tracypoolsurface.x - 1
'	tracypoolbase.x = tracypoolbase.x - 1
	tracypoolsurface.size_z = 0.98*tracypoolsurface.size_z
'	tracypoolbase.size_z = 0.98*tracypoolbase.size_z

end if

if not openpool and tracypoolsurface.x =< 80 then 
	tracypoolsurface.visible = 0
	TB1body.visible = 1
	rocket.visible = 1
'	tracypoolbase.visible = 0
	tb1counter = tb1counter + 1

	if tb1counter => 50 then 
		TB1body.z = TB1body.z + 10
		rocket.height = rocket.height + 10
		if TB1body.z => 3500 then
			TB1body.visible = 0
			rocket.visible = 0:rocket.imagea = "clear": rocket.timerenabled = 0
			rocket.height = rocket.height - (tb1counter-49)*10
'			launchtb1timer.enabled = 0
			openpool = true
			tb1counter = 0
			exit sub
		end if
	end if
end if

if openpool then 
	tracypoolsurface.visible = 1
'	tracypoolbase.visible = 1
	tracypoolsurface.x = tracypoolsurface.x + 1
'	tracypoolbase.x = tracypoolbase.x + 1
	tracypoolsurface.size_z = (1/0.98)*tracypoolsurface.size_z
'	tracypoolbase.size_z = (1/0.98)*tracypoolbase.size_z

	if tracypoolsurface.x => 150 then 
		tracypoolsurface.size_z = 75
'		tracypoolbase.size_z = 75
		TB1body.visible = 1
		TB1body.z = 0
		launchtb1timer.enabled = 0
		openpool = false
		exit sub
	end if
end If

end sub

Dim tb3counter:tb3counter = 0

sub launchtb3timer_timer()

	if tb3counter = 0 then startrocket 3
	'TB3.z = -180 (base of pool)
	TB3.visible = 1
	rocket.visible = 1
	tb3counter = tb3counter + 1

	if tb3counter = 1 then :playsoundat "1. standby for blastoff TB3", sw13: DMDBigText "STANDBY",117,1: end if
	if tb3counter =300 then :playsoundat "2. blast off TB3", sw13: end if
	if tb3counter =750 then :playsoundat "3. lift off TB3", sw13: DMDBigText "LIFT OFF",117,1: end if
	if tb3counter =900 then :playsoundat "4. TB3 lift off sound", sw13: end if
	if tb3counter => 900 then 
		TB3.z = TB3.z + 10
		rocket.height = rocket.height + 10
		if TB3.z => 3500 then
			stopsound "4. TB3 lift off sound"
			TB3.visible = 0
			rocket.visible = 0:rocket.imagea = "clear": rocket.timerenabled = 0
			rocket.height = rocket.height - (tb1counter-49)*10
			launchtb3timer.enabled = 0
			tb3counter = 0
			TB3.z = -180
		end if
	end if

end sub

dim imageselect: 

sub startrocket(tb)
	if tb = 3 then imageselect = 25: rocket.height =  - (500 - 260) : rocket.x = 164: end if
	if tb = 1 then imageselect = 1: rocket.height =  -(500 - 255): rocket.x = 150: end if
	rocket.timerenabled = 1
end sub

Sub rocket_timer()

select case imageselect
case 1: rocket.imagea = "rocket" & imageselect
case 2: rocket.imagea = "rocket" & imageselect
case 3: rocket.imagea = "rocket" & imageselect
case 4: rocket.imagea = "rocket" & imageselect
case 5: rocket.imagea = "rocket" & imageselect
case 6: rocket.imagea = "rocket" & imageselect
case 7: rocket.imagea = "rocket" & imageselect
case 8: rocket.imagea = "rocket" & imageselect
case 25: rocket.imagea = "rocket" & imageselect
case 26: rocket.imagea = "rocket" & imageselect
case 27: rocket.imagea = "rocket" & imageselect
case 28: rocket.imagea = "rocket" & imageselect
case 29: rocket.imagea = "rocket" & imageselect
end Select

imageselect = imageselect + 1
if imageselect = 8 then imageselect = 1 
if imageselect = 29 then imageselect = 25 

end sub


Dim palmplus: palmplus = 1
dim palmcounter: palmcounter = 0
dim palmtilt: palmtilt = true

sub shakepalm_timer()

if palmtilt Then
	shakepalm.interval = 10
	palm1.rotz = palm1.rotz - 0.1*palmplus
	palm2.rotz = palm2.rotz - 0.1*palmplus
	palm3.rotz = palm3.rotz - 0.1*palmplus
	palm4.rotz = palm4.rotz - 0.1*palmplus
	if palm1.rotz < -20 and palmplus = 1 then
		palmplus = -1
		palm1.rotz = -20: palm2.rotz = -20: palm3.rotz = -20: palm4.rotz = -20
		shakepalm.enabled = 0
		exit Sub
	end If
	if palm1.rotz > 0 and palmplus = -1 then
		palmplus = 1
		palm1.rotz = 0: palm2.rotz = 0: palm3.rotz = 0: palm4.rotz = 0
		shakepalm.enabled = 0
		exit Sub
	end If
end if

if not palmtilt then
	shakepalm.interval = 2
	palmcounter = palmcounter + 1
	if palm1.rotz > 20 then palmplus = -1
	if palm1.rotz < -20 then palmplus = 1
	palm1.rotz = palm1.rotz + palmplus
	palm2.rotz = palm2.rotz + palmplus
	palm3.rotz = palm3.rotz + palmplus
	palm4.rotz = palm4.rotz + palmplus

	if palmcounter > 500 then 
		if int(palm1.rotz) = 0 then 
			palm1.rotz = 0
			palm2.rotz = 0
			palm3.rotz = 0
			palm4.rotz = 0
			shakepalm.enabled = 0
			palmcounter = 0
			palmplus = 1
			exit Sub
		end If
	end if

end if

end sub

Dim molestage: molestage = 1	'1 = move, 2 = tilt, 3 = rotate drill, 4 = reset 

sub movemole_timer()

'Molebase.x = 120:Molebase.y = 1115: Molebase.z = 150: 'Molebase.rotx = 90: 'Molebase.roty = 158
'Molebody.x = 120: Molebody.y = 1115: Molebody.z = 150: 'Molebody.rotx = 90: 'Molebody.roty = 158
'Moledrill.x = 120: Moledrill.y = 1115: Moledrill.z = 150: 'Moledrill.rotx = 90: 'Moledrill.roty = 158

' Moledrill.rotz = Moledrill.rotz + 1 ' rotates drill
'Molebody.rotx = 0
'Molebody.rotz = 45


if molestage = 1 then 
	Molebase.roty = Molebase.roty - 0.1
	Molebody.roty = Molebody.roty - 0.1
	Moledrill.roty = Moledrill.roty - 0.1
	if Molebody.roty =< 140 Then
		Molebase.roty = 140: Molebody.roty = 140: Moledrill.roty = 140
		molestage = 2
		movemole.enabled = 0
		exit Sub
	end if
end if

if molestage = 2 then 
	Molebody.rotx = Molebody.rotx - 0.1
	Moledrill.rotx = Moledrill.rotx - 0.1
	Molebody.roty = Molebody.roty + (0.1/5)
	Moledrill.roty = Moledrill.roty + (0.1/5)
	Molebody.rotz = Molebody.rotz + 0.05
	Moledrill.rotz = Moledrill.rotz + 0.05

	if Molebody.rotx =< 60 Then
		Molebody.rotx = 60: Molebody.roty = 146: Molebody.rotz = 15
		Moledrill.rotx = 60: Moledrill.roty = 146: Moledrill.rotz = 15
		molestage = 3
		movemole.enabled = 0
		exit Sub
	end if
end if

if molestage = 3 then 
	Moledrill.rotz = Moledrill.rotz + 3
	if Moledrill.rotz => 360 then Moledrill.rotz = Moledrill.rotz - 360
end If

if molestage = 4 then 
	if int(Moledrill.rotz)<>0 then 
		Moledrill.rotz = Moledrill.rotz + 1
		if Moledrill.rotz => 360 then Moledrill.rotz = Moledrill.rotz - 360
		exit sub
	end if
	if int(Moledrill.rotz) = 0 Then
			Moledrill.rotz = 0
			Molebody.rotx = Molebody.rotx + 0.1
			Moledrill.rotx = Moledrill.rotx + 0.1
			Molebody.roty = Molebody.roty - (0.1/5)
			Moledrill.roty = Moledrill.roty - (0.1/5)
			Molebody.rotz = Molebody.rotz - 0.05
			if Molebody.rotx => 90 Then
				Molebody.rotx = 90: Molebody.rotz = 0
				Moledrill.rotx = 90: Moledrill.rotz = 0
				Molebase.roty = Molebase.roty + 0.1
				Molebody.roty = Molebody.roty + 0.12
				Moledrill.roty = Moledrill.roty + 0.12
				if Molebase.roty => 158 Then
					Molebase.roty = 158: Molebody.roty = 158: Moledrill.roty = 158
					molestage = 1
					movemole.enabled = 0
					exit Sub
				end if
			end if
	end If
end if

end sub

'*** High Score Handling ***

sub loadhs

Dim FileObj
Dim ScoreFile
Set FileObj=CreateObject("Scripting.FileSystemObject")
If Not FileObj.FolderExists(UserDirectory) then 
	Exit Sub
End if
If Not FileObj.FileExists(UserDirectory & "thunderbirds.txt") then
	Exit Sub
End if
Set ScoreFile=FileObj.GetFile(UserDirectory & "thunderbirds.txt")
Set TextStr=ScoreFile.OpenAsTextStream(1,0)
If (TextStr.AtEndOfStream=True) then
	Exit Sub
End if
temp1s=TextStr.ReadLine
temp2s=textstr.readline
temp3s=textstr.readline
temp4s=Textstr.ReadLine
temp5s=textstr.readline
temp6s=Textstr.ReadLine
TextStr.Close

credit = CDbl(temp1s)
playerscore(1) = CDbl(temp2s)
playerscore(2) = CDbl(temp3s)
playerscore(3) = CDbl(temp4s)
playerscore(4) = CDbl(temp5s)
highscore = cdbl(temp6s)
Set ScoreFile=Nothing
Set FileObj=Nothing
end sub

sub savehs

Dim FileObj
Dim ScoreFile
Set FileObj=CreateObject("Scripting.FileSystemObject")
If Not FileObj.FolderExists(UserDirectory) then 
	Exit Sub
End if
Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "thunderbirds.txt",True)
scorefile.writeline credit
ScoreFile.WriteLine playerscore(1)
ScoreFile.WriteLine playerscore(2)
ScoreFile.WriteLine playerscore(3)
ScoreFile.WriteLine playerscore(4)
scorefile.writeline highscore
ScoreFile.Close
Set ScoreFile=Nothing
Set FileObj=Nothing
end sub


Sub load_lstate   'load currentplayers light state

l1.state = Lstates(currentplayer,1)
l2.state = Lstates(currentplayer,2)
l3.state = Lstates(currentplayer,3)
l4.state = Lstates(currentplayer,4)
l5.state = Lstates(currentplayer,5)
l6.state = Lstates(currentplayer,6)
l7.state = Lstates(currentplayer,7)
l8.state = Lstates(currentplayer,8)
l9.state = Lstates(currentplayer,9)
l10.state = Lstates(currentplayer,10)
l11.state = Lstates(currentplayer,11)
l12.state = Lstates(currentplayer,12)
l13.state = Lstates(currentplayer,13)
l14.state = Lstates(currentplayer,14)
l15.state = Lstates(currentplayer,15)
l16.state = Lstates(currentplayer,16)
l17.state = Lstates(currentplayer,17)
l18.state = Lstates(currentplayer,18)
l19.state = Lstates(currentplayer,19)
l20.state = Lstates(currentplayer,20)
l21.state = Lstates(currentplayer,21)
l22.state = Lstates(currentplayer,22)
l23.state = Lstates(currentplayer,23)
l24.state = Lstates(currentplayer,24)
l25.state = Lstates(currentplayer,25)
l26.state = Lstates(currentplayer,26)
l27.state = Lstates(currentplayer,27)
l28.state = Lstates(currentplayer,28)
l29.state = Lstates(currentplayer,29)
l30.state = Lstates(currentplayer,30)
l31.state = Lstates(currentplayer,31)
l32.state = Lstates(currentplayer,32)
l33.state = Lstates(currentplayer,33)
l34.state = Lstates(currentplayer,34)
l35.state = Lstates(currentplayer,35)
l36.state = Lstates(currentplayer,36)
l37.state = Lstates(currentplayer,37)
l38.state = Lstates(currentplayer,38)
l39.state = Lstates(currentplayer,39)
l40.state = Lstates(currentplayer,40)
l41.state = Lstates(currentplayer,41)

end sub

Sub store_lstate   'store currentplayers light state

Lstates(currentplayer,1) = l1.state
Lstates(currentplayer,2) = l2.state
Lstates(currentplayer,3) = l3.state
Lstates(currentplayer,4) = l4.state
Lstates(currentplayer,5) = l5.state
Lstates(currentplayer,6) = l6.state
Lstates(currentplayer,7) = l7.state
Lstates(currentplayer,8) = l8.state
Lstates(currentplayer,9) = l9.state
Lstates(currentplayer,10) = l10.state
Lstates(currentplayer,11) = l11.state
Lstates(currentplayer,12) = l12.state
Lstates(currentplayer,13) = l13.state
Lstates(currentplayer,14) = l14.state
Lstates(currentplayer,15) = l15.state
Lstates(currentplayer,16) = l16.state
Lstates(currentplayer,17) = l17.state
Lstates(currentplayer,18) = l18.state
Lstates(currentplayer,19) = l19.state
Lstates(currentplayer,20) = l20.state
Lstates(currentplayer,21) = l21.state
Lstates(currentplayer,22) = l22.state
Lstates(currentplayer,23) = l23.state
Lstates(currentplayer,24) = l24.state
Lstates(currentplayer,25) = l25.state
Lstates(currentplayer,26) = l26.state
Lstates(currentplayer,27) = l27.state
Lstates(currentplayer,28) = l28.state
Lstates(currentplayer,29) = l29.state
Lstates(currentplayer,30) = l30.state
Lstates(currentplayer,31) = l31.state
Lstates(currentplayer,32) = l32.state
Lstates(currentplayer,33) = l33.state
Lstates(currentplayer,34) = l34.state
Lstates(currentplayer,35) = l35.state
Lstates(currentplayer,36) = l36.state
Lstates(currentplayer,37) = l37.state
Lstates(currentplayer,38) = l38.state
Lstates(currentplayer,39) = l39.state
Lstates(currentplayer,40) = l40.state
Lstates(currentplayer,41) = l41.state

end sub

'*************
'   LUT
'*************

Sub UpdateLUT_timer()

if fade then LUTImage = LUTImage + 1
if not fade then LUTImage = LUTImage - 1

Select Case LutImage	
				
Case 0: Table1.ColorGradeImage = "LUT0":fade = true: updatelut.interval = 50: updatelut.enabled = 0
Case 1: Table1.ColorGradeImage = "LUT1"
Case 2: Table1.ColorGradeImage = "LUT2"
Case 3: Table1.ColorGradeImage = "LUT3"
Case 4: Table1.ColorGradeImage = "LUT4"
Case 5: Table1.ColorGradeImage = "LUT5"
Case 6: Table1.ColorGradeImage = "LUT6"
Case 7: Table1.ColorGradeImage = "LUT7"
Case 8: Table1.ColorGradeImage = "LUT8"
Case 9: Table1.ColorGradeImage = "LUT9"
Case 10: Table1.ColorGradeImage = "LUT10"
Case 11: Table1.ColorGradeImage = "LUT11"
Case 12: Table1.ColorGradeImage = "LUT12"
Case 13: Table1.ColorGradeImage = "LUT13"
Case 14: Table1.ColorGradeImage = "LUT14"
Case 15: Table1.ColorGradeImage = "LUT15"
Case 16: Table1.ColorGradeImage = "LUT16"
Case 17: Table1.ColorGradeImage = "LUT17"
Case 18: Table1.ColorGradeImage = "LUT18"
Case 19: Table1.ColorGradeImage = "LUT19"
Case 20: Table1.ColorGradeImage = "LUT20"
Case 21: Table1.ColorGradeImage = "LUT21"
Case 22: Table1.ColorGradeImage = "LUT21":fade = false: updatelut.enabled = 0

End Select
End Sub

sub thunderbirdsarego()

	l18.state = 1
	playbackground 0
	playsoundat "TB are go", opto
	vpmtimer.addtimer 16000, "playsoundat ""Tb4drama"", opto '"
	if multiballon=1 then :exit sub :end if
	mbip = 0
	multiballon=1
	multiballdelay.enabled=1
	l13.state = 2
	l14.state = 2
	LightSeqmultiballlights.Play SeqBlinking,,10000,100
end sub

Sub Multiballdelay_timer

select case mbip

	Case 0
			DMDBigText "5",77,1
			ballrelease.CreateBall
			ballrelease.Kick 90.7, 34
			ballsaver.interval=60000
			saveball
			mbip=mbip+1
	Case 1
			DMDBigText "4",77,1
			ballrelease.CreateBall
			ballrelease.Kick 90.7, 34
			mbip=mbip+1
	Case 2
			DMDBigText "3",77,1
			ballrelease.CreateBall
			ballrelease.Kick 90.7, 34
			mbip=mbip+1
	case 3
			DMDBigText "2",77,1
			ballrelease.CreateBall
			ballrelease.Kick 90.7, 34
			mbip=mbip+1
	case 4
			DMDBigText "1",77,1
			ballrelease.CreateBall
			ballrelease.Kick 90.7, 34
			multiballdelay.enabled=0
			mbip=4
			vpmtimer.addtimer 1300, "DMDBigText ""THUNDERBIRDS"",77,0 '"
			vpmtimer.addtimer 2600, "DMDBigText ""ARE GO"",77,0 '"	
			vpmtimer.addtimer 5200, "DMDBigText ""HIT RAMP"",77,0 '"
			vpmtimer.addtimer 5200, "DMDBigText ""SPELL"",77,0 '"
			vpmtimer.addtimer 7200, "DMDBigText ""INTERNATIONAL"",77,1 '"
			vpmtimer.addtimer 9200, "DMDBigText ""RESCUE"",77,1 '"

	end Select

end sub


Sub shootingsingle(location)
	playsoundat "shooting a" & Int(4*rnd+1), location
end Sub


Sub shootingdouble(location)
	playsoundat "shooting b" & Int(2*rnd+1), location
end Sub


sub shootingecho(location)
	playsoundat "shooting c" & Int(3*rnd+1), location
end sub

Dim IRcount: IRcount = 1

Sub IRdisplay()

IRcount = IRcount + 1

if IRcount =>20 then 
	if IRcount =20 then  stopsound "Tb4drama": playsoundat "success", ramptrigger2: backblade.image = "clear"
	if IRcount >20 then  IRcount = 21
else
	backblade.image = "backblade"&IRcount
end if

select case IRcount
case 2: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext1", "IRtext1.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 3: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext2", "IRtext2.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 4: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext3", "IRtext3.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 5: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext4", "IRtext4.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 6: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext5", "IRtext5.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 7: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext6", "IRtext6.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 8: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext7", "IRtext7.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 9: createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext8", "IRtext8.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 10:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext9", "IRtext9.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 11:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext10", "IRtext10.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 12:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext11", "IRtext11.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 13:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext12", "IRtext12.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 14:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext13", "IRtext13.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 15:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext14", "IRtext14.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 16:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext15", "IRtext15.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 17:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext16", "IRtext16.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 18:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext17", "IRtext17.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 19:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext18", "IRtext18.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"
case 20:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext19", "IRtext19.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""SUPER JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 10000000 '": vpmtimer.addtimer 3900, "DMDBigText ""10000000"",77,0 '"
case 21:createsplash "Internationalrescue", "IRVID", "IRlogo.gif", "IRtext19", "IRtext19.png", 100, 140, 3000 :vpmtimer.addtimer 3000, "DMDBigText ""JACKPOT"",77,0 '":  vpmtimer.addtimer 3000, "Addscore 100000 '": vpmtimer.addtimer 3900, "DMDBigText ""100000"",77,0 '"	
end select

end sub

Dim playing: playing = 1

sub playbackground (playnext)

if playbackgroundtimer.enabled and playnext <> 0 then :exit sub :end if

select case playnext

case 0:

stopsound "hood background"
stopsound "island 1"
stopsound "undersea"
stopsound "Thunderbirds"
stopsound "cham cham"
stopsound "island 3"
stopsound "flying high"
stopsound "shooting star"
stopsound "March of the Oysters"
stopsound "island 2"
playbackgroundtimer.enabled = 0: playbackgroundtimer.interval = 10

case 1: playsoundat "hood background", plunger: playbackgroundtimer.interval = 52000: playbackgroundtimer.enabled = 1
case 2: playsoundat "island 1", plunger: playbackgroundtimer.interval = 26000: playbackgroundtimer.enabled = 1
case 3: playsoundat "undersea", plunger: playbackgroundtimer.interval = 123000: playbackgroundtimer.enabled = 1
case 4: playsoundat "Thunderbirds", plunger: playbackgroundtimer.interval = 153000: playbackgroundtimer.enabled = 1
case 5: playsoundat "cham cham", plunger: playbackgroundtimer.interval = 84000: playbackgroundtimer.enabled = 1
case 6: playsoundat "island 3", plunger: playbackgroundtimer.interval = 40000: playbackgroundtimer.enabled = 1
case 7: playsoundat "flying high", plunger: playbackgroundtimer.interval = 13500: playbackgroundtimer.enabled = 1
case 8: playsoundat "shooting star", plunger: playbackgroundtimer.interval = 157000: playbackgroundtimer.enabled = 1
case 9: playsoundat "March of the Oysters", plunger: playbackgroundtimer.interval = 110000: playbackgroundtimer.enabled = 1
case 10: playsoundat "island 2", plunger: playbackgroundtimer.interval = 22000: playbackgroundtimer.enabled = 1

end select

if playnext <> 0 then playing = playing + 1 end if
if playing = 11 then playing = 1 end if

end sub

sub playbackgroundtimer_timer()
	playbackgroundtimer.enabled = 0
	playbackground playing
end sub

Sub bigpoints()

dim random : random = int(4*rnd)

playsoundat "bell", drain

select case Random
case 0:Addscore 10000: vpmtimer.addtimer 3000, "DMDBigText ""BIG POINTS"",57,1 '" :vpmtimer.addtimer 4000, "DMDBigText ""10000"",57,0 '"
case 1:Addscore 25000: vpmtimer.addtimer 3000, "DMDBigText ""BIG POINTS"",57,1 '" :vpmtimer.addtimer 4000, "DMDBigText ""25000"",57,0 '"
case 2:Addscore 50000: vpmtimer.addtimer 3000, "DMDBigText ""BIG POINTS"",57,1 '" :vpmtimer.addtimer 4000, "DMDBigText ""50000"",57,0 '"
case 3:Addscore 75000: vpmtimer.addtimer 3000, "DMDBigText ""BIG POINTS"",57,1 '" :vpmtimer.addtimer 4000, "DMDBigText ""75000"",57,0 '"

end select

end Sub

Sub speeding()

Dim Random, text, text1

if multiballon = 1 then exit sub end if

random = int(4*rnd  + 1) 

select case Random

case 1: text = "FAB1-1.gif": text1 = "speeding1"
case 2: text = "TB1flight1.gif": text1 = "speeding2"
case 3: text = "TB1flight2.gif": text1 = "speeding3"
case 4: text = "TB3 space.gif": text1 = "speeding4"

end select

createsplash "Speedingvideo", text1, text, "clear", "clear.png", 77, 77, 1300

end sub

'***************   TB missions   ************************

' Variables general

Dim screen
Dim outro
Dim success
Dim startcount
Dim Obj

'****** TB1 variables 	****
' game completes when reaching level 7 or time runs out

difficulty = TB1difficulty 					' 3 is hard
extendtime=200 + (3 - difficulty)*50 '(200 = 1 sec - every balloon extends this time)
starttime = 12000 + (3 - difficulty)*1200 '(ms into seconds - linked to move terrain timer 1 min = 1 x 60 x (1000 / 5ms) = 12000 )

dim extendtime: 
dim timeremaining, starttime
dim difficulty
Dim moveLR, moveUD	'move left/right and move up/down
Dim level
Dim count
Dim count1
Dim direction:	'true for right to left, false for left to right
Dim side:	'true for left, false for right
Dim expand
dim balloonnumber,posx(20), posz(20): 
Dim R: 
dim balloonspopped
dim Extraball
dim extraballoffered:extraballoffered = false

'*** TB3 mission Game variables ***

Dim difficultyT
Dim duration,durationmax
Dim incmax
Dim incrementx,incrementy,incrementz
Dim thruston: 
Dim autopiloton
Dim maxend
Dim TotalT
Dim viewpoint	'viewpoint = 1 is 1st person, viewpoint = 0 is 3rd person
Dim tb3x0, tb3y0, tb3z0
Dim tb3x1, tb3y1, tb3z1
Dim tb5x0, tb5y0, tb5z0
Dim tb5x1, tb5y1, tb5z1
Dim tb5x2, tb5y2, tb5z2
Dim tb3rotx0, tb3roty0, tb3rotz0
tb3x0=tb3.x:tb3y0=tb3.y:tb3z0=tb3.z
tb5x0=tb5.x:tb5y0=tb5.y:tb5z0=tb5.z
Dim TBdiffx, TBdiffy,TBdiffz					'3rd person view docking difference in positions
dim autox, autoy, autoz
dim centrex, centrey,centrez					'3rd person view position for TB3 to neatly dock TB5
Dim autoradius
Dim separation1,separation2, totalTtemp: 
Dim move1:move1=0.02
Dim move2:move2=0.06   'For VR Meteors
Dim move3:move3=0.08
Dim MoveShip: MoveShip = 0.2
Dim MoveShip2: MoveShip2 = 0.25
Dim move4:move4=22
Dim move5:move5=-24

'TB4 mission variables 

Dim difficulty4
Dim left, right, down, up
Dim row
Dim TB4position
Dim collided: 
Dim Mine(70)
Dim minesmissed:
Dim mineleftright
Dim mineupdown
dim corridory0, corridor1y0, corridor2y0, corridor3y0, corridor4y0, corridor5y0
dim corridorinterval: 
Dim deltax,deltaz
Dim leftright: 
Dim updown: 


' **** TB3 Mission ****

Sub TB3missionsetup()
	tb3gameconfig()
	maxend=50000:autoradius = 1000: incmax = 30 :difficultyT = 2.5 'multiplier to available time
	durationmax = 2500			'less is more difficult
	thruston=false
	TBdiffx=2780: TBdiffy=514: TBdiffz=-3800
	totalTtemp = 0
	outro = false
	screen = 0
	success=false
	move.enabled = 1
	updatelut.enabled = 1
	intro.enabled = 1
	SetBackglassTB3mission()
	autopiloton=false
	duration=0:TotalT = 0
	incrementx=0:incrementy=0:incrementz=0
	viewpoint=0 
	arrowx.size_x = 0:arrowy.size_y = 0:arrowz.size_z = 0
	vgameon = false
	setTB3pos()
	setTB5pos()
	centrex = tb5x0 - TBdiffx
	centrey = tb5y0 - TBdiffy
	centrez = tb5z0 - TBdiffz
	playsound "space", -1, 0.1, 0, 0, 0
	updategauge
	Rock1Timer.enabled =1
	SkyTimer.enabled =1
	fuelgauge.imagea ="tb3Fuel"
	Speedgauge.imagea ="tb3Speed"
	timegauge.imagea ="tb3Time"

end sub

Sub TB3missionend()

	success=false
	viewpoint=0 : recentretb5()
	autopiloton=false
	duration=0:TotalT = 0
	incrementx=0:incrementy=0:incrementz=0
	arrowx.size_x = 0:arrowy.size_y = 0:arrowz.size_z = 0
	centrex = tb5x0 - TBdiffx
	centrey = tb5y0 - TBdiffy
	centrez = tb5z0 - TBdiffz
	updategauge
	screen = 0
	outro = false
	updatelut.enabled = 1
	counter.visible=0
	briefing.imagea = "clear"
	for each Obj in display: obj.visible = 0 : next
	for each Obj in TB3missionthings: obj.visible = 0 : next
	for each Obj in fueltb3 : obj.visible = 0 : next
	for each Obj in countdowntb3 : obj.visible = 0 : next
	for each Obj in speedotb3 : obj.visible = 0 : next
	Speedgauge.visible = 0
	intro.enabled = 0
	move.enabled = 0
	Rock1Timer.enabled =0
	SkyTimer.enabled =0
	stopsound "space"
	tb3pinballconfig()
End sub

'********************  Flasher position  *******************

Sub SetBackglassTB3mission()

For Each obj In display
	obj.x = obj.x - 10
	'obj.height = 1000
	obj.y = -1000   'adjusts the distance from the backglass towards the user
Next

For Each obj In fueltb3
	obj.height = - abs(obj.height)
	obj.x = obj.x - 10
	'obj.height = - obj.y + 1500
	obj.y = 100   'adjusts the distance from the backglass towards the user
Next

For Each obj In countdowntb3
	obj.x = obj.x - 10
	obj.height = - abs(obj.height)
	'obj.height = - obj.y + 1500
	obj.y = 100   'adjusts the distance from the backglass towards the user
Next

counter.visible=0
for each Obj in display: obj.visible = 1 : next
for each Obj in TB3missionthings: obj.visible = 1 : next
for each Obj in fueltb3 : obj.visible = 1 : next
for each Obj in countdowntb3 : obj.visible = 1 : next

End Sub

Sub intro_timer()

Dim tb1bonus

if TB3missionon=1 then

briefing.visible=1
screen = screen + 1

if not outro then

	Select case screen

	case 1:briefing.imagea = "tb3mission01"
	case 2:briefing.imagea = "tb3mission02"
	case 3:briefing.imagea = "tb3left"
	case 4:briefing.imagea = "tb3right"
	case 5:briefing.imagea = "tb3up"
	case 6:briefing.imagea = "tb3down"
	case 7:briefing.imagea = "tb3forwards"
	case 8:briefing.imagea = "tb3backwards"
	case 9:briefing.imagea = "tb3fire"
	case 10:briefing.imagea = "tb3start":screen = 0

	end select

end if

if  outro Then

	Select case screen
	case 1
		viewpoint=0: recentretb5
		if success then
			briefing.imagea = "tb3success"
			if gamemode <>4 then vpmtimer.addtimer 17000, "videomissionon = 0: tb3missionon = 0: transitionmission 3 '" end if
		end if
		if not success then 
			briefing.imagea = "tb3failed"
			if gamemode <>4 then vpmtimer.addtimer 6000, "videomissionon = 0: tb3missionon = 0: transitionmission 3 '" end if
		end if

	case 3
		if gamemode =4 then briefing.imagea = "tb3reset"
		if success and gamemode <>4 then 
			briefing.imagea = "million"
		end if
	case 4
		if gamemode =4 then briefing.imagea = "changegame"
		if success and gamemode <>4 then 
			addscore 1000000
'			vpmtimer.addtimer 17000, "DMDBigText ""MISSION BONUS"",117,0 '"
'			vpmtimer.addtimer 19000, "DMDBigText 1000000,117,0 '"
		end if
	case 5
		screen = 0
		briefing.imagea = "clear"
		briefing.visible=0
		intro.enabled=0
	end Select
end if

end if

if tb1missionon=1 then

	briefing.visible=1
	screen = screen + 1

if not outro then

	Select case screen

	case 1:briefing.imagea = "tb1mission01"
	case 2:briefing.imagea = "tb1mission02"
	case 3:briefing.imagea = "tb1mission03"
	case 4:briefing.imagea = "tb1left"
	case 5:briefing.imagea = "tb1right"
	case 6:briefing.imagea = "tb1up"
	case 7:briefing.imagea = "tb1down"
	case 8:briefing.imagea = "tb1start":screen = 0

	end select

end if

if  outro Then

	Select case screen
	case 1
		if success then briefing.imagea = "tb1complete"
'		if not success then briefing.imagea = "tb1failed"
	case 3
		if gamemode =4 then briefing.imagea = "tb1reset"
		if success and gamemode <>4 then 
			briefing.imagea = "balloonbonus"
			tb1bonus = balloonspopped*10000
			addscore tb1bonus
'			vpmtimer.addtimer 28000, "DMDBigText ""MISSION BONUS"",117,0 '"
'			vpmtimer.addtimer 30000, "DMDBigText tb1bonus,117,0 '"
		end if
	case 4
		if gamemode =4 then briefing.imagea = "changegame"
		if extraball and gamemode <>4 then 
			shootagain = shootagain + 1
			briefing.imagea = "extraballadded"
		end if
	case 5
		screen = 0
		briefing.imagea = "clear"
		briefing.visible=0
		intro.enabled=0
		if gamemode <>4 then vpmtimer.addtimer 22000, "videomissionon = 0: tb1missionon = 0: transitionmission 1 '" end if
	end Select
end if

end if

if TB4missionon=1 then
briefing.visible=1
screen = screen + 1

if not outro then

	Select case screen

	case 1:briefing.imagea = "tb4mission01"
	case 2:briefing.imagea = "tb4left"
	case 3:briefing.imagea = "tb4right"
	case 4:briefing.imagea = "tb4up"
	case 5:briefing.imagea = "tb4down"
	case 6:briefing.imagea = "tb4mines"
	case 7:briefing.imagea = "tb4start":screen = 0

	end select

end if

if  outro Then

	Select case screen
	case 1
		if success then 
			briefing.imagea = "tb4success"
			if gamemode <>4 then vpmtimer.addtimer 18000, "videomissionon = 0: tb4missionon = 0: transitionmission 4 '" end if
		end if
		if not success then 
			briefing.imagea = "tb4failed"
			if gamemode <>4 then vpmtimer.addtimer 6000, "videomissionon = 0: tb4missionon = 0: transitionmission 4 '" end if
		end if
	case 3
		if gamemode =4 then briefing.imagea = "tb4reset"
		if success and gamemode <>4 then 
			briefing.imagea = "million"
			addscore 1000000
'			vpmtimer.addtimer 18000, "DMDBigText ""MISSION BONUS"",117,0 '"
'			vpmtimer.addtimer 20000, "DMDBigText 1000000,117,0 '"
		end if
	case 4
		if gamemode =4 then briefing.imagea = "changegame"
	case 5
		screen = 0
		briefing.imagea = "clear"
		briefing.visible=0
		intro.enabled=0
	end Select
end if
end if

end sub

Sub resettb3mission
	success=false
	viewpoint=0 : recentretb5()
	autopiloton=false
	duration=0:TotalT = 0
	incrementx=0:incrementy=0:incrementz=0
	arrowx.size_x = 0:arrowy.size_y = 0:arrowz.size_z = 0
	setTB3pos()
	setTB5pos()
	centrex = tb5x0 - TBdiffx
	centrey = tb5y0 - TBdiffy
	centrez = tb5z0 - TBdiffz
	playsound "space", -1, 0.1, 0, 0, 0
	for each Obj in speedotb3 : obj.visible = 1 : next
	Speedgauge.visible = 1
	updategauge
end sub

Sub playthrust

if TB3missionon=1 then

if vgameon then

	If duration<durationmax then 

		if thruston=false and (LFF.enabled=true or RFF.enabled=true or LMS.enabled=true or RMS.enabled=true) then 
			playsound "thrust", -1, 100, 0, 0, 0
			thruston = True
		end if

	Else

		stopsound "thrust"
		thruston = false

	end if

	if thruston=true  and (LFF.enabled=false and RFF.enabled=false and LMS.enabled=false and RMS.enabled=false) then 
		stopsound "thrust"
		thruston = false
	end if

end if

end If

end sub


Sub setTB3pos()

Dim tempx, tempy, tempz

tempy = rnd(1)
tempz = rnd(1)

if tempy=>0.5 then tempy=1 else tempy =-1
if tempz=>0.5 then tempz=1 else tempz =-1

do 
tb3x0 = maxend*rnd(1)
	
loop until (tb3x0 > 30000) and (tb3x0 < maxend)

do 
tb3y0 = tempy*maxend*rnd(1)
	
loop until (tb3y0 < 6000) and (tb3y0 > -maxend)

do 
tb3z0 = tempz*maxend*rnd(1)
	
loop until (tb3z0 < maxend) and (tb3z0 > -maxend)

tb3.x=-tb3x0:tb3.y=tb3y0:tb3.z=tb3z0

End Sub

Sub setTB5pos()

Dim tempy, tempz

tempy = rnd(1)
tempz = rnd(1)

if tempy=>0.5 then tempy=1 else tempy =-1
if tempz=>0.5 then tempz=1 else tempz =-1

do 
tb5x0 = maxend*rnd(1)

loop until (tb5x0 > 30000) and (tb5x0 < maxend)
	
do 
tb5y0 = tempy*maxend*rnd(1)
	
loop until (tb5y0 < 6000) and (tb5y0 > -maxend)

do 
tb5z0 = tempz*maxend*rnd(1)
	
loop until (tb5z0 < maxend) and (tb5z0 > -maxend)

tb5.x=tb5x0:tb5.y=tb5y0:tb5.z=tb5z0

End sub

Sub calculateT

Dim Dt, Dx, Dy, Dz
Dim tb3x01,tb3y01,tb3z01
Dim tb5x01,tb5y01,tb5z01


tb3x01=tb3.x:tb3y01=tb3.y:tb3z01=tb3.z
tb5x01=tb5.x:tb5y01=tb5.y:tb5z01=tb5.z

Dx = abs(TB5x01 - TB3x01)
Dy = abs(TB5y01 - TB3y01)
Dz = abs(TB5z01 - TB3z01)

Dt = Dx + Dy + Dz

TotalT = difficultyT*(Dt/incmax)			'100ths of a secong

end sub

sub start1_timer

if TB3missionon=1 then

	startcount = startcount - 1

select case startcount

Case 13
	counter.imagea = "digit5"
Case 11
	counter.imagea = "digit4"
Case 9
	counter.imagea = "digit3"
case 7
	counter.imagea = "digit2"
case 5
	counter.imagea = "digit1"
case 3
	counter.imagea = "clear"
case -1
	calculateT
	counter.visible = 0
	playsound "space", -1, 0.1, 0, 0, 0
	start1.enabled= 0
	vgameon = true
end select

end if

if tb1missionon=1 then

	startcount = startcount - 1

select case startcount

Case 13
	counter.imagea = "digit5"
Case 11
	counter.imagea = "digit4"
Case 9
	counter.imagea = "digit3"
case 7
	counter.imagea = "digit2"
case 5
	counter.imagea = "digit1"
case 3
	counter.imagea = "clear"
case -1
	counter.visible = 0
	playsound "jet", -1, 0.25, 0, 0, 0
	start1.enabled= 0
	vgameon = true
	moveterrain.enabled = 1
end select
end if

if TB4missionon=1 then
	fish2timer.enabled = 1
	startcount = startcount - 1

select case startcount

Case 13
	counter.imagea = "digit5"
Case 11
	counter.imagea = "digit4"
Case 9
	counter.imagea = "digit3"
case 7
	counter.imagea = "digit2"
case 5
	counter.imagea = "digit1"
case 3
	counter.imagea = "clear"
case -1
	counter.visible = 0
	playsound "tb4drama", 0, 100, 0, 0, 0
	start1.enabled= 0
	vgameon = true
	movecorridor.enabled = 1
	speedup.enabled = 1
	fish3timer.enabled = 1
end select
end if

end sub

sub LFF_timer

Dim objroty, transz, transy

if TB3missionon=1 then
	playthrust
	if (vgameon = true and autopiloton=false and duration<durationmax and TotalT>0) then
		if RFF.enabled=true then :incrementz=incrementz+0.1*rnd(1) : duration=duration+0.5 :else:  incrementy=incrementy+0.1*rnd(1) : duration=duration+1 :end if

		if incrementy=>incmax then incrementy=incmax
		if incrementz=>2*incmax then incrementz=2*incmax
	end if
	if duration>durationmax or TotalT<=0 then lfF.enabled = 0
end if

if tb1missionon=1then
	if not RFF.enabled then moveLR = (40 + (level * difficulty)) else moveLR = 0
end if

if TB4missionon=1 then

objroty=TB4yoke.objroty: transz=TB4yoke.transz: transy=TB4yoke.transy

	if left then 
		TB4left.enabled = 1
		TB4yoke.objroty = objroty -	2					'-10
		TB4yoke.transz = transz + 28					'140
		TB4yoke.transy = transy - 2						'-10
		if int(objroty) = -10 then :TB4yoke.objroty = -10: LFF.enabled = 0: TB4yoke.transz = 140: TB4yoke.transy = -10: exit sub: end if
	end if

	if not left then 
		TB4yoke.objroty = objroty +	2					'0
		TB4yoke.transz = transz - 28					'0
		TB4yoke.transy = transy + 2						'0
		if int(objroty) = 0 then :TB4yoke.objroty = 0: LFF.enabled = 0: TB4yoke.transz =0: TB4yoke.transy = 0: exit sub: end if
	end if
end if

end Sub

sub RFF_timer

Dim objroty, transz, transy

if TB3missionon=1 then
	playthrust
	if (vgameon = true and autopiloton=false and duration<durationmax and TotalT>0) then
		if LFF.enabled=true then :incrementz=incrementz+0.1*rnd(1): duration=duration+0.5 :else: incrementy=incrementy-0.1*rnd(1): duration=duration+1:end if
		if incrementy<-incmax then incrementy=-incmax
		if incrementz=>2*incmax then incrementz=2*incmax
	end if

	if duration>durationmax or TotalT<=0 then rfF.enabled = 0
end if

if tb1missionon=1then
	if not LFF.enabled then moveLR = -(40 + (level * difficulty)) else moveLR = 0
end if

if TB4missionon=1 then

objroty=TB4yoke.objroty: transz=TB4yoke.transz: transy=TB4yoke.transy

	if Right then 
		TB4right.enabled = 1
		TB4yoke.objroty = objroty +	2					'10
		TB4yoke.transz = transz - 28					'-140
		TB4yoke.transy = transy - 2						'-10
		if int(objroty) = 10 then RFF.enabled = 0: :TB4yoke.objroty = 10: TB4yoke.transz = -140: TB4yoke.transy = -10: exit sub: end if
	end if

	if not Right then 
		TB4yoke.objroty = objroty -	2					'0
		TB4yoke.transz = transz + 28					'0
		TB4yoke.transy = transy + 2						'0
		if int(objroty) = 0 then RFF.enabled = 0: TB4yoke.objroty = 0: TB4yoke.transz =0: TB4yoke.transy = 0: exit sub: end if
	end if
end if

end Sub

sub LMS_timer

Dim objrotx, transy

if TB3missionon=1 then
	playthrust
	if (vgameon = true and autopiloton=false and duration<durationmax and TotalT>0) then
		if RMS.enabled=true then :incrementz=incrementz-0.1*rnd(1): duration=duration+0.5 :else: incrementx=incrementx-0.1*rnd(1): duration=duration+1:end if
		if incrementx<-incmax then incrementx=-incmax
		if incrementz<-2*incmax then incrementz=-2*incmax
	end if
	if duration>durationmax or TotalT<=0 then lms.enabled = 0
end if

if tb1missionon=1then
	if not RMS.enabled then moveUD = (40 + (level * difficulty)) else moveUD = 0
end if

if TB4missionon=1 then

objrotx = TB4yoke.objrotx: transy = TB4yoke.transy

	if down then 
		TB4down.enabled = 1
		TB4yoke.objrotx = objrotx + 1					'5
		TB4yoke.transy = transy + 38					'190
		if int(objrotx) = 5 then LMS.enabled = 0: TB4yoke.objrotx = 5: TB4yoke.transy = 190: exit sub: end if
	end if

	if not down then 
		TB4yoke.objrotx = objrotx - 1					'0
		TB4yoke.transy = transy - 38					'0
		if int(objrotx) = 0 then LMS.enabled = 0: TB4yoke.objrotx = 0: TB4yoke.transy = 0: exit sub: end if
	end if
end if

end sub

Sub RMS_timer

Dim objrotx, transy

if TB3missionon=1 then
	playthrust
	if (vgameon = true and autopiloton=false and duration<durationmax and TotalT>0) then
		if LMS.enabled=true then :incrementz=incrementz-0.1*rnd(1): duration=duration+0.5 :else: incrementx=incrementx+0.1*rnd(1): duration=duration+1:end if
		if incrementx=>incmax then incrementx=incmax
		if incrementz<-2*incmax then incrementz=-2*incmax
	end if
	if duration>durationmax or TotalT<=0 then rms.enabled = 0
end if

if tb1missionon=1then
	if not LMS.enabled then moveUD = -(40 + (level * difficulty)) else moveUD = 0
end if

if TB4missionon=1 then

objrotx = TB4yoke.objrotx: transy = TB4yoke.transy

	if up then 
		TB4up.enabled = 1
		TB4yoke.objrotx = objrotx - 1					'-5
		TB4yoke.transy = transy - 38					'-190
		if int(objrotx) = - 5 then RMS.enabled = 0: TB4yoke.objrotx = - 5: TB4yoke.transy = -190: exit sub: end if
	end if

	if not up then 
		TB4yoke.objrotx = objrotx + 1					'0
		TB4yoke.transy = transy + 38					'0
		if int(objrotx) = 0 then RMS.enabled = 0: TB4yoke.objrotx = 0: TB4yoke.transy = 0: exit sub: end if
	end if
end if

end sub


Sub move_timer

Dim deltax, deltay, deltaz, deltar
Dim sizex, sizey, sizez

if TB3missionon=1 then

sizex = 30*(0.5*incrementz/incmax)
sizey = -30*(incrementy/incmax)
sizez = 30*(incrementx/incmax)

		if viewpoint=0 then

			deltax=tb3.x - centrex
			deltay=tb3.y - centrey
			deltaz=tb3.z - centrez
			deltar = deltax^2+deltay^2+deltaz^2
			if deltar<=autoradius^2 then autopilot.enabled=1

			tb3.z=tb3.z+incrementx
			tb3.y=tb3.y-incrementy
			tb3.x=tb3.x+0.5*incrementz	'twice as many increments when both buttons pressed

			arrowx.size_x = sizex
			if 30*(0.5*incrementz)/incmax > 0 then arrowx.image = "green" else arrowx.image = "red" 
			arrowy.size_y = sizey
			if 30*(incrementy)/incmax < 0 then arrowy.image = "green" else arrowy.image = "red" 
			arrowz.size_z = sizez
			if 30*(incrementx)/incmax > 0 then arrowz.image = "green" else arrowz.image = "red" 


		end if

		if viewpoint=1 then 

			deltax=tb5.x - TBdiffy
			deltay=tb5.y + TBdiffx
			deltaz=tb5.z - TBdiffz
			deltar = deltax^2+deltay^2+deltaz^2
			if deltar<=autoradius^2 then autopilot.enabled=1

			tb5.x=tb5.x+incrementy
			tb5.z=tb5.z-incrementx
			tb5.y=tb5.y+0.5*incrementz	'twice as many increments when both buttons pressed

			arrowy.size_y = -30*(0.5*incrementz)/incmax
			if 30*(0.5*incrementz)/incmax > 0 then arrowy.image = "green" else arrowy.image = "red" 
			arrowx.size_x = - 30*(incrementy)/incmax
			if 30*(incrementy)/incmax < 0 then arrowx.image = "green" else arrowx.image = "red" 
			arrowz.size_z = 30*(incrementx)/incmax
			if 30*(incrementx)/incmax > 0 then arrowz.image = "green" else arrowz.image = "red" 


		end if

	TotalT = TotalT - 1
	if TotalT <=0 then TotalT = 0
	updategauge

	If vgameon and TotalT <=0 and autopilot.enabled = false then
		move.enabled=0
		stopsound "space"
		playsound "failed", 0, 100, 0, 0, 0
		for each Obj in speedotb3 : obj.visible = 0 : next
		Speedgauge.visible = 0
		success=false
		outro=true:screen=0
		intro.enabled=1
	end if

	if vgameon and TotalT> 0 and duration=>durationmax then 
		if totalTtemp = 0 then 
			totalTtemp = TotalT
			separation1 = deltar
			separation2 = deltar
		end if

		if (totalTtemp - TotalT) = 1 then  separation2 = deltar	: totalTtemp = 0

		if (separation2 - separation1 > 0) or  ( deltar > sqr(totalT * sqr(incrementx^2 + incrementy^2 + incrementz^2)) ) then   
			move.enabled=0
			stopsound "space"
			playsound "failed", 0, 100, 0, 0, 0
			for each Obj in speedotb3 : obj.visible = 0 : next
			Speedgauge.visible = 0
			success=false
			outro=true:screen=0
			intro.enabled=1
		end if

	end if

end if

end sub

sub autopilot_timer

Dim temp
temp = move.interval

	if autopiloton = false then 
		if viewpoint = 0 then
			autox = centrez-tb3.z
			autoy = centrey-tb3.y
			autoz = centrex-tb3.x
		end if

		if viewpoint = 1 then
			autox = tb5.z - TBdiffz
			autoy = tb5.x - TBdiffy
			autoz = tb5.y + TBdiffx
		end if
		autopiloton = true
	end if

if viewpoint = 0 then
		incrementx = 0.001*temp*autox
		incrementy = -0.001*temp*autoy
		incrementz = 0.001*temp*autoz

	if ((centrex-tb3.x)^2<=1000) then incrementz=0
	if ((centrey-tb3.y)^2<=1000) then incrementy=0
	if ((centrez-tb3.z)^2<=1000) then incrementx=0
end if

if viewpoint = 1 then
		incrementx = 0.001*temp*autox
		incrementy = - 0.001*temp*autoy
		incrementz = - 0.001*temp*autoz

	if ((tb5.x - TBdiffy)^2<=1000) then incrementy=0
	if ((tb5.y + TBdiffx)^2<=1000) then incrementz=0
	if ((tb5.z - TBdiffz)^2<=1000) then incrementx=0

end if

if (incrementx=0 and incrementy=0 and incrementz=0) then

	move.enabled=0: autopilot.enabled = 0
	stopsound "space"
	playsound "success", 0, 100, 0, 0, 0
	for each Obj in speedotb3 : obj.visible = 0 : next
	Speedgauge.visible = 0
	success=true
	outro=true:screen=0
	intro.enabled=1
end if

end sub

Sub recentretb5
dim a,b,c
	if viewpoint = 1 then

		tb3x1=tb3.x:tb3y1=tb3.y:tb3z1=tb3.z
		TB3.x = 500								'offset due to playfield co-ordinates
		TB3.y = 500								'offset due to playfield co-ordinates
		TB3.z = 0
		TB3.roty = 270


		tb5.x = tb5y0 - tb3y1
		tb5.y = -(tb5x0 - tb3x1)
		tb5.z = tb5z0 - tb3z1

		tb5.rotx = 90 				'tb5.rotx - tb3rotx0
		tb5.roty = 0 				'tb5.roty - tb3roty0
		tb5.rotz = 0 				'tb5.rotz - tb3roty0

		tb5x2=tb5.x:tb5y2=tb5.y:tb5z2=tb5.z
	end if

	if viewpoint = 0 then 

		tb5x1=tb5.x:tb5y1=tb5.y:tb5z1=tb5.z

		a= tb5x1 - tb5x2
		b= -(tb5y1 - tb5y2)
		c= tb5z1 - tb5z2

		tb3.x = tb3x1 - b
		tb3.y = tb3y1 - a
		tb3.z = tb3z1 - c
		TB3.roty = 0

		tb5.x = tb5x0+500									'offset due to playfield co-ordinates
		tb5.y = tb5y0-500									'offset due to playfield co-ordinates
		tb5.z = tb5z0

		tb5.rotx = 90 				'tb5.rotx + tb3rotx0
		tb5.roty = 90 				'tb5.roty + tb3roty0
		tb5.rotz = 0 				'tb5.rotz + tb3roty0

	end if
end sub

'moving objects.. ****************************************************************************************************

Sub SkyTimer_Timer()

Stars.ObjRotZ=Stars.ObjRotZ+move1

End Sub

Sub Rock1Timer_Timer()

Rock1.ObjRotZ=Rock1.ObjRotZ+move2
Rock1.ObjRotX=Rock1.ObjRotX+move3

Rock2.ObjRotZ=Rock2.ObjRotZ+move2
Rock2.ObjRotX=Rock2.ObjRotX+move3

Rock4.ObjRotZ=Rock4.ObjRotZ+move3
Rock4.ObjRotX=Rock4.ObjRotX+move2

Rock7.ObjRotZ=Rock7.ObjRotZ+move3
Rock7.ObjRotX=Rock7.ObjRotX+move2

FallingStar1.ObjRotZ=FallingStar1.ObjRotZ+move3
FallingStar1.ObjRotX=FallingStar1.ObjRotX+move3

FallingStar2.ObjRotZ=FallingStar2.ObjRotZ+move3
FallingStar2.ObjRotX=FallingStar2.ObjRotX+move3

'End Sub

'Sub StarTimer_Timer()

FallingStar1.Y=FallingStar1.Y+move4
FallingStar2.Y=FallingStar2.Y+move5

If FallingStar1.Y>=50000 then Randomize (21): FallingStar1.Y = -42000: FallingStar1.X = 15000 + rnd(1)*-25000 : FallingStar1.Size_x = 1300 * rnd(1) +320 : FallingStar1.Size_y = 1300 * rnd(1) +320: FallingStar1.Size_z = 1300 * rnd(1) +550' Randomize X position, x,y,z size here
If FallingStar2.Y<=-40000 then Randomize (5): FallingStar2.Y = 50000: FallingStar2.X = 15000 + rnd(1)*-25000 : FallingStar2.Size_x = 1300 * rnd(1) +320 : FallingStar2.Size_y = 1300 * rnd(1) +320: FallingStar2.Size_z = 1300 * rnd(1) +550' Randomize X position, x,y,z size here

End Sub

' ***************** CODE BELOW IS FOR THE gauges   *******************************
'*****************************************************************************************************************************************

Sub updategauge()

Dim fuel

fuel = durationmax - duration


Dim temp1, temp2, temp3, temp4, temp5, temp6

temp1 = fuel \ 1000
temp2 = (fuel - 1000*temp1) \ 100
temp3 = (fuel - 1000*temp1 - 100*temp2) \ 10
temp4 = (fuel - 1000*temp1 - 100*temp2 - 10*temp3) \ 1

    ' **************************************

	thousands.imagea="digit" & CStr(temp1)
    hundreds.imagea="digit" & CStr(temp2)
    tens.imagea="digit" & CStr(temp3)
    ones.imagea="digit" & CStr(temp4)
   
' time remaining - TotalT

temp1 = totalT \ 100000
temp2 = (totalT - 100000*temp1) \ 10000 
temp3 = (totalT - 100000*temp1 - 10000*temp2) \ 1000
temp4 = (totalT - 100000*temp1 - 10000*temp2 - 1000*temp3) \ 100
temp5 = (totalt - 100000*temp1 - 10000*temp2 - 1000*temp3 - 100*temp4) \ 10
temp6 = (totalt - 100000*temp1 - 10000*temp2 - 1000*temp3 - 100*temp4 - 10*temp5) \ 1

	hour1.imagea="digit" & CStr(temp1)
    hour2.imagea="digit" & CStr(temp2)

	minute1.imagea="digit" & CStr(temp3)
    minute2.imagea="digit" & CStr(temp4)

	second1.imagea="digit" & CStr(temp5)
    second2.imagea="digit" & CStr(temp6)

End Sub

'******** TB1 Mission Script ****************

Sub TB1missionsetup()
	tb1gameconfig()
	thousands.x = -2380.525
	thousands.y = 1193.863
	hit1.x = 500: hit1.y = 300
	hit2.x = 500: hit2.y = 500
	screen = 0
	level = 1
	count = 1
	count1 = 1
	direction = True
	side = True
	expand = true
	balloonnumber = 0
	R = 12000
	balloonspopped = 0
	Extraball = false
	outro = false
	success=false
	SetBackglassTB1mission()
	moveterrain.enabled = 1
	cloudinit()
	ballooninit()
	intro.enabled = 1
	vgameon = false
	updatelut.enabled = 1
	moveterrain.interval = 5
	moveterrain.enabled = 0
	playsound "island", -1, 0.1, 0, 0, 0
	timeremaining = starttime
	updatedash
	sky.visible=1
	TB1body.visible=1
	timegauge.imagea ="timeremaining"
end sub

Sub TB1missionend()

	thousands.x = -1462.525
	thousands.y = 2129.863
	TB1legs.y = 500
	TB1legs.z = -500
	TB1legs.roty = 90
	success=false
	count = 1: count1 = 1
	level = 1
	extraballoffered = false
	vgameon = false
	moveterrain.interval = 5
	moveterrain.enabled = 0
	ones.imagea = "clear": tens.imagea = "clear": hundreds.imagea = "clear": balloongauge.imagea = "ballonspopped"
	balloonspopped = 0
	timeremaining = starttime
	updatelut.enabled = 1
	TB1legs.visible = 0
	briefing.imagea = "clear"
	for each Obj in display : obj.visible = 0 : next
	for each obj in gauges : obj.visible = 0 : next
	for each Obj in clouds : obj.visible = 0 : next
	for each Obj in balloons : obj.visible = 0 : next
	for each Obj in terrainall : obj.visible = 0 : next
	intro.enabled = 0
	stopsound "island"
	sky.visible=0
	tb1pinballconfig()
	TB1body.visible=0
End sub

'********************  Flasher position  *******************

Sub SetBackglassTB1mission()

For Each obj In display
	obj.x = obj.x - 10
	'obj.height = 1000
	obj.y = 100   'adjusts the distance from the backglass towards the user
Next

for each Obj in display : obj.visible = 1 : next

for each obj in gauges : obj.x = obj.x - 10: obj.y = -0: next
'for each obj in gauges : obj.x = obj.x - 10: obj.height = -100: obj.y = -0: next
for each obj in gauges : obj.visible = 1 : next

for each Obj in clouds : obj.visible = 1 : next
for each Obj in balloons : obj.visible = 1 : next
for each Obj in terrainall : obj.visible = 1 : next

End Sub

Sub cloudinit()

dim c
dim posx, posy, plusminus
dim spacing, base
dim scaling, size
dim image

posy = 160000
posx = 150000
base = 40000
size = 1000

for c = 1 to 20
	plusminus = 2*rnd
	if plusminus <1 then plusminus = 1:image = "cloud-light": else plusminus = -1 :image = "cloud-dark": end if
	spacing = base + plusminus*5000*rnd
	scaling = size*(1 + 2*rnd)

	select case c

	case 1: cloud1.x = -posx: cloud1.y = posy - spacing: cloud1.size_x = scaling:cloud1.size_y = scaling:cloud1.image = image
	case 2: cloud2.x = -posx: cloud2.y = cloud1.y - spacing: cloud2.size_x = scaling:cloud2.size_y = scaling:cloud2.image = image
	case 3: cloud3.x = -posx: cloud3.y = cloud2.y - spacing: cloud3.size_x = scaling:cloud3.size_y = scaling:cloud3.image = image
	case 4: cloud4.x = -posx: cloud4.y = cloud3.y - spacing: cloud4.size_x = scaling:cloud4.size_y = scaling:cloud4.image = image
	case 5: cloud5.x = -posx: cloud5.y = cloud4.y - spacing: cloud5.size_x = scaling:cloud5.size_y = scaling:cloud5.image = image
	case 6: cloud6.x = -posx: cloud6.y = cloud5.y - spacing: cloud6.size_x = scaling:cloud6.size_y = scaling:cloud6.image = image
	case 7: cloud7.x = -posx: cloud7.y = cloud6.y - spacing: cloud7.size_x = scaling:cloud7.size_y = scaling:cloud7.image = image
	case 8: cloud8.x = -posx: cloud8.y = cloud7.y - spacing: cloud8.size_x = scaling:cloud8.size_y = scaling:cloud8.image = image
	case 9: cloud9.x = -posx: cloud9.y = cloud8.y - spacing: cloud9.size_x = scaling:cloud9.size_y = scaling:cloud9.image = image
	case 10:cloud10.x = -posx: cloud10.y = cloud9.y - spacing: cloud10.size_x = scaling:cloud10.size_y = scaling:cloud10.image = image
	case 11:cloud11.x = posx: cloud11.y = posy - spacing: cloud11.size_x = scaling:cloud11.size_y = scaling:cloud11.image = image
	case 12:cloud12.x = posx: cloud12.y = cloud11.y - spacing: cloud12.size_x = scaling:cloud12.size_y = scaling:cloud12.image = image
	case 13:cloud13.x = posx: cloud13.y = cloud12.y - spacing: cloud13.size_x = scaling:cloud13.size_y = scaling:cloud13.image = image
	case 14:cloud14.x = posx: cloud14.y = cloud13.y - spacing: cloud14.size_x = scaling:cloud14.size_y = scaling:cloud14.image = image
	case 15:cloud15.x = posx: cloud15.y = cloud14.y - spacing: cloud15.size_x = scaling:cloud15.size_y = scaling:cloud15.image = image
	case 16:cloud16.x = posx: cloud16.y = cloud15.y - spacing: cloud16.size_x = scaling:cloud16.size_y = scaling:cloud16.image = image
	case 17:cloud17.x = posx: cloud17.y = cloud16.y - spacing: cloud17.size_x = scaling:cloud17.size_y = scaling:cloud17.image = image
	case 18:cloud18.x = posx: cloud18.y = cloud17.y - spacing: cloud18.size_x = scaling:cloud18.size_y = scaling:cloud18.image = image
	case 19:cloud19.x = posx: cloud19.y = cloud18.y - spacing: cloud19.size_x = scaling:cloud19.size_y = scaling:cloud19.image = image
	case 20:cloud20.x = posx: cloud20.y = cloud19.y - spacing: cloud20.size_x = scaling:cloud20.size_y = scaling:cloud20.image = image

	end select
next

end sub

sub ballooninit

Dim signx, signz, tempx, tempz, deltax, deltaz, temp1x, temp1z

balloonnumber = balloonnumber + 1

if balloonnumber = 1 then
	tempx=rnd
	tempz=rnd
	if tempx <0.5 then signx = -1 else signx = 1 end if
	if tempz <0.5 then signz = -1 else signz = 1 end if
	posx(1) = signx * 60000 * rnd
	posz(1) = signz * 20000 * rnd
	ballooninit
end if

if balloonnumber>1 and balloonnumber<20 then
	tempx=rnd
	tempz=rnd
	if tempx <0.5 then signx = -1 else signx = 1 end if
	if tempz <0.5 then signz = -1 else signz = 1 end if
	deltax = signx * R * rnd
	deltaz = signz * R * rnd
	temp1x = posx(balloonnumber-1)+deltax
	temp1z = posz(balloonnumber-1)+deltaz
	if temp1x > 60000 or temp1x < -60000 then posx(balloonnumber) = posx(balloonnumber-1) - deltax else posx(balloonnumber) = posx(balloonnumber-1) + deltax end if
	if temp1z > 20000 or temp1z < -20000 then posz(balloonnumber) = posz(balloonnumber-1) - deltaz else posz(balloonnumber) = posz(balloonnumber-1) + deltaz end if
	ballooninit
end if

if balloonnumber=20 then
	tempx=rnd
	tempz=rnd
	if tempx <0.5 then signx = -1 else signx = 1 end if
	if tempz <0.5 then signz = -1 else signz = 1 end if
	deltax = signx * R * rnd
	deltaz = signz * R * rnd
	temp1x = posx(balloonnumber-1)+deltax
	temp1z = posz(balloonnumber-1)+deltaz
	if temp1x > 60000 or temp1x < -60000 then posx(balloonnumber) = posx(balloonnumber-1) - deltax else posx(balloonnumber) = posx(balloonnumber-1) + deltax end if
	if temp1z > 20000 or temp1z < -20000 then posz(balloonnumber) = posz(balloonnumber-1) - deltaz else posz(balloonnumber) = posz(balloonnumber-1) + deltaz end if

end if	

	balloon1.x = posx(1):balloon1.z = posz(1):balloon1.y = -100000:assignvalue 1
	balloon2.x = posx(2):balloon2.z = posz(2):balloon2.y = balloon1.y - 20000:assignvalue 2
	balloon3.x = posx(3):balloon3.z = posz(3):balloon3.y = balloon2.y - 20000:assignvalue 3
	balloon4.x = posx(4):balloon4.z = posz(4):balloon4.y = balloon3.y - 20000:assignvalue 4
	balloon5.x = posx(5):balloon5.z = posz(5):balloon5.y = balloon4.y - 20000:assignvalue 5
	balloon6.x = posx(6):balloon6.z = posz(6):balloon6.y = balloon5.y - 20000:assignvalue 6
	balloon7.x = posx(7):balloon7.z = posz(7):balloon7.y = balloon6.y - 20000:assignvalue 7
	balloon8.x = posx(8):balloon8.z = posz(8):balloon8.y = balloon7.y - 20000:assignvalue 8
	balloon9.x = posx(9):balloon9.z = posz(9):balloon9.y = balloon8.y - 20000:assignvalue 9
	balloon10.x = posx(10):balloon10.z = posz(10):balloon10.y = balloon9.y - 20000:assignvalue 10
	balloon11.x = posx(11):balloon11.z = posz(11):balloon11.y = balloon10.y - 20000:assignvalue 11
	balloon12.x = posx(12):balloon12.z = posz(12):balloon12.y = balloon11.y - 20000:assignvalue 12
	balloon13.x = posx(13):balloon13.z = posz(13):balloon13.y = balloon12.y - 20000:assignvalue 13
	balloon14.x = posx(14):balloon14.z = posz(14):balloon14.y = balloon13.y - 20000:assignvalue 14
	balloon15.x = posx(15):balloon15.z = posz(15):balloon15.y = balloon14.y - 20000:assignvalue 15
	balloon16.x = posx(16):balloon16.z = posz(16):balloon16.y = balloon15.y - 20000:assignvalue 16
	balloon17.x = posx(17):balloon17.z = posz(17):balloon17.y = balloon16.y - 20000:assignvalue 17
	balloon18.x = posx(18):balloon18.z = posz(18):balloon18.y = balloon17.y - 20000:assignvalue 18
	balloon19.x = posx(19):balloon19.z = posz(19):balloon19.y = balloon18.y - 20000:assignvalue 19
	balloon20.x = posx(20):balloon20.z = posz(20):balloon20.y = balloon19.y - 20000:assignvalue 20

	for each Obj in balloons : obj.visible = 1: next

end sub

sub assignvalue (b)

Dim random, random2, seeballoon, v

v = 1000-50*(difficulty*level-1)
random = 1000*rnd
random2 = 1000*rnd

if random<850 then 
	random=1 
elseif (random=>850 and random<980) then 
	random=2 
elseif random=>980 then 
	random=3 
end If

if random2 =< v then seeballoon = true else seeballoon = false end if

if (balloonspopped => 50) and (not extraballoffered) and (seeballoon = true) then random = "EB" : extraballoffered = true : end if

	select case b

		case 1: balloon1.image = "balloon"&random: balloon1.visible =  seeballoon
		case 2: balloon2.image = "balloon"&random: balloon2.visible =  seeballoon
		case 3: balloon3.image = "balloon"&random: balloon3.visible =  seeballoon
		case 4: balloon4.image = "balloon"&random: balloon4.visible =  seeballoon
		case 5: balloon5.image = "balloon"&random: balloon5.visible =  seeballoon
		case 6: balloon6.image = "balloon"&random: balloon6.visible =  seeballoon
		case 7: balloon7.image = "balloon"&random: balloon7.visible =  seeballoon
		case 8: balloon8.image = "balloon"&random: balloon8.visible =  seeballoon
		case 9: balloon9.image = "balloon"&random: balloon9.visible =  seeballoon
		case 10: balloon10.image = "balloon"&random: balloon10.visible =  seeballoon
		case 11: balloon11.image = "balloon"&random: balloon11.visible =  seeballoon
		case 12: balloon12.image = "balloon"&random: balloon12.visible =  seeballoon
		case 13: balloon13.image = "balloon"&random: balloon13.visible =  seeballoon
		case 14: balloon14.image = "balloon"&random: balloon14.visible =  seeballoon
		case 15: balloon15.image = "balloon"&random: balloon15.visible =  seeballoon
		case 16: balloon16.image = "balloon"&random: balloon16.visible =  seeballoon
		case 17: balloon17.image = "balloon"&random: balloon17.visible =  seeballoon
		case 18: balloon18.image = "balloon"&random: balloon18.visible =  seeballoon
		case 19: balloon19.image = "balloon"&random: balloon19.visible =  seeballoon
		case 20: balloon20.image = "balloon"&random: balloon20.visible =  seeballoon
	end Select

end sub

Sub Resettb1mission_timer()
	TB1body.y = 500
	TB1legs.y = 500
	TB1body.z = -500
	TB1legs.z = -500
	TB1body.roty = 90
	TB1legs.roty = 90
	success=false
	count = 1: count1 = 1
	level = 1
	extraballoffered = false
	vgameon = false
	moveterrain.interval = 5
	moveterrain.enabled = 0
	playsound "island", -1, 0.1, 0, 0, 0
	ones.imagea = "clear": tens.imagea = "clear": hundreds.imagea = "clear": balloongauge.imagea = "ballonspopped"
	balloonspopped = 0
	timeremaining = starttime
	updatelut.enabled = 1
	TB1legs.visible = 0
	cloudinit
	ballooninit
	updatedash
	Resettb1mission.enabled=0
end sub

sub moveterrain_timer()

timeremaining = timeremaining - 1

if timeremaining = 0 Then
	updatedash
	completed
	exit sub
end if

dim y1, y2
y2 = hit2.y
y1 = hit1.y

Dim inc, b
inc = 50 + (2 * level * difficulty)

	for each Obj in terrainA : obj.y = obj.y +  inc:next
	for each Obj in terrainB : obj.y = obj.y +  inc:next
	for each Obj in terrainC : obj.y = obj.y +  inc:next
	for each Obj in terrainD : obj.y = obj.y +  inc:next

if terrain1.y =>200000 Then
	for each Obj in terrainA : obj.y = obj.y - 200000: next
	for each Obj in terrainB : obj.y = obj.y - 200000: next
	for each Obj in terrainC : obj.y = obj.y - 200000: next
	for each Obj in terrainD : obj.y = obj.y - 200000: next
end if

	for each Obj in clouds :obj.y = obj.y + inc: if obj.y => 200000 then obj.y = obj.y -400000 end if: next

If terrain1.transy => (-20000-moveUD) and terrain1.transy =< (20000-moveUD) Then			' up / down
	for each Obj in clouds : obj.transy = obj.transy +  moveUD: next
	for each Obj in terrainall : obj.transy = obj.transy +  moveUD: next
	for each Obj in balloons : obj.transy = obj.transy +  moveUD: next
	sky.transy = sky.transy  +  moveUD:

end if

If terrain1.transz => (-60000-moveLR) and terrain1.transz =< (60000-moveLR) Then			' left / right
	for each Obj in terrainall : obj.transz = obj.transz +  moveLR: next
	for each Obj in balloons : obj.transx = obj.transx +  moveLR: next

end if

for b = 1 to 20

	select case b

		case 1: balloon1.y = balloon1.y + inc: if balloon1.y => 200000 then moveballoon 1
		case 2: balloon2.y = balloon2.y + inc: if balloon2.y => 200000 then moveballoon 2
		case 3: balloon3.y = balloon3.y + inc: if balloon3.y => 200000 then moveballoon 3
		case 4: balloon4.y = balloon4.y + inc: if balloon4.y => 200000 then moveballoon 4
		case 5: balloon5.y = balloon5.y + inc: if balloon5.y => 200000 then moveballoon 5
		case 6: balloon6.y = balloon6.y + inc: if balloon6.y => 200000 then moveballoon 6
		case 7: balloon7.y = balloon7.y + inc: if balloon7.y => 200000 then moveballoon 7
		case 8: balloon8.y = balloon8.y + inc: if balloon8.y => 200000 then moveballoon 8
		case 9: balloon9.y = balloon9.y + inc: if balloon9.y => 200000 then moveballoon 9
		case 10: balloon10.y = balloon10.y + inc: if balloon10.y => 200000 then moveballoon 10
		case 11: balloon11.y = balloon11.y + inc: if balloon11.y => 200000 then moveballoon 11
		case 12: balloon12.y = balloon12.y + inc: if balloon12.y => 200000 then moveballoon 12
		case 13: balloon13.y = balloon13.y + inc: if balloon13.y => 200000 then moveballoon 13
		case 14: balloon14.y = balloon14.y + inc: if balloon14.y => 200000 then moveballoon 14
		case 15: balloon15.y = balloon15.y + inc: if balloon15.y => 200000 then moveballoon 15
		case 16: balloon16.y = balloon16.y + inc: if balloon16.y => 200000 then moveballoon 16
		case 17: balloon17.y = balloon17.y + inc: if balloon17.y => 200000 then moveballoon 17
		case 18: balloon18.y = balloon18.y + inc: if balloon18.y => 200000 then moveballoon 18
		case 19: balloon19.y = balloon19.y + inc: if balloon19.y => 200000 then moveballoon 19
		case 20: balloon20.y = balloon20.y + inc: if balloon20.y => 200000 then moveballoon 20
	end Select
next

	for each Obj in balloons :if obj.y => 200000 then obj.y = obj.y -400000: end if: next

for b = 1 to 20

	select case b

		case 1: if balloon1.y => y1 and balloon1.y =< y2 then collisiontb1 1
		case 2: if balloon2.y => y1 and balloon2.y =< y2 then collisiontb1 2
		case 3: if balloon3.y => y1 and balloon3.y =< y2 then collisiontb1 3
		case 4: if balloon4.y => y1 and balloon4.y =< y2 then collisiontb1 4
		case 5: if balloon5.y => y1 and balloon5.y =< y2 then collisiontb1 5
		case 6: if balloon6.y => y1 and balloon6.y =< y2 then collisiontb1 6
		case 7: if balloon7.y => y1 and balloon7.y =< y2 then collisiontb1 7
		case 8: if balloon8.y => y1 and balloon8.y =< y2 then collisiontb1 8
		case 9: if balloon9.y => y1 and balloon9.y =< y2 then collisiontb1 9
		case 10: if balloon10.y => y1 and balloon10.y =< y2 then collisiontb1 10
		case 11: if balloon11.y => y1 and balloon11.y =< y2 then collisiontb1 11
		case 12: if balloon12.y => y1 and balloon12.y =< y2 then collisiontb1 12
		case 13: if balloon13.y => y1 and balloon13.y =< y2 then collisiontb1 13
		case 14: if balloon14.y => y1 and balloon14.y =< y2 then collisiontb1 14
		case 15: if balloon15.y => y1 and balloon15.y =< y2 then collisiontb1 15
		case 16: if balloon16.y => y1 and balloon16.y =< y2 then collisiontb1 16
		case 17: if balloon17.y => y1 and balloon17.y =< y2 then collisiontb1 17
		case 18: if balloon18.y => y1 and balloon18.y =< y2 then collisiontb1 18
		case 19: if balloon19.y => y1 and balloon19.y =< y2 then collisiontb1 19
		case 20: if balloon20.y => y1 and balloon20.y =< y2 then collisiontb1 20
	end Select
next

updatedash
end sub

sub moveballoon (b)

Dim signx, signz, tempx, tempz, deltax, deltaz, temp1x, temp1z
	tempx=rnd
	tempz=rnd
	if tempx <0.5 then signx = -1 else signx = 1 end if
	if tempz <0.5 then signz = -1 else signz = 1 end if
	deltax = signx * R * rnd
	deltaz = signz * R * rnd

if b >1 then
	temp1x = posx(b-1)+deltax
	temp1z = posz(b-1)+deltaz
	if temp1x > 60000 or temp1x < -60000 then posx(b) = posx(b-1) - deltax else posx(b) = posx(b-1) + deltax end if
	if temp1z > 20000 or temp1z < -20000 then posz(b) = posz(b-1) - deltaz else posz(b) = posz(b-1) + deltaz end if
else 
	temp1x = posx(20)+deltax
	temp1z = posz(20)+deltaz
	if temp1x > 60000 or temp1x < -60000 then posx(1) = posx(20) - deltax else posx(1) = posx(20) + deltax end if
	if temp1z > 20000 or temp1z < -20000 then posz(1) = posz(20) - deltaz else posz(1) = posz(20) + deltaz end if
	level = level + 1
	if level = 7 Then
		updatedash
		completed
		exit sub
	end if
end if

select case b
	case 1: balloon1.x = posx(1) : balloon1.z = posz(1): assignvalue 1
	case 2: balloon2.x = posx(2) : balloon2.z = posz(2): assignvalue 2
	case 3: balloon3.x = posx(3) : balloon3.z = posz(3): assignvalue 3
	case 4: balloon4.x = posx(4) : balloon4.z = posz(4): assignvalue 4
	case 5: balloon5.x = posx(5) : balloon5.z = posz(5): assignvalue 5
	case 6: balloon6.x = posx(6) : balloon6.z = posz(6): assignvalue 6
	case 7: balloon7.x = posx(7) : balloon7.z = posz(7): assignvalue 7
	case 8: balloon8.x = posx(8) : balloon8.z = posz(8): assignvalue 8
	case 9: balloon9.x = posx(9) : balloon9.z = posz(9): assignvalue 9
	case 10: balloon10.x = posx(10) : balloon10.z = posz(10): assignvalue 10
	case 11: balloon11.x = posx(11) : balloon11.z = posz(11): assignvalue 11
	case 12: balloon12.x = posx(12) : balloon12.z = posz(12): assignvalue 12
	case 13: balloon13.x = posx(13) : balloon13.z = posz(13): assignvalue 13
	case 14: balloon14.x = posx(14) : balloon14.z = posz(14): assignvalue 14
	case 15: balloon15.x = posx(15) : balloon15.z = posz(15): assignvalue 15
	case 16: balloon16.x = posx(16) : balloon16.z = posz(16): assignvalue 16
	case 17: balloon17.x = posx(17) : balloon17.z = posz(17): assignvalue 17
	case 18: balloon18.x = posx(18) : balloon18.z = posz(18): assignvalue 18
	case 19: balloon19.x = posx(19) : balloon19.z = posz(19): assignvalue 19
	case 20: balloon20.x = posx(20) : balloon20.z = posz(20): assignvalue 20
end Select

end sub

Sub collisiontb1 (b)

dim x1, z1, x2 , z2, collidex, collidez, hx, hz
dim offsetx: offsetx = 1250
dim offsetz: offsetz = -500

dim margin: margin = 800

x1 = -(posx(b) - offsetx) - margin
z1 = -(posz(b) - offsetz) - margin
x2 = -(posx(b) - offsetx) + margin
z2 = -(posz(b) - offsetz)+ margin
collidex = false: collidez = false

	select case b

		case 1: if balloon1.transy => z1 and balloon1.transy =< z2 then collidez = true
		case 2: if balloon2.transy => z1 and balloon2.transy =< z2 then collidez = true
		case 3: if balloon3.transy => z1 and balloon3.transy =< z2 then collidez = true
		case 4: if balloon4.transy => z1 and balloon4.transy =< z2 then collidez = true
		case 5: if balloon5.transy => z1 and balloon5.transy =< z2 then collidez = true
		case 6: if balloon6.transy => z1 and balloon6.transy =< z2 then collidez = true
		case 7: if balloon7.transy => z1 and balloon7.transy =< z2 then collidez = true
		case 8: if balloon8.transy => z1 and balloon8.transy =< z2 then collidez = true
		case 9: if balloon9.transy => z1 and balloon9.transy =< z2 then collidez = true
		case 10: if balloon10.transy => z1 and balloon10.transy =< z2 then collidez = true
		case 11: if balloon11.transy => z1 and balloon11.transy =< z2 then collidez = true
		case 12: if balloon12.transy => z1 and balloon12.transy =< z2 then collidez = true
		case 13: if balloon13.transy => z1 and balloon13.transy =< z2 then collidez = true
		case 14: if balloon14.transy => z1 and balloon14.transy =< z2 then collidez = true
		case 15: if balloon15.transy => z1 and balloon15.transy =< z2 then collidez = true
		case 16: if balloon16.transy => z1 and balloon16.transy =< z2 then collidez = true
		case 17: if balloon17.transy => z1 and balloon17.transy =< z2 then collidez = true
		case 18: if balloon18.transy => z1 and balloon18.transy =< z2 then collidez = true
		case 19: if balloon19.transy => z1 and balloon19.transy =< z2 then collidez = true
		case 20: if balloon20.transy => z1 and balloon20.transy =< z2 then collidez = true
	end Select

	select case b

		case 1: if balloon1.transx => x1 and balloon1.transx =< x2 then collidex = true
		case 2: if balloon2.transx => x1 and balloon2.transx =< x2 then collidex = true
		case 3: if balloon3.transx => x1 and balloon3.transx =< x2 then collidex = true
		case 4: if balloon4.transx => x1 and balloon4.transx =< x2 then collidex = true
		case 5: if balloon5.transx => x1 and balloon5.transx =< x2 then collidex = true
		case 6: if balloon6.transx => x1 and balloon6.transx =< x2 then collidex = true
		case 7: if balloon7.transx => x1 and balloon7.transx =< x2 then collidex = true
		case 8: if balloon8.transx => x1 and balloon8.transx =< x2 then collidex = true
		case 9: if balloon9.transx => x1 and balloon9.transx =< x2 then collidex = true
		case 10: if balloon10.transx => x1 and balloon10.transx =< x2 then collidex = true
		case 11: if balloon11.transx => x1 and balloon11.transx =< x2 then collidex = true
		case 12: if balloon12.transx => x1 and balloon12.transx =< x2 then collidex = true
		case 13: if balloon13.transx => x1 and balloon13.transx =< x2 then collidex = true
		case 14: if balloon14.transx => x1 and balloon14.transx =< x2 then collidex = true
		case 15: if balloon15.transx => x1 and balloon15.transx =< x2 then collidex = true
		case 16: if balloon16.transx => x1 and balloon16.transx =< x2 then collidex = true
		case 17: if balloon17.transx => x1 and balloon17.transx =< x2 then collidex = true
		case 18: if balloon18.transx => x1 and balloon18.transx =< x2 then collidex = true
		case 19: if balloon19.transx => x1 and balloon19.transx =< x2 then collidex = true
		case 20: if balloon20.transx => x1 and balloon20.transx =< x2 then collidex = true
	end Select

	select case b

		case 1: 
if collidex and collidez and balloon1.visible then 
explosion.enabled = 1 : balloon1.visible = 0: timeremaining = timeremaining + extendtime
if balloon1.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon1.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon1.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon1.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon1.image = "balloonEB" and (not collidex or not collidez) then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 2: 
if collidex and collidez and balloon2.visible then 
explosion.enabled = 1 : balloon2.visible = 0: timeremaining = timeremaining + extendtime
if balloon2.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon2.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon2.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon2.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon2.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 3: 
if collidex and collidez and balloon3.visible then 
explosion.enabled = 1 : balloon3.visible = 0: timeremaining = timeremaining + extendtime
if balloon3.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon3.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon3.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon3.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon3.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 4: 
if collidex and collidez and balloon4.visible then 
explosion.enabled = 1 : balloon4.visible = 0: timeremaining = timeremaining + extendtime
if balloon4.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon4.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon4.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon4.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon4.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 5: 
if collidex and collidez and balloon5.visible then 
explosion.enabled = 1 : balloon5.visible = 0: timeremaining = timeremaining + extendtime
if balloon5.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon5.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon5.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon5.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon5.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 6: 
if collidex and collidez and balloon6.visible then 
explosion.enabled = 1 : balloon6.visible = 0: timeremaining = timeremaining + extendtime
if balloon6.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon6.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon6.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon6.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon6.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 7: 
if collidex and collidez and balloon7.visible then 
explosion.enabled = 1 : balloon7.visible = 0: timeremaining = timeremaining + extendtime
if balloon7.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon7.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon7.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon7.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon7.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 8: 
if collidex and collidez and balloon8.visible then 
explosion.enabled = 1 : balloon8.visible = 0: timeremaining = timeremaining + extendtime
if balloon8.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon8.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon8.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon8.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon8.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 9: 
if collidex and collidez and balloon9.visible then 
explosion.enabled = 1 : balloon9.visible = 0: timeremaining = timeremaining + extendtime
if balloon9.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon9.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon9.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon9.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon9.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 10: 
if collidex and collidez and balloon10.visible then 
explosion.enabled = 1 : balloon10.visible = 0: timeremaining = timeremaining + extendtime
if balloon10.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon10.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon10.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon10.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon10.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 11: 
if collidex and collidez and balloon11.visible then 
explosion.enabled = 1 : balloon11.visible = 0: timeremaining = timeremaining + extendtime
if balloon11.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon11.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon11.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon11.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon11.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 12: 
if collidex and collidez and balloon12.visible then 
explosion.enabled = 1 : balloon12.visible = 0: timeremaining = timeremaining + extendtime
if balloon12.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon12.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon12.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon12.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon12.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 13: 
if collidex and collidez and balloon13.visible then 
explosion.enabled = 1 : balloon13.visible = 0: timeremaining = timeremaining + extendtime
if balloon13.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon13.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon13.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon13.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon13.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 14: 
if collidex and collidez and balloon14.visible then 
explosion.enabled = 1 : balloon14.visible = 0: timeremaining = timeremaining + extendtime
if balloon14.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon14.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon14.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon14.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon14.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 15: 
if collidex and collidez and balloon15.visible then 
explosion.enabled = 1 : balloon15.visible = 0: timeremaining = timeremaining + extendtime
if balloon15.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon15.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon15.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon15.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon15.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 16: 
if collidex and collidez and balloon16.visible then 
explosion.enabled = 1 : balloon16.visible = 0: timeremaining = timeremaining + extendtime
if balloon16.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon16.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon16.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon16.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon16.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 17: 
if collidex and collidez and balloon17.visible then 
explosion.enabled = 1 : balloon17.visible = 0: timeremaining = timeremaining + extendtime
if balloon17.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon17.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon17.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon17.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon17.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 18: 
if collidex and collidez and balloon18.visible then 
explosion.enabled = 1 : balloon18.visible = 0: timeremaining = timeremaining + extendtime
if balloon18.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon18.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon18.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon18.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon18.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 19: 
if collidex and collidez and balloon19.visible then 
explosion.enabled = 1 : balloon19.visible = 0: timeremaining = timeremaining + extendtime
if balloon19.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon19.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon19.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon19.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon19.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if
		case 20: 
if collidex and collidez and balloon20.visible then 
explosion.enabled = 1 : balloon20.visible = 0: timeremaining = timeremaining + extendtime
if balloon20.image = "balloon1" then balloonspopped = balloonspopped + 1 end if
if balloon20.image = "balloon2" then balloonspopped = balloonspopped + 2 end if
if balloon20.image = "balloon3" then balloonspopped = balloonspopped + 3 end if
if balloon20.image = "balloonEB" then Extraball=true :PlaySound "happy", 0, 100, 0, 0, 0: end if
Elseif balloon20.image = "balloonEB" and (not collidex or not collidez)then 
PlaySound "fail", 0, 0.25, 0, 0, 0
end if

end Select

collidex = false: collidez = false
 
End sub


' ***************** CODE BELOW IS FOR THE gauges   *******************************
'*****************************************************************************************************************************************

Sub updatedash()

Dim popped, totalt

totalt = timeremaining

popped = balloonspopped

Dim temp1, temp2, temp3, temp4, temp5, temp6, min, sec, hunds

temp1 = popped \ 100
temp2 = (popped - 100*temp1) \ 10
temp3 = (popped - 100*temp1 - 10*temp2)

if popped< 100 then hundreds.imagea = "clear" else hundreds.imagea = "digit" & CStr(temp1)
if popped< 10 then tens.imagea = "clear" else tens.imagea = "digit" & CStr(temp2)
ones.imagea = "digit" & CStr(temp3)

thousands.imagea = "digit" & level

    ' **************************************
' time remaining - TotalT. divisions multiplied by 2 due to a 5ms timer - needs to be 10ms to be 1

temp1 = Int(totalT/120000)
temp2 = Int((totalT-temp1*120000)/12000)
temp3 = Int((totalT-temp1*120000-temp2*12000)/2000)
temp4 = Int((totalT-temp1*120000-temp2*12000-temp3*2000)/200)
temp5 = Int((totalT-temp1*120000-temp2*12000-temp3*2000-temp4*200)/20)
temp6 = Int((totalT-temp1*120000-temp2*12000-temp3*2000-temp4*200-temp5*20)/2)

if totalt =<0 then
	temp1=0:temp2=0:temp3=0:temp4=0:temp5=0:temp6=0 
end if

hour1.imagea="digit"&temp1
hour2.imagea="digit"&temp2

minute1.imagea="digit"&temp3
minute2.imagea="digit"&temp4

second1.imagea="digit"&temp5
second2.imagea="digit"&temp6

End Sub

Sub completed()
		for each Obj in balloons : obj.visible = 0: next
		moveterrain.enabled = 0
		success=true
		screen = 0
		stopsound "jet"
		PlaySound "victory", 0, 100, 0, 0, 0
		intro.enabled = 1
		outro = true
		TB1legs.visible = 1
		moveTB1.enabled = 1
end sub

sub moveTB1_timer()

dim terz

terz = terrain1.z + terrain1.transy + 4000

if TB1body.y=> -100000 then
	TB1body.y = TB1body.y - 200
	TB1legs.y = TB1legs.y - 200
end If

if TB1body.y=< -100000 then
		if TB1body.roty <180 then
			TB1body.roty = TB1body.roty + 2
			TB1legs.roty = TB1legs.roty + 2
		end if
		if TB1body.roty =>180 then
			if TB1body.z=> terz then
				TB1body.z = TB1body.z - 200
				TB1legs.z = TB1legs.z - 200
			Else
				TB1body.z = terz
				TB1legs.z = terz
				moveTB1.enabled = 0
			end if
		end if
end if

end sub

'***************TB4 Mission   *************************

sub TB4missionsetup()
	difficulty4 = TB4difficulty		'1 for easy, 2 for intermediate, 3 for impossible
	corridory0 = corridor.y : corridor1y0= corridor1.y : corridor2y0= corridor2.y : corridor3y0= corridor3.y : corridor4y0= corridor4.y: corridor5y0= corridor5.y
	count = 1
	count1 = 1
	screen = 0
	outro = false
	success=false
	level = 0
	collided = false
	minesmissed=0
	direction = True	'true for right to left, false for left to right
	side = True	'true for left, false for right
	expand = true
	mineleftright = (3000)/5
	mineupdown = (3500)/5
	deltax = 6250
	leftright = (deltax/2)/5	
	deltaz=7000
	updown = (deltaz/2)/5
	corridorinterval = 400
	SetBackglasstb4mission()
	movecorridor.enabled = 1
	intro.enabled = 1
	submarine.visible = 0
	fish3.visible = 0
	fish3fins.visible = 0
	vgameon = false
	updatelut.enabled = 1
	movecorridor.interval = 20
	movecorridor.enabled = 0
	playsound "undersea", -1, 0.1, 0, 0, 0
	resetmine
	fishtimer.enabled = 1
	fish2timer.enabled = 0
	collided = false
	hit1.x = 493: hit1.y = 200
	hit2.x = 493: hit2.y = 800
end sub

Sub TB4missionend()
	success=false
	minesmissed=0
	collided = false
	count = 1: count1 = 1
	level = 0
	submarine.visible = 0
	fish3.visible = 0
	fish3fins.visible = 0
	Submarine.roty = 45
	Submarine.x = 20000
	Submarine.y = -333000
	Submarine.z = -20000
	Fish3.x = -22000
	Fish3Fins.x = -22000
	Fish3.y = -334500
	Fish3Fins.y = -334500
	Fish3.z = -15000
	Fish3Fins.z = -15000
	vgameon = false
	movecorridor.interval = 20
	movecorridor.enabled = 0
	intro.enabled = 0
	stopsound "undersea"
	resetcorridors()
	resetmine()
	briefing.imagea = "clear"
	briefing.height = 0
	counter.height = 0
	displaymine0.imagea = "clear": displaymine1.imagea = "clear": displaymine2.imagea = "clear": displaylevel.imagea = "clear"
	fishtimer.enabled = 0
	fish2timer.enabled = 0
	fish5timer.enabled = 0
	updatelut.enabled = 1
	for each Obj in display : obj.visible = 0 : next
	for each Obj in corridors : obj.visible = 0 : next
	for each Obj in fish1 : obj.visible = 0 : next
	for each Obj in tb4bits : obj.visible = 0 : next
	for each Obj in tb4yokedisplay : obj.visible = 0 : next
End sub

'********************  Flasher position  *******************

Sub SetBackglasstb4mission()

For Each obj In display

obj.x = obj.x - 10
obj.height = 1000
obj.y = -5000   'adjusts the distance from the backglass towards the user

Next

For Each obj In tb4yokedisplay

'obj.x = obj.x - 10

'obj.height = 950

obj.y = 1755   'adjusts the distance from the backglass towards the user

Next

for each Obj in display : obj.visible = 1 : next
for each Obj in corridors : obj.visible = 1 : next
for each Obj in fish1 : obj.visible = 1 : next
for each Obj in tb4bits : obj.visible = 1 : next
for each Obj in tb4yokedisplay : obj.visible = 1 : next


End Sub

Sub Resettb4mission_timer()
	success=false
	minesmissed=0
	collided = false
	count = 1: count1 = 1
	level = 0
	submarine.visible = 0
	fish3.visible = 0
	fish3fins.visible = 0
	Submarine.roty = 45
	Submarine.x = 20000
	Submarine.y = -333000
	Submarine.z = -20000
	Fish3.x = -22000
	Fish3Fins.x = -22000
	Fish3.y = -334500
	Fish3Fins.y = -334500
	Fish3.z = -15000
	Fish3Fins.z = -15000
	vgameon = false
	movecorridor.interval = 20
	movecorridor.enabled = 0
	playsound "undersea", -1, 0.1, 0, 0, 0
	resetcorridors()
	resetmine()
	displaymine0.imagea = "clear": displaymine1.imagea = "clear": displaymine2.imagea = "clear": displaylevel.imagea = "clear"
	fishtimer.enabled = 1
	fish2timer.enabled = 0
	fish5timer.enabled = 0
	resettb4mission.enabled=0
	updatelut.enabled = 1
end sub


Sub resetmine()

	for each obj in Minegroup1 :obj.y = marker1.y - 600 :next
	for each obj in Minegroup2 :obj.y = marker1.y - (-marker1.y + marker.y) - 600 :next
	for each obj in Minegroup3 :obj.y = marker1.y - (-2*marker1.y + 2*marker.y) - 600 :next
	for each obj in Minegroup4 :obj.y = marker1.y - (-3*marker1.y + 3*marker.y) - 600 :next
	for each obj in Minegroup5 :obj.y = marker1.y - (-4*marker1.y + 4*marker.y) - 600 :next
	for each obj in Minegroup6 :obj.y = marker1.y - (-5*marker1.y + 5*marker.y) - 600 :next

	for each obj in Minegroup1 :obj.visible = 0 :next
	for each obj in Minegroup2 :obj.visible = 0 :next
	for each obj in Minegroup3 :obj.visible = 0 :next
	for each obj in Minegroup4 :obj.visible = 0 :next
	for each obj in Minegroup5 :obj.visible = 0 :next
	for each obj in Minegroup6 :obj.visible = 0 :next

	for obj = 1 to 70
		Mine(obj) = False	
	next

end sub

sub resetcorridors()

corridor.y = corridory0 : corridor1.y= corridor1y0 : corridor2.y= corridor2y0 : corridor3.y= corridor3y0 : corridor4.y= corridor4y0: corridor5.y= corridor5y0

end sub

Sub speedup_timer()

	count1 = count1 + 1

dim var
var = level * difficulty4

	select case count1

		case 5000
			level = 1
			movecorridor.interval = movecorridor.interval - var
		case 20000
			level = 2
			movecorridor.interval = movecorridor.interval - var
		case 35000
			level = 3
			movecorridor.interval = movecorridor.interval - var
		case 50000
			level = 4
			movecorridor.interval = movecorridor.interval - var
		case 62000
			stopsound "tb4drama"
			PlaySound "drama", 0, 100, 0, 0, 0
		case 65000
			level = 5
			movecorridor.interval = movecorridor.interval - var
		case 80000
			level = 6
			speedup.enabled = 0
	end select

end sub

sub movecorridor_timer()

Dim var

var = level

	corridor.y = corridor.y + corridorinterval
	corridor1.y = corridor1.y + corridorinterval
	corridor2.y = corridor2.y + corridorinterval
	corridor3.y = corridor3.y + corridorinterval
	corridor4.y = corridor4.y + corridorinterval
	corridor5.y = corridor5.y + corridorinterval

	for each obj in Minegroup1 :obj.y = obj.y + corridorinterval :next
	for each obj in Minegroup2 :obj.y = obj.y + corridorinterval :next
	for each obj in Minegroup3 :obj.y = obj.y + corridorinterval :next
	for each obj in Minegroup4 :obj.y = obj.y + corridorinterval :next
	for each obj in Minegroup5 :obj.y = obj.y + corridorinterval :next
	for each obj in Minegroup6 :obj.y = obj.y + corridorinterval :next

	if corridor1.y => corridory0 and success = false Then
		corridor.y = corridory0 : corridor1.y= corridor1y0 : corridor2.y= corridor2y0 : corridor3.y= corridor3y0 : corridor4.y= corridor4y0: corridor5.y= corridor5y0
	end if

	displaylevel.imagea = "digit"&var

	if success = true Then
		if corridor5.y => - 2*corridory0 then movecorridor.enabled = 0	end if
		exit sub
	end if

	if level =>6 then
		success=true
		screen = 0
		for each obj in Minegroup1 :obj.visible = 0 :next: for obj = 11 to 19: mine(obj)=false: next
		for each obj in Minegroup2 :obj.visible = 0 :next: for obj = 21 to 29: mine(obj)=false: next
		for each obj in Minegroup3 :obj.visible = 0 :next: for obj = 31 to 39: mine(obj)=false: next
		for each obj in Minegroup4 :obj.visible = 0 :next: for obj = 41 to 49: mine(obj)=false: next
		for each obj in Minegroup5 :obj.visible = 0 :next: for obj = 51 to 59: mine(obj)=false: next
		for each obj in Minegroup6 :obj.visible = 0 :next: for obj = 61 to 69: mine(obj)=false: next
		stopsound "tb4drama"
		stopsound "drama"
		PlaySound "success", 0, 100, 0, 0, 0
		submarine.visible = 1
		fish3.visible = 1
		fish3fins.visible = 1
		submarinetimer.enabled = 1
		fish4timer.enabled = 1
		intro.enabled = 1
		outro = true
		exit sub
	end if

	TB4location()

	if Mineball11.y => (hit1.y - 600) and Mineball11.y <= (hit2.y - 600) then collisiontb4 1,tb4position
	if Mineball21.y => (hit1.y - 600) and Mineball21.y <= (hit2.y - 600) then collisiontb4 2,tb4position
	if Mineball31.y => (hit1.y - 600) and Mineball31.y <= (hit2.y - 600) then collisiontb4 3,tb4position
	if Mineball41.y => (hit1.y - 600) and Mineball41.y <= (hit2.y - 600) then collisiontb4 4,tb4position
	if Mineball51.y => (hit1.y - 600) and Mineball51.y <= (hit2.y - 600) then collisiontb4 5,tb4position
	if Mineball61.y => (hit1.y - 600) and Mineball61.y <= (hit2.y - 600) then collisiontb4 6,tb4position 

	if collided then 
		explosion.enabled = 1
		movecorridor.enabled = 0
		speedup.enabled = 0
	end if

	if Mineball11.y => (marker0.y  - 600) then :for each obj in Minegroup1 :obj.y = marker5.y - 600 :next:for each obj in Minegroup1 :obj.visible = 0 :next: minespassed 1:for obj = 11 to 19: mine(obj)=false: next: displaymine 1,var: end if
	if Mineball21.y => (marker0.y  - 600) then :for each obj in Minegroup2 :obj.y = marker5.y - 600 :next:for each obj in Minegroup2 :obj.visible = 0 :next: minespassed 2:for obj = 21 to 29: mine(obj)=false: next: displaymine 2,var: end if
	if Mineball31.y => (marker0.y  - 600) then :for each obj in Minegroup3 :obj.y = marker5.y - 600 :next:for each obj in Minegroup3 :obj.visible = 0 :next: minespassed 3:for obj = 31 to 39:mine(obj)=false: next: displaymine 3,var: end if
	if Mineball41.y => (marker0.y  - 600) then :for each obj in Minegroup4 :obj.y = marker5.y - 600 :next:for each obj in Minegroup4 :obj.visible = 0 :next: minespassed 4:for obj = 41 to 49:mine(obj)=false: next: displaymine 4,var: end if
	if Mineball51.y => (marker0.y  - 600) then :for each obj in Minegroup5 :obj.y = marker5.y - 600 :next:for each obj in Minegroup5 :obj.visible = 0 :next: minespassed 5:for obj = 51 to 59:mine(obj)=false: next: displaymine 5,var: end if
	if Mineball61.y => (marker0.y  - 600) then :count = count + 1: for each obj in Minegroup6 :obj.y = marker5.y - 600 :next:for each obj in Minegroup6 :obj.visible = 0 :next: minespassed 6:for obj = 61 to 69:mine(obj)=false: next: displaymine 6,var: end if

end sub

sub minespassed (passed)

	Dim a, b, c 

	a = 10*passed + 1
	b = 10*passed + 9

	for c = a to b
		if Mine(c) = true then minesmissed = minesmissed+1
	next

end sub

Sub TB4left_timer

if corridor.x =>int(500+(deltax/2)) then :corridor.x = int(500+(deltax/2)): TB4left.enabled = 0: exit sub: end if

	corridor.x = corridor.x + leftright
	corridor1.x = corridor1.x + leftright
	corridor2.x = corridor2.x + leftright
	corridor3.x = corridor3.x + leftright
	corridor4.x = corridor4.x + leftright
	corridor5.x = corridor5.x + leftright

	for each obj in Minegroup1 :obj.X = obj.X + mineleftright :next
	for each obj in Minegroup2 :obj.X = obj.X + mineleftright :next
	for each obj in Minegroup3 :obj.X = obj.x + mineleftright :next
	for each obj in Minegroup4 :obj.X = obj.x + mineleftright :next
	for each obj in Minegroup5 :obj.X = obj.x + mineleftright :next
	for each obj in Minegroup6 :obj.X = obj.x + mineleftright :next

if int(corridor.x) = 500 then :corridor.x = 500: TB4left.enabled = 0: end if
'if not LFF.enabled and int(corridor.x) = 500 then :corridor.x = 500: TB4left.enabled = 0: end if

end Sub

Sub TB4right_timer

if corridor.x <= int(500-(deltax/2)) then :corridor.x = int(500-(deltax/2)): TB4right.enabled = 0: exit sub: end if

	corridor.x = corridor.x - leftright
	corridor1.x = corridor1.x - leftright
	corridor2.x = corridor2.x - leftright
	corridor3.x = corridor3.x - leftright
	corridor4.x = corridor4.x - leftright
	corridor5.x = corridor5.x - leftright

	for each obj in Minegroup1 :obj.X = obj.X - mineleftright :next
	for each obj in Minegroup2 :obj.X = obj.X - mineleftright :next
	for each obj in Minegroup3 :obj.X = obj.x - mineleftright :next
	for each obj in Minegroup4 :obj.X = obj.x - mineleftright :next
	for each obj in Minegroup5 :obj.X = obj.x - mineleftright :next
	for each obj in Minegroup6 :obj.X = obj.x - mineleftright :next

if int(corridor.x) = 500 then :corridor.x = 500: TB4right.enabled = 0: end if
'if not RFF.enabled and int(corridor.x) = 500 then :corridor.x = 500: TB4right.enabled = 0: end if

end Sub

Sub TB4down_timer

if corridor.z => int(-7500+(deltaz/2)) then :corridor.z = int(-7500+(deltaz/2)): TB4down.enabled = 0: exit sub: end if

	corridor.z = corridor.z + updown
	corridor1.z = corridor1.z + updown
	corridor2.z = corridor2.z + updown
	corridor3.z = corridor3.z + updown
	corridor4.z = corridor4.z + updown
	corridor5.z = corridor5.z + updown

	for each obj in Minegroup1 :obj.z = obj.z + mineupdown :next
	for each obj in Minegroup2 :obj.z = obj.z + mineupdown :next
	for each obj in Minegroup3 :obj.z = obj.z + mineupdown :next
	for each obj in Minegroup4 :obj.z = obj.z + mineupdown :next
	for each obj in Minegroup5 :obj.z = obj.z + mineupdown :next
	for each obj in Minegroup6 :obj.z = obj.z + mineupdown :next

if int(corridor.z) = -7500 then :corridor.z = -7500: TB4down.enabled = 0: end if
'if not LMS.enabled and int(corridor.z) = -7500 then :corridor.z = -7500: TB4down.enabled = 0: end if

end Sub

Sub TB4up_timer

if corridor.z <= int(-7500-(deltaz/2)) then :corridor.z = int(-7500-(deltaz/2)): TB4up.enabled = 0: exit sub: end if

	corridor.z = corridor.z - updown
	corridor1.z = corridor1.z - updown
	corridor2.z = corridor2.z - updown
	corridor3.z = corridor3.z - updown
	corridor4.z = corridor4.z - updown
	corridor5.z = corridor5.z - updown



	for each obj in Minegroup1 :obj.z = obj.z - mineupdown :next
	for each obj in Minegroup2 :obj.z = obj.z - mineupdown :next
	for each obj in Minegroup3 :obj.z = obj.z - mineupdown :next
	for each obj in Minegroup4 :obj.z = obj.z - mineupdown :next
	for each obj in Minegroup5 :obj.z = obj.z - mineupdown :next
	for each obj in Minegroup6 :obj.z = obj.z - mineupdown :next

if int(corridor.z) = -7500 then :corridor.z = -7500: TB4up.enabled = 0: end if
'if not RMS.enabled and int(corridor.z) = -7500 then :corridor.z = -7500: TB4up.enabled = 0: end if

end Sub

sub displaymine (minerow,minelevel)

	Dim temp1, temp2, temp3

	temp1 = minesmissed \ 100
	temp2 = (minesmissed - 100*temp1) \ 10
	temp3 = (minesmissed - 100*temp1 - 10*temp2)

	if minesmissed< 100 then displaymine2.imagea = "clear" else displaymine2.imagea = "digit" & CStr(temp1)
	if minesmissed< 10 then displaymine1.imagea = "clear" else displaymine1.imagea = "digit" & CStr(temp2)
	displaymine0.imagea = "digit" & CStr(temp3)

	dim random: random = Int(9*rnd(1)+1)

select case minerow

case 1:

	select Case random

		case 1: 
			if minelevel=>1 and mineball11.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball11.visible then mineball11.visible=1: mine(11)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball11.visible=1:mine(11)=true
		case 2:
			if minelevel=>1 and mineball12.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball12.visible then mineball12.visible=1: mine(12)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball12.visible=1:mine(12)=true
		case 3: 
			if minelevel=>1 and mineball13.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball13.visible then mineball13.visible=1: mine(13)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball13.visible=1:mine(13)=true
		case 4:
			if minelevel=>1 and mineball14.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball14.visible then mineball14.visible=1: mine(14)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball14.visible=1:mine(14)=true
		case 5:
			if minelevel=>1 and mineball15.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball15.visible then mineball15.visible=1: mine(15)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball15.visible=1:mine(15)=true
		case 6:
			if minelevel=>1 and mineball16.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball16.visible then mineball16.visible=1: mine(16)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball16.visible=1:mine(16)=true
		case 7:
			if minelevel=>1 and mineball17.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball17.visible then mineball17.visible=1: mine(17)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball17.visible=1:mine(17)=true
		case 8:
			if minelevel=>1 and mineball18.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball18.visible then mineball18.visible=1: mine(18)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball18.visible=1:mine(18)=true
		case 9: 
			if minelevel=>1 and mineball19.visible then displaymine minerow,minelevel: exit sub 
			if minelevel>1 and not mineball19.visible then mineball19.visible=1: mine(19)=true: minelevel=minelevel-1:displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball19.visible=1:mine(19)=true

	end select

case 2:

	select Case random

		case 1: 
			if minelevel=>1 and mineball21.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball21.visible then mineball21.visible=1: mine(21)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball21.visible=1: mine(21)=true: 
		case 2:
			if minelevel=>1 and mineball22.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball22.visible then mineball22.visible=1: mine(22)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball22.visible=1: mine(22)=true: 
		case 3: 
			if minelevel=>1 and mineball23.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball23.visible then mineball23.visible=1: mine(23)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball23.visible=1: mine(23)=true: 
		case 4:
			if minelevel=>1 and mineball24.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball24.visible then mineball24.visible=1: mine(24)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball24.visible=1: mine(24)=true: 
		case 5:
			if minelevel=>1 and mineball25.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball25.visible then mineball25.visible=1: mine(25)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball25.visible=1: mine(25)=true: 
		case 6:
			if minelevel=>1 and mineball26.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball26.visible then mineball26.visible=1: mine(26)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball26.visible=1: mine(26)=true: 
		case 7:
			if minelevel=>1 and mineball27.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball27.visible then mineball27.visible=1: mine(27)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball27.visible=1: mine(27)=true: 
		case 8:
			if minelevel=>1 and mineball28.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball28.visible then mineball28.visible=1: mine(28)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball28.visible=1: mine(28)=true: 
		case 9: 
			if minelevel=>1 and mineball29.visible then displaymine minerow,minelevel: exit sub 
			if minelevel>1 and not mineball29.visible then mineball29.visible=1: mine(29)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball29.visible=1: mine(29)=true: 

	end select

case 3:

	select Case random

		case 1: 
			if minelevel=>1 and mineball31.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball31.visible then mineball31.visible=1: mine(31)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball31.visible=1: mine(31)=true: 
		case 2:
			if minelevel=>1 and mineball32.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball32.visible then mineball32.visible=1: mine(32)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball32.visible=1: mine(32)=true: 
		case 3: 
			if minelevel=>1 and mineball33.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball33.visible then mineball33.visible=1: mine(33)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball33.visible=1: mine(33)=true: 
		case 4:
			if minelevel=>1 and mineball34.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball34.visible then mineball34.visible=1: mine(34)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball34.visible=1: mine(34)=true: 
		case 5:
			if minelevel=>1 and mineball35.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball35.visible then mineball35.visible=1: mine(35)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball35.visible=1: mine(35)=true: 
		case 6:
			if minelevel=>1 and mineball36.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball36.visible then mineball36.visible=1: mine(36)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball36.visible=1: mine(36)=true: 
		case 7:
			if minelevel=>1 and mineball37.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball37.visible then mineball37.visible=1: mine(37)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball37.visible=1: mine(37)=true: 
		case 8:
			if minelevel=1 and mineball38.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball38.visible then mineball38.visible=1: mine(38)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball38.visible=1: mine(38)=true: 
		case 9: 
			if minelevel=>1 and mineball39.visible then displaymine minerow,minelevel: exit sub 
			if minelevel>1 and not mineball39.visible then mineball39.visible=1: mine(39)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball39.visible=1: mine(39)=true: 

	end select

case 4:

	select Case random

		case 1: 
			if minelevel=>1 and mineball41.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball41.visible then mineball41.visible=1: mine(41)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball41.visible=1: mine(41)=true: 
		case 2:
			if minelevel=>1 and mineball42.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball42.visible then mineball42.visible=1: mine(42)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball42.visible=1: mine(42)=true: 
		case 3: 
			if minelevel=>1 and mineball43.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball43.visible then mineball43.visible=1: mine(43)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball43.visible=1: mine(43)=true: 
		case 4:
			if minelevel=>1 and mineball44.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball44.visible then mineball44.visible=1: mine(44)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball44.visible=1: mine(44)=true: 
		case 5:
			if minelevel=>1 and mineball45.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball45.visible then mineball45.visible=1: mine(45)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball45.visible=1: mine(45)=true: 
		case 6:
			if minelevel=>1 and mineball46.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball46.visible then mineball46.visible=1: mine(46)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball46.visible=1: mine(46)=true: 
		case 7:
			if minelevel=>1 and mineball47.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball47.visible then mineball47.visible=1: mine(47)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball47.visible=1: mine(47)=true: 
		case 8:
			if minelevel=>1 and mineball48.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball48.visible then mineball48.visible=1: mine(48)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball48.visible=1: mine(48)=true: 
		case 9: 
			if minelevel=>1 and mineball49.visible then displaymine minerow,minelevel: exit sub 
			if minelevel>1 and not mineball49.visible then mineball49.visible=1: mine(49)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball49.visible=1: mine(49)=true: 

	end select

case 5:

	select Case random

		case 1: 
			if minelevel=>1 and mineball51.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball51.visible then mineball51.visible=1: mine(51)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball51.visible=1: mine(51)=true: 
		case 2:
			if minelevel=>1 and mineball52.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball52.visible then mineball52.visible=1: mine(52)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball52.visible=1: mine(52)=true: 
		case 3: 
			if minelevel=>1 and mineball53.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball53.visible then mineball53.visible=1: mine(53)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball53.visible=1: mine(53)=true: 
		case 4:
			if minelevel=>1 and mineball54.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball54.visible then mineball54.visible=1: mine(54)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball54.visible=1: mine(54)=true: 
		case 5:
			if minelevel=>1 and mineball55.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball55.visible then mineball55.visible=1: mine(55)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball55.visible=1: mine(55)=true: 
		case 6:
			if minelevel=>1 and mineball56.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball56.visible then mineball56.visible=1: mine(56)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball56.visible=1: mine(56)=true: 
		case 7:
			if minelevel=>1 and mineball57.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball57.visible then mineball57.visible=1: mine(57)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball57.visible=1: mine(57)=true: 
		case 8:
			if minelevel=>1 and mineball58.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball58.visible then mineball58.visible=1: mine(58)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball58.visible=1: mine(58)=true: 
		case 9: 
			if minelevel=>1 and mineball59.visible then displaymine minerow,minelevel: exit sub 
			if minelevel>1 and not mineball59.visible then mineball59.visible=1: mine(59)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball59.visible=1: mine(59)=true: 

	end select

case 6:

	select Case random

		case 1: 
			if minelevel=>1 and mineball61.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball61.visible then mineball61.visible=1: mine(61)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball61.visible=1: mine(61)=true: 
		case 2:
			if minelevel=>1 and mineball62.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball62.visible then mineball62.visible=1: mine(62)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball62.visible=1: mine(62)=true: 
		case 3: 
			if minelevel=>1 and mineball63.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball63.visible then mineball63.visible=1: mine(63)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball63.visible=1: mine(63)=true: 
		case 4:
			if minelevel=>1 and mineball64.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball64.visible then mineball64.visible=1: mine(64)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball64.visible=1: mine(64)=true: 
		case 5:
			if minelevel=>1 and mineball65.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball65.visible then mineball65.visible=1: mine(65)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball65.visible=1: mine(65)=true: 
		case 6:
			if minelevel=>1 and mineball66.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball66.visible then mineball66.visible=1: mine(66)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball66.visible=1: mine(66)=true: 
		case 7:
			if minelevel=>1 and mineball67.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball67.visible then mineball67.visible=1: mine(67)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball67.visible=1: mine(67)=true: 
		case 8:
			if minelevel=>1 and mineball68.visible then displaymine minerow,minelevel: exit sub
			if minelevel>1 and not mineball68.visible then mineball68.visible=1: mine(68)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball68.visible=1: mine(68)=true: 
		case 9: 
			if minelevel=>1 and mineball69.visible then displaymine minerow,minelevel: exit sub 
			if minelevel>1 and not mineball69.visible then mineball69.visible=1: mine(69)=true: : minelevel=int(minelevel-1):displaymine minerow,minelevel: exit sub
			if minelevel=1 then mineball69.visible=1: mine(69)=true: 

	end select

end select

end sub

Sub TB4location()

	Dim x
	Dim z

	x = Int(corridor.x)
	z = Int(corridor.z)

select case x

Case 500 + (deltax/2)

	select case z
		case -7500 - (deltaz/2)
			TB4position = 1
		Case -7500
			TB4position = 4
		Case -7500 + (deltaz/2)
			TB4position = 7
	end select

Case 500

	select case z
		case -7500 - (deltaz/2)
			TB4position = 2
		Case -7500
			TB4position = 5
		Case -7500 + (deltaz/2)
			TB4position = 8
	end select

Case 500 - (deltax/2)

	select case z
		case -7500 - (deltaz/2)
			TB4position = 3
		Case -7500
			TB4position = 6
		Case -7500 + (deltaz/2)
			TB4position = 9
	end select

End select

end sub

Sub collisiontb4 (minerow1, TB4p)

Dim TB4pos : 

TB4pos = int(10*minerow1 + TB4p)

select case TB4pos

		case 11: 	if mine(11) then collided = true: mineball11.visible = 0: end if
		case 12:	if mine(12) then collided = true: mineball12.visible = 0: end if
		case 13: 	if mine(13) then collided = true: mineball13.visible = 0: end if
		case 14:	if mine(14) then collided = true: mineball14.visible = 0: end if
		case 15:	if mine(15) then collided = true: mineball15.visible = 0: end if
		case 16:	if mine(16) then collided = true: mineball16.visible = 0: end if
		case 17:	if mine(17) then collided = true: mineball17.visible = 0: end if
		case 18: 	if mine(18) then collided = true: mineball18.visible = 0: end if
		case 19: 	if mine(19) then collided = true: mineball19.visible = 0: end if

		case 21: 	if mine(21) then collided = true: mineball21.visible = 0: end if
		case 22:	if mine(22) then collided = true: mineball22.visible = 0: end if
		case 23:	if mine(23) then collided = true: mineball23.visible = 0: end if
		case 24:	if mine(24) then collided = true: mineball24.visible = 0: end if
		case 25:	if mine(25) then collided = true: mineball25.visible = 0: end if
		case 26:	if mine(26) then collided = true: mineball26.visible = 0: end if
		case 27:	if mine(27) then collided = true: mineball27.visible = 0: end if
		case 28:	if mine(28) then collided = true: mineball28.visible = 0: end if
		case 29:	if mine(29) then collided = true: mineball29.visible = 0: end if

		case 31:	if mine(31) then collided = true: mineball31.visible = 0: end if
		case 32:	if mine(32) then collided = true: mineball32.visible = 0: end if
		case 33:	if mine(33) then collided = true: mineball33.visible = 0: end if
		case 34:	if mine(34) then collided = true: mineball34.visible = 0: end if
		case 35:	if mine(35) then collided = true: mineball35.visible = 0: end if
		case 36:	if mine(36) then collided = true: mineball36.visible = 0: end if
		case 37:	if mine(37) then collided = true: mineball37.visible = 0: end if
		case 38:	if mine(38) then collided = true: mineball38.visible = 0: end if
		case 39:	if mine(39) then collided = true: mineball39.visible = 0: end if

		case 41:	if mine(41) then collided = true: mineball41.visible = 0: end if
		case 42:	if mine(42) then collided = true: mineball42.visible = 0: end if
		case 43:	if mine(43) then collided = true: mineball43.visible = 0: end if
		case 44:	if mine(44) then collided = true: mineball44.visible = 0: end if
		case 45:	if mine(45) then collided = true: mineball45.visible = 0: end if
		case 46:	if mine(46) then collided = true: mineball46.visible = 0: end if
		case 47:	if mine(47) then collided = true: mineball47.visible = 0: end if
		case 48:	if mine(48) then collided = true: mineball48.visible = 0: end if
		case 49:	if mine(49) then collided = true: mineball49.visible = 0: end if


		case 51:	if mine(51) then collided = true: mineball51.visible = 0: end if
		case 52:	if mine(52) then collided = true: mineball52.visible = 0: end if
		case 53:	if mine(53) then collided = true: mineball53.visible = 0: end if
		case 54:	if mine(54) then collided = true: mineball54.visible = 0: end if
		case 55:	if mine(55) then collided = true: mineball55.visible = 0: end if
		case 56:	if mine(56) then collided = true: mineball56.visible = 0: end if
		case 57:	if mine(57) then collided = true: mineball57.visible = 0: end if
		case 58:	if mine(58) then collided = true: mineball58.visible = 0: end if
		case 59:	if mine(59) then collided = true: mineball59.visible = 0: end if

		case 61:	if mine(61) then collided = true: mineball61.visible = 0: end if
		case 62:	if mine(62) then collided = true: mineball62.visible = 0: end if
		case 63:	if mine(63) then collided = true: mineball63.visible = 0: end if
		case 64:	if mine(64) then collided = true: mineball64.visible = 0: end if
		case 65:	if mine(65) then collided = true: mineball65.visible = 0: end if
		case 66:	if mine(66) then collided = true: mineball66.visible = 0: end if
		case 67:	if mine(67) then collided = true: mineball67.visible = 0: end if
		case 68:	if mine(68) then collided = true: mineball68.visible = 0: end if
		case 69:	if mine(69) then collided = true: mineball69.visible = 0: end if

end select

End sub

Sub explosion_timer 

Dim Random, x, y, z

random = int(2*rnd)

if tb4missionon = 1 then
	x = hit1.X: y = hit1.y
	explode.X = x: 	explode.Y = y - 300:explode.z = 800
	explode1.X = x: explode1.Y = y: explode1.z = 250
	stopsound "tb4drama"
	stopsound "drama"
	if not explode1.visible and not explode.visible then
		PlaySound "explosion", 0, 100, 0, 0, 0
		if random = 0 then explode.visible = 1 else explode1.visible = 1
	end if

end if

if tb1missionon = 1 then
	x = hit2.X: y = hit2.y
	explode.X = x: 	explode.Y = y:explode.z = 0
	explode1.X = x: explode1.Y = y: explode1.z = -300
	if not explode1.visible and not explode.visible then
		PlaySound "bell", 0, 100, 0, 0, 0
		if random = 0 then explode.visible = 1 else explode1.visible = 1
	end if
end if

If explode1.visible then
	if expand = false then
		explode1.size_X = explode1.size_X - 1
		explode1.size_Y = explode1.size_Y - 1
		explode1.size_Z = explode1.size_Z - 1
		if explode1.size_X <= 0 then 
			explode1.size_X = 0 :explode1.size_Y = 0 :explode1.size_Z = 0 :expand = true :explode1.visible=0
			explosion.enabled = 0
			if tb4missionon = 1 then
				intro.enabled = 1
				screen = 0
				outro = true
				success=false
				PlaySound "failed", 0, 100, 0, 0, 0
				fish4timer.enabled = 1
			end if
		end if
	end if

	if expand = true then
		explode1.size_X = explode1.size_X + 1
		explode1.size_Y = explode1.size_Y + 1
		explode1.size_Z = explode1.size_Z + 1
		if explode1.size_X => 150 then :explode1.size_X = 150 :expand = false :end if
	end if

end if

If explode.visible then
	if expand = false then
		explode.size_X = explode.size_X - 1
		explode.size_Y = explode.size_Y - 1
		explode.size_Z = explode.size_Z - 1
		if explode.size_X <= 0 then 
			explode.size_X = 0 :explode.size_Y = 0 :explode.size_Z = 0: expand = true :explode.visible=0
			explosion.enabled = 0
			if tb4missionon = 1 then
				intro.enabled = 1
				screen = 0
				outro = true
				success=false
				PlaySound "failed", 0, 100, 0, 0, 0
				fish4timer.enabled = 1
			end if
		end if
	end if

	if expand = true then
		explode.size_X = explode.size_X + 1
		explode.size_Y = explode.size_Y + 1
		explode.size_Z = explode.size_Z + 1
		if explode.size_X => 200 then :explode.size_X = 200 :expand = false :end if
	end if

end if

end sub

sub fishtimer_timer()

Dim a, b, c, d, e
dim a1, b1, c1, d1, e1

a1 = int(2*(rnd)): b1 = int(2*(rnd)): c1 = int(2*(rnd)):d1 = int(2*(rnd)):e1 = int(2*(rnd))
if a1 = 0 then a = -int(2*rnd) else a = int(2*rnd)
if b1 = 0 then b = -int(2*rnd) else b = int(2*rnd)
if c1 = 0 then c = -int(2*rnd) else c = int(2*rnd)
if d1 = 0 then d = -int(2*rnd) else d = int(2*rnd)
if e1 = 0 then e = -int(2*rnd) else e = int(2*rnd)

	if fish1a.x > -20000 and fish1a.x < 20000 then
		if direction then 
			for each obj in Fish1: obj.x = obj.x - 4: next
		Else
			for each obj in Fish1: obj.x = obj.x + 4: next
		end if
		fish1a.transy = fish1a.transy + a
		fish1b.transy = fish1b.transy + b
		fish1c.transy = fish1c.transy + c
		fish1d.transy = fish1d.transy + d
		fish1e.transy = fish1e.transy + e
		fish1a.transz = fish1a.transz + b
		fish1b.transz = fish1b.transz + a
		fish1c.transz = fish1c.transz + e
		fish1d.transz = fish1d.transz + c
		fish1e.transz = fish1e.transz + d
	end If

	if (fish1a.x <= -20000 and fish1a.x > -40000) or (fish1a.x => 20000 and fish1a.x < 40000) then 
		if direction then 
			for each obj in Fish1: obj.x = obj.x - 40: next
		Else
			for each obj in Fish1: obj.x = obj.x + 40: next
		end if
		for each obj in Fish1: obj.visible=1: next
	end If

	if (fish1a.x <= -40000) or (fish1a.x => 40000)  then 
		if direction then 
			for each obj in Fish1: obj.x = obj.x - 0.1: next
		Else
			for each obj in Fish1: obj.x = obj.x + 0.1: next
		end if
		for each obj in Fish1: obj.visible=0: next
		fish1a.transy = 0
		fish1b.transy = 0
		fish1c.transy = 0
		fish1d.transy = 0
		fish1e.transy = 0
		fish1a.transz = 0
		fish1b.transz = 0
		fish1c.transz = 0
		fish1d.transz = 0
		fish1e.transz = 0
	end If

	if direction and fish1a.x < -41000 then
		direction = false
		for each obj in Fish1: obj.x = obj.x + 1000: obj.roty = 0: next
	end if

	if not direction and fish1a.x > 41000 then
		direction = true
		for each obj in Fish1: obj.x = obj.x - 1000: obj.roty = 180: next
	end if

end sub

sub fish2timer_timer()
	fishtimer.enabled = 0
	if direction then
		for each obj in Fish1: obj.x = obj.x - 1000: next
	else
		for each obj in Fish1: obj.x = obj.x + 1000: next
	end if
	if fish1a.x =< -40000 or fish1a.x => 40000 then
		for each obj in Fish1: obj.visible = 0: next
		direction = not direction
		fish2timer.enabled = 0
	end if
end sub

'******* Submarine ********

Sub SubmarineTimer_Timer()
	fish5timer.enabled = 1
	submarine.y = Submarine.y + 2*corridorinterval
	Fish3.y = Fish3.y + 2*corridorinterval
	Fish3Fins.y = Fish3Fins.y + 2*corridorinterval
	if Submarine.y => marker.y then 
		submarine2timer.enabled = 1
		submarinetimer.enabled = 0
	end if
end Sub


Sub Submarine2Timer_Timer()
	submarine.z = Submarine.z + 2*corridorinterval
	submarine.x = Submarine.x - 2*corridorinterval
	if submarine.roty<90 then submarine.roty = Submarine.roty + 1 end if
	if Submarine.z => 0 then 
		submarine2timer.enabled = 0
	end if
end Sub

Sub fish5timer_timer()

Dim rotz
rotz = int(fish3.rotz)

	if rotz = 270 then
		Fish3.x = Fish3.x + 100
		Fish3Fins.x = Fish3Fins.x + 100
	end if

	if rotz = 90 then
		Fish3.x = Fish3.x - 100
		Fish3Fins.x = Fish3Fins.x - 100
	end if

if Fish3.x=>42000 then fish3.rotz = rotz - 1 end If
if Fish3.x=<-42000 then fish3.rotz = rotz + 1 end If
if Fish3fins.x=>42000 then fish3fins.rotz = rotz - 1 end If
if Fish3fins.x=<-42000 then fish3fins.rotz = rotz + 1 end If

end sub

'****** fishes during the game  *****

sub fish3timer_timer()

Dim a, b, c, d, e, f
dim a1, b1, c1, d1, e1, f1

a1 = int(2*(rnd)): b1 = int(2*(rnd)): c1 = int(2*(rnd)):d1 = int(2*(rnd)):e1 = int(2*(rnd)):f1 = int(2*(rnd))
if a1 = 0 then a = -int(2*rnd) else a = int(2*rnd)
if b1 = 0 then b = -int(2*rnd) else b = int(2*rnd)
if c1 = 0 then c = -int(2*rnd) else c = int(2*rnd)
if d1 = 0 then d = -int(2*rnd) else d = int(2*rnd)
if e1 = 0 then e = -int(2*rnd) else e = int(2*rnd)
if f1 = 0 then f = -int(2*rnd) else f = int(2*rnd)


	if fish2a.y > -60000 and fish2a.y =< 20000 then
		for each obj in Fish2: obj.y = obj.y - 4: obj.visible=1: next
		fish2a.transy = fish2a.transy + a
		fish2b.transy = fish2b.transy + b
		fish2c.transy = fish2c.transy + c
		fish2d.transy = fish2d.transy + d
		fish2e.transy = fish2e.transy + e
		fish2f.transy = fish2f.transy + e
		fish2a.transz = fish2a.transz + b
		fish2b.transz = fish2b.transz + a
		fish2c.transz = fish2c.transz + e
		fish2d.transz = fish2d.transz + c
		fish2e.transz = fish2e.transz + d
		fish2f.transz = fish2f.transz + d
	end If

	if (fish2a.y <= -60000 and fish2a.y > -90000) then 
		for each obj in Fish2: obj.y = obj.y - 40: obj.visible=1: next

	end If

	if (fish2a.y <= -90000) then 

			for each obj in Fish2: obj.y = obj.y - 0.1: next

		for each obj in Fish2: obj.visible=0: next
		fish2a.transy = 0
		fish2b.transy = 0
		fish2c.transy = 0
		fish2d.transy = 0
		fish2e.transy = 0
		fish2f.transy = 0
		fish2a.transz = 0
		fish2b.transz = 0
		fish2c.transz = 0
		fish2d.transz = 0
		fish2e.transz = 0
		fish2f.transz = 0
	end If

	if side and fish2a.y < -91000 then
		side = false
		for each obj in Fish2: obj.y = obj.y + 100000:obj.x = obj.x + 30000: obj.visible=1: next
	end if

	if not side and fish2a.y < -91000 then
		side = true
		for each obj in Fish2: obj.y = obj.y + 100000:obj.x = obj.x - 30000: obj.visible=1: next
	end if

end sub

sub fish4timer_timer()
	fish3timer.enabled = 0

	for each obj in Fish2: obj.y = obj.y - 1000: next

	if side and fish2a.y < -91000 then
		side = false
		for each obj in Fish2: obj.y = obj.y + 100000:obj.x = obj.x + 30000: obj.visible=0: next
		fish4timer.enabled = 0
	end if

	if not side and fish2a.y < -91000 then
		side = true
		for each obj in Fish2: obj.y = obj.y + 100000:obj.x = obj.x - 30000: obj.visible=0: next
		fish4timer.enabled = 0
	end if

end sub

Sub tb1pinballconfig()

TB1body.x = 156
TB1body.y = 243
TB1body.z = 0
TB1body.size_x = 4
TB1body.size_y = 4
TB1body.size_z = 4
TB1body.rotx = 90
TB1body.roty = 0
TB1body.rotz = -90
TB1body.transx = 0
TB1body.transy = 0
TB1body.transz = 0

end sub

Sub tb1gameconfig()

TB1body.x = 500
TB1body.y = 500
TB1body.z = -500
TB1body.size_x = 100
TB1body.size_y = 100
TB1body.size_z = 100
TB1body.rotx = 90
TB1body.roty = 90
TB1body.rotz = 0
TB1body.transx = 0
TB1body.transy = 0
TB1body.transz = 0

end sub

Sub tb3pinballconfig()

TB3.x = 157
TB3.y = 257
TB3.z = -180
TB3.size_x = 2
TB3.size_y = 2
TB3.size_z = 2
TB3.rotx = 90
TB3.roty = 0
TB3.rotz = 0
TB3.transx = 0
TB3.transy = 0
TB3.transz = 0

end sub

Sub tb3gameconfig()

TB3.x = -8151
TB3.y = -6407
TB3.z = 0
TB3.size_x = 50
TB3.size_y = 50
TB3.size_z = 50
TB3.rotx = 90
TB3.roty = 0
TB3.rotz = -90
TB3.transx = 0
TB3.transy = -5000
TB3.transz = 0

end sub