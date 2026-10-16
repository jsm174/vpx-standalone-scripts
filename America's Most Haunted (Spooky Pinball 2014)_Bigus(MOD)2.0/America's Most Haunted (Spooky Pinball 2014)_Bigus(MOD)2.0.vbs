'Notepad++ search term: '(?!;)(.*)(video\()
'unsigned char - ""
'!= - <>
'(?!\t)(.*)\-=(.*) - \1 = \1 - \2
'(?!\t)(.*)\+=(.*) - \1 = \1 + \2
'(?!(\t|s|u|b| ))(.*)\((.*, .*)\) - \2 \3
'void - Sub
'\nint - Function
'(\n)End If - (\n)End Sub
'ghostmove\((.*)\) - ghostMove \1
'light\((.*)\) - light \1
'video\('(\w)', '(\w)', '(\w)' (.*)\) - video "\1", "\2", "\3" \4
'\treturn - \tExit Sub   But watch out for something after Exit Sub as it indicates a function return
Option Explicit
Randomize
If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
'Const BallSize = 48
ExecuteGlobal GetTextFile("core.VBS")
'ExecuteGlobal GetTextFile("variables.vb")
'ExecuteGlobal GetTextFile("Lights.vb")
Pi = Round(4 * Atn(1), 6)

Dim Controller
Set Controller = CreateObject("B2S.Server")
Controller.Run

'Various Variables
Dim TextStr2, B2SOn, DOFs

Const LMEMTableConfig="LMEMDOFTables.txt"

'B2S/DOF version
sub SaveLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim temp1
	dim tempb2s
	tempb2s=0
	if B2SOn=true then
		if DOFs = true then
			tempb2s=2
		else
			tempb2s=1
		end if
	else
		tempb2s=0
	end if
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig=FileObj.CreateTextFile(UserDirectory & LMEMTableConfig,True)
	LMConfig.WriteLine tempb2s
	LMConfig.Close
	Set LMConfig=Nothing
	Set FileObj=Nothing

end Sub

'B2S/DOF version
sub LoadLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim tempC
	dim tempb2s
    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMTableConfig) then
		Exit Sub
	End if
	Set LMConfig=FileObj.GetFile(UserDirectory & LMEMTableConfig)
	Set TextStr2=LMConfig.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	TextStr2.Close
	tempb2s=cdbl(tempC)
	if tempb2s=0 then
		B2SOn=false
		DOFs = false
	elseif tempb2s= 1 then
		B2SOn=true
		DOFs = false
	elseif tempb2s= 2 then
		B2SOn=true
		DOFs = true
	end if
	Set LMConfig=Nothing
	Set FileObj=Nothing
end sub

'B2S/DOF version
Function SoundFX (sound)
    If DOFs = true Then
        SoundFX = ""
    Else
        SoundFX = sound
    End If
End Function

'B2S/DOF version
Sub DOF(dofevent, dofstate)	
	If B2SOn = True Then		
		If dofstate = 2 Then
			Controller.B2SSetData dofevent, 1
			Controller.B2SSetData dofevent, 0
		Else
			Controller.B2SSetData dofevent, dofstate
		End If
	End If
End Sub

'B2S/DOF version
Sub resetAllRGBDOF()
	Dim x
	For x = 150 to 170
		DOF x, 0
	Next
End Sub

'B2S/DOF version
Sub Table1_exit()
	SaveLMEMConfig
	if B2SOn Then Controller.Stop
End Sub

Sub SetBackGlass(side, color)
	On Error Resume Next
	If B2SOn = True Then
		Select Case Side
		Case 1:
			If color = "white" Then Controller.B2SSetData 4, 1
			If color = "red" Then Controller.B2SSetData 1, 1
			If color = "green" Then Controller.B2SSetData 2, 1
			If color = "blue" Then Controller.B2SSetData 3, 1
			If color = "purple" Then Controller.B2SSetData 5, 1
		Case 2:
			If color = "white" Then Controller.B2SSetData 9, 1
			If color = "red" Then Controller.B2SSetData 6, 1
			If color = "green" Then Controller.B2SSetData 7, 1
			If color = "blue" Then Controller.B2SSetData 8, 1
			If color = "purple" Then Controller.B2SSetData 10, 1
		End Select
	End If
End Sub

Sub BackGlassOff(side)
	On Error Resume Next
	If B2SOn = True Then
		If side = 1 OR side = 2 Then
			Controller.B2SSetData 1, 0
			Controller.B2SSetData 2, 0
			Controller.B2SSetData 3, 0
			Controller.B2SSetData 4, 0
			Controller.B2SSetData 5, 0
		End If
		If side = 2 OR side = 3 Then
			Controller.B2SSetData 6, 0
			Controller.B2SSetData 7, 0
			Controller.B2SSetData 8, 0
			Controller.B2SSetData 9, 0
			Controller.B2SSetData 10, 0
		End If
	End If
End  Sub

Set mMagnaSave = New cvpmMagnet : With mMagnaSave
	.InitMagnet TrSw24m, 17
	.size = 160
	.GrabCenter = False
End With

Set AutoPlunger = New cvpmImpulseP
With AutoPlunger
	.InitImpulseP TrAutoPlunge, 50, 1
	.CreateEvents "AutoPlunger"
End With

Dim AllowTest
AllowTest = 0								'EP- Set this to 1 to allow test buttons
Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.PullBack
		'AutoPlunger.PullBack
	End If
	If keycode = LeftFlipperKey Then
		LFlip 1
	End If
	If keycode = RightFlipperKey Then
		RFlip 1
	End If
	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If
	If keycode = 9 Then							'EP- Menu button, i.e. button #8
		If CoinDoorState = 1 Then
			Select Case MenuNumber
			Case 1:
				MenuNumber = 4
				MenuItem = "Options 3"
			Case 2:
				MenuNumber = 1
				MenuItem = "Solenoids"
			Case 3:
				MenuNumber = 2
				MenuItem = "Options 1"
			Case 4:
				MenuNumber = 3
				MenuItem = "Options 2"
			End Select
			DMDScene "", MenuItem, 15, "", -1, 14, 16665, 14, 100
			playSFX 2, "Y", "Z", "Z", 255
		End If
		If run=3 AND CoinDoorState = 0 Then
			ShowGameStatus()
		End If
	End If
	If keycode = 10 Then							'EP- Menu button, i.e. button #9
		If CoinDoorState = 1 Then
			Select Case MenuNumber
			Case 1:
				MenuNumber = 2
				MenuItem = "Options 1"
			Case 2:
				MenuNumber = 3
				MenuItem = "Options 2"
			Case 3:
				MenuNumber = 4
				MenuItem = "Options 3"
			Case 4:
				MenuNumber = 1
				MenuItem = "Solenoids"
			End Select
			DMDScene "", MenuItem, 15, "", -1, 14, 16665, 14, 100
			playSFX 2, "Y", "Z", "Z", 255
		End If
		If run=3 AND CoinDoorState = 0 Then
			ShowGameStatus()
		End If
	End If
	If keycode = 11 Then 							'Did user press the switch, or press it during a game and aborted the game? EP- Enter button, i.e. button #0
		If CoinDoorstate = 1 Then
			If run=0 Then
				'video('A', 'Z', 'Z', 0, 0, 255)	'Play a fake video
				menuAbortFlag = 0					'Clear flag
				ShowOptions(MenuNumber)
			End If
			If run=3 Then
				if (menuAbortFlag = 0) Then				'Did user press the Enter switch during a game?
					'extraBalls = 1						'TESTING!
					'video('A', 'Z', 'Z', 0, 0, 255)	'Play a fake video
					menuAbortFlag = 1					'Set flag
					stopMusic()
					ball = ballsPerGame					'This will end the game
					ShowOptions(MenuNumber)
				End If
			End If
		Else
			'EP- Tell the user to open the door first
		End If
	End If
	If keycode = 207 Then							'EP- Coin Door, i.e. End key
		CoinDoorState = CoinDoorState * -1
		CoinDoorOpenClose()
	End If
'	keycode for the End key to simulate opening the CoinDoor()
	If keycode = AddCreditKey Then CabCoin()
	If keycode = StartGameKey Then
		If (cursorPos <> 50) Then
			modeTimer = Int(375000/cycleAdjuster)								'EP- Have to put this in LFlip routine
			if (inChar = 91 and cursorPos > 0) Then			'Backspace?
				cursorPos = cursorPos - 1					'Send cursor back
				initials(cursorPos) = 32					'Set that initial back to an empty SPACE
				playSFX 1, "O", "R", "Z", 255
			End If
			if (inChar <> 91) Then							'Set a character, as long as it's not a backspace				
				initials(cursorPos) = inChar				'Set the character
				If inChar = 92 Then initials(cursorPos) = 32
				cursorPos = cursorPos + 1
				if (cursorPos = 3) Then						'Done?
					playSFX 1, "O", "R", "Y", 255
					cursorPos = 99							'Set flag to exit			
				else 
					playSFX 1, "O", "R", "Y", 255
				End If
			End If
			if (cursorPos <> 3) Then						'Don't bother changing this on last press				
'				video('Z', whichPlayer + 48, cursorPos + 48, loopVideo, 0, 0)
				sendInitials inChar, HSPlace
				NameEntry HSCheck, HSPlace
			End If
		Else
			CabStart(run)
		End If
	End If
'************
'*Test kicker stuff
'************
	If AllowTest = 1 Then
		If KeyCode = 208 Then ModeStart()
		If keycode = 200 Then ServeBall()
		If Keycode = 30 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -45, 20, 0
		If Keycode = 31 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -35, 60, 0
		If Keycode = 32 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -25, 30, 0
		If Keycode = 33 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -18, 30, 0
		If Keycode = 34 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -9, 25, 0
		If Keycode = 35 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -4, 25, 0
		If Keycode = 36 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -0, 25, 0
		If Keycode = 37 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 5, 30, 0
		If Keycode = 38 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 9, 25, 0
		If Keycode = 39 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 15, 50, 0
		If Keycode = 40 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 20, 20, 0
		If Keycode = 47 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 28, 60, 0
		If Keycode = 48 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 45, 30, 0
		If Keycode = 20 Then
		If NOT (BallMoverHold is Nothing) Then
			BallMoverHold.X = KiHold.X
			BallMoverHold.Y = KiHold.Y
			TrSw57_UnHit()
		End If
		End If
		If Keycode = 21 Then BallMoverHold.X = TrSw57.X:BallMoverHold.Y = TrSw57.Y:KiHold.Kick 0, 0, 0':TrSw57_Hit()
		If Keycode = 19 Then KiTest1.CreateBall:KiTest1.Kick 0, 0, 0
	End If
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		'AutoPlunger.Fire
	End If
	If keycode = LeftFlipperKey Then
		LFlip 0
	End If
	If keycode = RightFlipperKey Then
		RFlip 0
	End If
End Sub

'********************
'* UltraDMD stuff
'********************

Dim UltraDMD

Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3
Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
Const UltraDMD_Animation_ZoomIn = 2
Const UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4
Const UltraDMD_Animation_ScrollOffRight = 5
Const UltraDMD_Animation_ScrollOnLeft = 6
Const UltraDMD_Animation_ScrollOnRight = 7
Const UltraDMD_Animation_ScrollOffUp = 8
Const UltraDMD_Animation_ScrollOffDown = 9
Const UltraDMD_Animation_ScrollOnUp = 10
Const UltraDMD_Animation_ScrollOnDown = 11
Const UltraDMD_Animation_None = 14

Sub LoadUltraDMD
	On Error Resume Next
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
	If Not UltraDMD is Nothing Then
		UltraDMD.Init
	End If
    If UltraDMD is Nothing Then
		MsgBox "No UltraDMD found.  This table MAY run without it (but why would you want to??)."
		Set UltraDMD = Nothing
        Exit Sub
    End If
    UltraDMD.Init
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If
    If UltraDMD.GetMinorVersion < 0 Then
        MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.0 or newer."
        Exit Sub
    End If
    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir
    curDir = fso.GetAbsolutePathName(".")
    Set fso = nothing
	If Not UltraDMD is Nothing Then
		UltraDMD.SetProjectFolder curDir & "\America's Most Haunted.UltraDMD"
		UltraDMD.SetVideoStretchMode UltraDMD_VideoMode_Middle
	End If
End Sub

Sub DMDScene (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout, prio)		'regular DMD call with priority
	If prio >= OldDMDPrio Then
		DMDSceneInt background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout
		OldDMDPrio = prio
	End If
End Sub

Sub DMDSceneInt (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout)		'This gets called if the priority is greater than or equal to the current scene in order to interrupt it
	Dim X
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
		UltraDMD.DisplayScene00 background, toptext, topbright, bottomtext, bottombright, animatein, pause*PauseAdjuster, animateout
		If pause > 0 OR animateIn < 14 OR animateOut < 14 Then
			TiDMDScore.Enabled = False
			TiDMDScore.Enabled = True
        End If
	End If
End Sub

Sub DMDSceneQ (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout)			'EP- Called to queue up a video
	Dim X
	If Not UltraDMD is Nothing Then
		UltraDMD.DisplayScene00 background, toptext, topbright, bottomtext, bottombright, animatein, pause*PauseAdjuster, animateout
		If pause > 0 OR animateIn < 14 OR animateOut < 14 Then
			TiDMDScore.Enabled = False
			TiDMDScore.Enabled = True
        End If
	End If
End Sub

Sub DMDSceneEOB (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout, prio)		'Special DMD call at the EoB
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
		UltraDMD.DisplayScene00 "EBA.gif", "", 0, "", 0, 14, 1, 14
		UltraDMD.DisplayScene00ExWithID "EoB", False, background, toptext, topbright, -1, bottomtext, bottombright, -1, animatein, pause*PauseAdjuster, animateout
		TiEoB.Enabled = 1
	End If
End Sub

Sub TiEoB_timer()
	If Not UltraDMD is Nothing Then
	Select Case EoBStep
		Case 1
			UltraDMD.ModifyScene00 "EoB", " ", EVP_Total(player)
			EoBStep = 2
		Case 2
			UltraDMD.ModifyScene00 "EoB", " ", photosTaken(player)
			EoBStep = 3
		Case 3
			UltraDMD.ModifyScene00 "EoB", " ", GhostsDefeated(player)
			EoBStep = 4
		Case 4
			UltraDMD.ModifyScene00 "EoB", " ", bonus
			EoBStep = 5
		Case 5
			EoBStep=6
		Case 6
			UltraDMD.ModifyScene00 "EoB", "", ""
			EoBStep = 1
			me.enabled = false
	End Select
	End If
End Sub

Sub TiDMDScore_Timer()
	If Not UltraDMD is Nothing Then
	On Error Resume Next
	If Not UltraDMD.IsRendering Then
        'When the scene finishes rendering, then immediately display the scoreboard
        me.Enabled = False
		DMDScore()
    End If
	On Error GoTo 0
	End If
End Sub

Sub DMDScore()
	If Not UltraDMD is Nothing Then
	On Error Resume Next
	OldDMDPrio = 0
	UltraDMD.DisplayScoreboard numPlayers, Player, PlayerScore(1), PlayerScore(2), PlayerScore(3), PlayerScore(4), "Ball " & Ball, "Credits " & credits
	On Error GoTo 0
	End If
End sub

'***** End UltraDMD ************
'B2S/DOF version
LoadLMEMConfig
'B2S/DOF version
if B2SOn then
	Set Controller = CreateObject("B2S.Server")
	Controller.B2SName = "americamosthaunted"
	Controller.Run()
	Controller.B2SSetData 35,4
	If Err Then MsgBox "Can't Load B2S.Server."
End If
LoadUltraDMD()

Function LoadValueLng(section, key)
	Dim strValue
	strValue = LoadValue(section, key)
	If IsNumeric(strValue) Then
		LoadValueLng = CLng(strValue)
	Else
		LoadValueLng = 0
	End If
end Function

Sub Table1_Init()
	Dim X, Y
	For X = 1 to 4
		MainTrough(X).CreateBall
		MainTrough(X).Kick 58, 10, 0
	Next
	For Each Y in MainTrough						'Set each kicker to be occupied
		Y.UserValue = 1
	Next
	Set BallMoverHold = Nothing
	WaLeftSide1.TimerInterval = 1
	Wall10.TimerInterval = 1
	Wall23.TimerInterval = 3000
	TiZero.Enabled = 0
	TiDMDScore.Enabled = 0
	Wall26.TimerInterval = 150
'******** Table Option Stuff***********
	If Not IsNumeric(LoadValueLng("AMH", "Solenoids")) Then
		SaveValue "AMH", "Solenoids", AMHOptionsSol
		SaveValue "AMH", "Options 1", AMHOptions1
		SaveValue "AMH", "Options 2", AMHOptions2
		SaveValue "AMH", "Options 3", AMHOptions3
	Else
		DoOptions()
	End If
	If Not IsNumeric(LoadValueLng("AMH", "HSPoints0")) Then
		SetDefaultScores()
	Else
		LoadHighScores()
	End If
'**************************************
	NoGame1											'EP- What happens when the machine turns on and gets before the "while run==0" loop
End Sub

'************************
'* My first attempt at an options menu
'************************
Sub DoOptions()
	Dim FlipPower, SlingPower, PopPower, LeftVUKPower, RightVUKPower, LaunchPower, X
	'Solenoids
	FlipPower = (LoadValueLng("AMH", "Solenoids") AND option1) * 515
'**	LeftFlipper.PowerLaw = FlipPower													'EP- Set the right and left flippers to 0.4 of whatever the player chose
	LeftFlipper.strength = FlipPower
'**	RightFlipper.PowerLaw = FlipPower
	RightFlipper.strength = FlipPower
	PopPower = ((LoadValueLng("AMH", "Solenoids") AND Option3)/256) * 0.5
	For Each X in Bumpers
		X.Force = PopPower																'EP- Set all the Pop Bumpers to 1.6 the value of whatever the player chose
	Next
	LeftVUKPower = ((LoadValueLng("AMH", "Solenoids") AND Option4)/4096) * 5
	VUKPower = LeftVUKPower																'EP- Set the door VUK power to be 5 times whatever the player chose
	RightVUKPower = ((LoadValueLng("AMH", "Solenoids") AND Option5)/65536) * 6
	ScoopPower = RightVUKPower															'EP- Set the scoop to be 3 times whatever the player chose
	LaunchPower = ((LoadValueLng("AMH", "Solenoids") AND Option6)/1048576) * 10
	PlungerStrength = LaunchPower
	AutoPlunger.Strength = PlungerStrength												'EP- Set the AutoPlunger strength to be 10 times what the player chose
	'Options 1
	FreePlay = (LoadValueLng("AMH", "Options 1") AND Option1)								'EP- Free Play N/Y 0, 1
	CoinsPerCredit = ((LoadValueLng("AMH", "Options 1") AND Option2)/16)					'EP- Coins per credit 1-9
	BallsPerGame = ((LoadValueLng("AMH", "Options 1") AND Option3)/256) + 1				'EP- Balls per game 1-9
	AllowExtraBalls = ((LoadValueLng("AMH", "Options 1") AND Option8)/268435456)			'EP- Are extra balls allowed; Y/N/100k/500k/1 million, 1-5
	'Options 2
	SpotProgress = (LoadValueLng("AMH", "Options 2") AND Option1)							'EP- How many pops are spotted at the start of the game (to make it easier to light the mode) 0, 3, 6, 9, 12
	AllowReplay = ((LoadValueLng("AMH", "Options 2") AND Option2)/16)						'EP- Are replays allowed N/Y 0, 1
	AllowMatch = ((LoadValueLng("AMH", "Options 2") AND Option3)/256)						'EP- Are matches allowed N/Y 0, 1
	Tournament = ((LoadValueLng("AMH", "Options 2") AND Option4)/4096)						'EP- Is tournament mode on N/Y 0, 1
	EVP_EBSetting = ((LoadValueLng("AMH", "Options 2") AND Option5)/65536) * 5				'EP- How many EVPs needed to collect extra ball
	ComboTimerStart = ((LoadValueLng("AMH", "Options 2") AND Option6)/1048576) * 2400		'EP- How long a player has to make a combo shot
	ZeroPointBall = ((LoadValueLng("AMH", "Options 2") AND Option8)/268435456)				'EP- If a ball scores nothing, save it Y/N
	'Options 3
	ScoopSaveStart = (LoadValueLng("AMH", "Options 3") AND Option1) * 15					'EP- How long the Ball Saver is on after shooting out of the scoop
End Sub

Const Option1 = 15				'bits 1-4, 2^0
Const Option2 = 240				'bits 5-8, 2^4
Const Option3 = 3840			'bits 9-12, 2^8
const Option4 = 61440			'bits 13-16, 2^12
Const Option5 = 983040			'bits 17-20, 2^16
Const Option6 = 15728640		'bits 21-24, 2^20
Const Option7 = 251658240		'bits 25-28, 2^24
Const Option8 = 1879048192		'bits 29-32, 2^28

Dim AMHOptionsSol, AMHOptions1, AMHOptions2, AMHOptions3, AMHMenuSol, AMHMenuOptions1, AMHMenuOptions2, AMHMenuOptions3, MenuItem, MenuNumber, DefaultSol, DefaultOptions1, DefaultOptions2, DefaultOptions3
DefaultSol = 5592325
DefaultOptions1 = 33555217
DefaultOptions2 = 36831504
DefaultOptions3 = 2
MenuNumber = 1
MenuItem = "Solenoids"
AMHOptionsSol = LoadValue("AMH", "Solenoids")
If AMHOptionsSol = "" Then AMHOPtionsSol = DefaultSol			'EP- if nothing is loaded, load the default
AMHOptions1 = LoadValue("AMH", "Options 1")
If AMHOptions1 = "" Then AMHOPtions1 = DefaultOptions1
AMHOptions2 = LoadValue("AMH", "Options 2")
If AMHOptions2 = "" Then AMHOPtions2 = DefaultOptions2
AMHOptions3 = LoadValue("AMH", "Options 3")
If AMHOptions3 = "" Then AMHOPtions3 = DefaultOptions3
Sub ShowOptions(whichMenu)
	Select Case whichMenu
	Case 1:
		If Not IsObject(AMHMenuSol) Then
			Set AMHMenuSol = New cvpmDips
			With AMHMenuSol
				.AddForm 530, 200, "America's Most Haunted Solenoid Options"
				.AddFrameExtra 0, 10, 40, "Flippers", Option1, Array("1**", 1, "2", 2, "3", 3, "4", 4, "5*", 5, "6", 6, "7", 7, "8", 8, "9", 9)
'				.AddFrameExtra 50, 10, 35, "Slings", Option2, Array("1", 1*16, "2", 2*16, "3", 3*16, "4", 4*16, "5*", 5*16, "6", 6*16, "7", 7*16, "8", 8*16, "9", 9*16)
				.AddFrameExtra 50, 10, 35, "Slings", Option2, Array("Not", 0, "Yet", 0, "If", 0, "ever", 0)
				.AddFrameExtra 95, 10, 30, "Pops", Option3, Array("1", 1*256, "2", 2*256, "3", 3*256, "4", 4*256, "5*", 5*256, "6", 6*256, "7", 7*256, "8", 8*256, "9", 9*256)
				.AddFrameExtra 135, 10, 50, "Left VUK", Option4, Array("1**", 1*4096, "2**", 2*4096, "3**", 3*4096, "4**", 4*4096, "5*", 5*4096, "6", 6*4096, "7", 7*4096, "8", 8*4096, "9", 9*4096)
				.AddframeExtra 195, 10, 70, "Right Scoop", Option5, Array("1", 1*65536, "2", 2*65536, "3", 3*65536, "4", 4*65536, "5*", 5*65536, "6", 6*65536, "7", 7*65536, "8", 8*65536, "9", 9*65536)
				.AddFrameExtra 275, 10, 80, "Auto Launcher", Option6, Array("1**", 1*1048576, "2**", 2*1048576, "3", 3*1048576, "4", 4*1048576, "5*", 5*1048576, "6", 6*1048576, "7", 7*1048576, "8", 8*1048576, "9", 9*1048576)
				.AddLabel 0,225,100,15,"* Default Value"
				.AddLabel 0,240,305,15,"** Not suggested as it may make the game un-playable"
			End With
		End If
		AMHOptionsSol = AMHMenuSol.ViewDipsExtra(AMHOptionsSol)
		SaveValue "AMH", "Solenoids", AMHOptionsSol
	Case 2:
		If Not IsObject(AMHMenuOptions1) Then
			Set AMHMenuOptions1 = New cvpmDips
			With AMHMenuOptions1
				.AddForm 530, 200, "America's Most Haunted Options 1"
				.AddFrameExtra 0, 10, 90, "Free Play", Option1, Array("No", 0, "Yes*", 1)
				.AddFrameExtra 100, 10, 90, "Coins Per Credit", Option2, Array("1*", 1*16, "2", 2*16, "3", 3*16, "4", 4*16, "5", 5*16, "6", 6*16, "7", 7*16, "8", 8*16)
				.AddFrameExtra 200, 10, 90, "Balls Per Game", Option3, Array("1", 1*256, "2", 2*256, "3*", 3*256, "4", 4*256, "5", 5*256, "6", 6*256, "7", 7*256, "8", 8*256, "9", 9*256)
				.AddFrameExtra 300, 10, 90, "Tilt Warning", Option4, Array("Not Yet", 0)
				.AddframeExtra 400, 10, 90, "Video Mode Speed", Option5, Array("We can dream", 0, "of a day", 0, "when I put in", 0, "A video mode", 0)
				.AddFrameExtra 500, 10, 90, "SFX Volume", Option6, Array("Not Yet", 0)
				.AddFrameExtra 600, 10, 90, "Music Volume", Option7, Array("Not Yet", 0)
				.AddFrameExtra 700, 10, 90, "Extra Balls", Option8, Array("No", 0, "Yes*", 1*268435456, "100 K", 2*268435456, "500 K", 3*268435456, "1 Million", 4*268435456)
			End With
		End If
		AMHOptions1 = AMHMenuOptions1.ViewDipsExtra(AMHOptions1)
		SaveValue "AMH", "Options 1", AMHOptions1
	Case 3:
		If Not IsObject(AMHMenuOptions2) Then
			Set AMHMenuOptions2 = New cvpmDips
			With AMHMenuOptions2
				.AddForm 530, 200, "America's Most Haunted Options 2"
				.AddFrameExtra 0, 10, 90,   "Spot Pops", Option1, Array("0*", 0, "3", 3, "6", 6, "9", 9, "12", 12)
				.AddFrameExtra 100, 10, 90,  "Replay?", Option2, Array("No", 0, "Yes*", 1*16)
				.AddFrameExtra 200, 10, 90,  "Match?", Option3, Array("No", 0, "Yes*", 1*256)
				.AddFrameExtra 300, 10, 90, "Tournament?", Option4, Array("No*", 0, "Yes", 1*4096)
				.AddframeExtra 400, 10, 90, "Extra Ball @", Option5, Array("5", 1*65536, "10*", 2*65536, "15", 3*65536, "20", 4*65536)
				.AddFrameExtra 500, 10, 90, "Combo Timer", Option6, Array("2", 1*1048576, "4", 2*1048576, "6*", 3*1048576, "8", 4*1048576, "10", 5*1048576)
				.AddFrameExtra 600, 10, 90, "Video Mode", Option7, Array("I dream of", 0, "a day when", 0, "I put in a", 0, "Video mode", 0)
				.AddFrameExtra 700, 10, 90, "Zero Point Ball", Option8, Array("No", 0, "Yes*", 1*268435456)
			End With
		End If
		AMHOptions2 = AMHMenuOptions2.ViewDipsExtra(AMHOptions2)
		SaveValue "AMH", "Options 2", AMHOptions2
	Case 4:
		If Not IsObject(AMHMenuOptions3) Then
			Set AMHMenuOptions3 = New cvpmDips
			With AMHMenuOptions3
				.AddForm 530, 200, "America's Most Haunted Solenoid Options"
				.AddFrameExtra 0, 10, 50, "Scoop Time", Option1, Array("1", 1, "2*", 2, "3", 3, "4", 4, "5", 5)
			End With
		End If
		AMHOptions3 = AMHMenuOptions3.ViewDipsExtra(AMHOptions3)
		SaveValue "AMH", "Options 3", AMHOptions3
	End Select
	DoOptions()
End Sub

Sub SetDefaultScores()
'	setHighScore 0, 20000000, "B", "U", "G"
	setHighScore 0, 2000, "B", "U", "G"
'	setHighScore 1, 17000000, "C", "O", "W"
	setHighScore 1, 1700, "C", "O", "W"
'	setHighScore 2, 15000000, "B", "F", "K"
	setHighScore 2, 1500, "B", "F", "K"
'	setHighScore 3, 12000000, "P", "E", "D"
	setHighScore 3, 1200, "P", "E", "D"
'	setHighScore 4, 10000000, "B", "J", "H"
	setHighScore 4, 1000, "B", "J", "H"
End Sub

Sub SetHighScore(whichPosition, theScore, char0, char1, char2)			'Puts high score on EEPROM
	If whichPosition < 5 Then
		SaveValue "AMH", "HSPoints" & whichPosition, theScore
		SaveValue "AMH", "HSName" & whichPosition, char0 & char1 & char2
	End If
End Sub

Sub GetHighScore(whichPosition)
	HighScores(whichPosition) = LoadValue("AMH", "HSPoints" & whichPosition)
	TopPlayers(whichPosition * 3) = Left(LoadValue("AMH", "HSName" & whichPosition), 1)
	TopPlayers((whichPosition * 3) + 1) = Mid(LoadValue("AMH", "HSName" & whichPosition), 2, 1)
	TopPlayers((whichPosition * 3) + 2) = Right(LoadValue("AMH", "HSName" & whichPosition), 1)
End Sub

Sub LoadHighScores()
	'EP- Don't need to check if scores are present as that was already checked prior to calling this sub
	Dim x
	For x = 0 to 4
		getHighScore(x)
	Next
End Sub

'************************

'***********************************************************
'* Ben Heck Code
'***********************************************************

Sub NoGame1()
	MachineReset() 							'Comment this out when testing board OFF the machine
	multiTimer = 0								'Use to run backglass lights
	multiCount = 0
	leftRGB(0) = 175
	leftRGB(1) = 175
	leftRGB(2) = 175
	rightRGB(0) = 175
	rightRGB(1) = 175
	rightRGB(2) = 175
	run = 0
	LoadHighScores()
	modeTimer = 0								'Use for Attract Mode
	houseKeeping()								'Check a few times, so we don't get a false positive on the door open or close state
'	if (bitRead(cabinet, Door) = 0) Then			'Is door open?	We need to check even if DOOR WARN is disabled	
'		coinDoorState = 1						'Set state to OPEN
'	Else
'		coinDoorState = 0						'Set state to CLOSED	
'	End If	
	'Serial.println(coinDoorDetect)
'	switchDebounceClear(16, 63)					'Reset debounces manually just in case
	drainTries = 0								'We haven't tried to kick the drain yet
	animatePF 0, 30, 1							'Original attract lights, set to repeat
	modeTimer = 0
	multiTimer = 0
	TiZero.Enabled = 1
Wall26.TimerEnabled = 1
End  Sub

Dim DMDAttract:DMDAttract = 1
Dim CurAttract:CurAttract = 1
Sub Wall26_Timer()
	Dim AttractSpacer, Z, Y
	'Debug.Print "timer running"
	If DMDAttract = 0 Then DMDAttract = 1
	If Not UltraDMD.IsRendering then
		Debug.Print "Not rendering"
		Select Case DMDAttract
		Case 1:
			CurAttract = 1
			UltraDMD.DisplayScene00 "AT0_Rev_2.gif", "", 0, "", 0, 14, 185 * pauseadjuster, 14
			DMDAttract = 2
		Case 2:
			CurAttract = 2
			If freePlay = 0 Then
				If Credits < 1 Then
					UltraDMD.DisplayScene00 "AT2.gif", "", 0, "", 0, 14, 180 * pauseadjuster, 14
				End If
			Else
				Dim X
				X = Random(6)*2
				UltraDMD.DisplayScene00 "", "FREE PLAY!!", 15, "", -1, 14, 90 * pauseadjuster, 14
			End If
			DMDAttract = 3
		Case 3:
			CurAttract = 3
			If FreePlay = 1 OR Credits > 0 Then
				UltraDMD.DisplayScene00 "AT1.gif", "", 0, "", 0, 14, 180 * pauseadjuster, 14
			End If
			DMDAttract = 1
		Case 4:
			CurAttract = 4
			z = 13 - Len(HighScores(0))
			For Y = 0 to z
				AttractSpacer = AttractSpacer & " "
			Next
			UltraDMD. DisplayScene00 "", "Show Producer: (1st)", 10, TopPlayers(0) & TopPlayers(1) & TopPlayers(2) & AttractSpacer & HighScores(0), 15, 14, 200 * pauseadjuster, 14
			DMDAttract = 1
		Case 5:
			CurAttract = 5
			z = 13 - Len(HighScores(1))
			For Y = 0 to z
				AttractSpacer = AttractSpacer & " "
			Next
			UltraDMD. DisplayScene00 "", "Team Leader:   (2nd)", 10, TopPlayers(3) & TopPlayers(4) & TopPlayers(5) & AttractSpacer & HighScores(1), 15, 14, 200 * pauseadjuster, 14
			DMDAttract = 1
		Case 6:
			CurAttract = 6
			z = 13 - Len(HighScores(2))
			For Y = 0 to z
				AttractSpacer = AttractSpacer & " "
			Next
			UltraDMD. DisplayScene00 "", "Psychic:       (3rd)", 10, TopPlayers(6) & TopPlayers(7) & TopPlayers(8) & AttractSpacer & HighScores(2), 15, 14, 200 * pauseadjuster, 14
			DMDAttract = 1
		Case 7:
			CurAttract = 7
			z = 13 - Len(HighScores(3))
			For Y = 0 to z
				AttractSpacer = AttractSpacer & " "
			Next
			UltraDMD. DisplayScene00 "", "Research:      (4th)", 10, TopPlayers(9) & TopPlayers(10) & TopPlayers(11) & AttractSpacer & HighScores(3), 15, 14, 200 * pauseadjuster, 14
			DMDAttract = 1
		Case 8:
			CurAttract = 8
			z = 13 - Len(HighScores(4))
			For Y = 0 to z
				AttractSpacer = AttractSpacer & " "
			Next
			UltraDMD. DisplayScene00 "", "Tech:          (5th)", 10, TopPlayers(12) & TopPlayers(13) & TopPlayers(14) & AttractSpacer & HighScores(4), 15, 14, 200 * pauseadjuster, 14
			DMDAttract = 1
		End Select
	End If
End Sub

Sub TiZero_Timer()
	If (run = 0) Then  								'While waiting for a start condition. Attract mode
		houseKeeping()
		if (countBalls() < 4) Then					'Not enough balls in trough?
			ballSearch()
		End If
		if (attractLights) Then
			AttractMode()
			'stressTest()
		End If
		if (SwLFlip = 1) Then
			if (tournament) Then					'If Tourney, both flippers do same thing
				Update(holdTourneyScores)			'Jump to last game's scores and stay there
			Else									'Normal operation
				Update(highScoreTable)				'Jump to High Score Table			
			End If
		End If
		if (SwRFlip = 1) Then
			if (tournament) Then					'If Tourney, both flippers do same thing
				Update(holdTourneyScores)			'Jump to last game's scores and stay there
			Else									'Normal operation
				if (showScores) Then
					Update(lastGameScores)			'Jump to last game's scores	
				Else								'If there wasn't a last game, jump to High Scores
					Update(highScoreTable)			'Jump to last game's scores and stay there							
				End If						
			End If
		End If
	Else
		Wall26.TimerEnabled = 0
		DMDAttract = 0
		StartGame()
		GameTimer.Enabled = 1
		Me.Enabled = 0
	End If
End Sub

Sub NoGame2()
'	leftDebounce = 0
'	LFlipTime = -10								'Make flipper re-triggerable, with debounce
'	LholdTime = 0								'Disable hold timer.
'	digitalWrite(LFlipHigh, 0) 					'Turn off high power
'	digitalWrite(LFlipLow, 0)  					'Switch off hold current	
'	rightDebounce = 0
'	RFlipTime = -10								'Make flipper re-triggerable, with debounce
'	RholdTime = 0								'Disable hold timer
'	digitalWrite(RFlipHigh, 0) 					'Turn off high power
'	digitalWrite(RFlipLow, 0)  					'Switch off hold current			
	GameTimer.Enabled = 0
	if (menuAbortFlag = 0) Then					'Game ended normally? (we didn't abort by entering the menu?)
		GameOver()								'Do normal game over stuff	
		gamesPlayed = gamesPlayed + numPlayers	'Increment games played
		saveAudits()							'Save game stats		
	End If
End Sub

Sub Wall23_timer()
	me.Timerenabled = 0
	NoGame1()
End Sub

Sub GameTimer_Timer()								'The Main Loop of the Game. We always keep this at the top
	houseKeeping()								'Do lights, switch debounce, get cab switches and control solenoids
'	flippers()									'EP- Disabling as this is an event driven game
	Timers()									'Event timers
	if (run = 2) Then							'Ball has been loaded, waiting for the ball to be launched?
		if (Sw57 = 0) Then					'Ball launched off shooter lane?
			launchCounter = launchCounter + 1
			'Serial.print("LAUNCH#")
			'Serial.println(launchCounter, DEC)
			run = 3								'Set condition
'			playMusic "M", 2					'Normal music
			musicplayer "bgout_M2.mp3"
		End If
	End If
	if (run = 3) Then								'If NOT in a drain state, do Logic and Switches
		logic()										'Think about things! Think... different...
'		switchCheck()								'Interpet the switches EP- Again, we're not cycle driven here
		secondsCounter = secondsCounter + 1
		if (secondsCounter > cycleSecond) Then		'Count # of seconds game has been active
			secondsCounter = 0
			totalBallTime = totalBallTime + 1
		End If
	End If
'**	PrFlipperL.RotY = LeftFlipper.CurrentAngle
'**	PrFlipperR.RotY = RightFlipper.CurrentAngle
End Sub

Sub TopMenu()									'EP- I'm not even going to think about doing maintenance stuff until I get the rest of this done
	
End Sub

Sub ShowGameStatus()
	
End Sub

sub addPlayer()																		'Adds additional players beyond Player 1
	if (numPlayers < 4) Then														'4 player limit.
		numPlayers = numPlayers + 1
		video "K", "P", numPlayers, noExitFlush, 45, 255						'Show new player intro, with NO NUMBERS	
		if (run <> 3) Then															'Ball still in shooter lane?			
			customScore "K", player, 64 + skillShot, allowSmall OR loopVideo, 120	'Update Skill Shot display
';			numbers 10, 2, 44, 27, numPlayers										'Update Number of players indicator			
		End If
		playSFX 0, "A", numPlayers + 64, 1 + random(4), 255							'ADJUST BASED OFF PLAYER ADDED
	Else																			'Player subtract only works in free play, and only removes players who haven't started a ball yet	
		if (freePlay and player < 4) Then											'If player 3 is ready to launch, Player 4 can be removed. But if player 4 is up, you are stuck with 4 players		
			numPlayers = player														'Change the total number of players to whichever player is up
'			video "K", "R", "1" + numPlayers, noExitFlush, 0, 255					'Show message (with offset in filename) EP- I don't have this gif, I have an old gif pack
			if (run <> 3) Then														'Ball still in shooter lane?			
				customScore "K", player, 64 + skillShot, allowSmall OR loopVideo, 120		'Update Skill Shot display
';				numbers 10, 2, 44, 27, numPlayers									'Update Number of players indicator			
			End If			
			playSFX 0, "P", "9", 65 + random(4), 255								'Random "I'm sitting this one out" quotes from Prison mode		
		End If	
	End If
End Sub

sub AttractMode()																	'Runs my slightly less crappy light show
	modeTimer = modeTimer + 1
	if (modeTimer = Int(1000/CycleAdjuster)) Then
		'digitalWrite(startLight, 0)
		modeTimer = 0
		if (lightCurrent = 0) Then
			GIpf(0)
			setCabModeFade 255, 0, 0, 25
		End If
		if (lightCurrent = 10) Then
			GIpf(128)
			setCabModeFade 0, 0, 255, 25
		End If		
		if (lightCurrent = 15) Then
			GIpf(208)
			setCabModeFade 255, 255, 255, 25
		End If	
		if (lightCurrent = 25) Then
			GIpf(240)
		End If	
		lightCurrent = lightCurrent + 1
		if (lightCurrent > lightEnd) Then			'Loop the animation
			lightCurrent = lightStart		
		End If
	End If
	multiTimer = multiTimer + 1							'Increment the light timer
	if (multiTimer > Int(5000/CycleAdjuster)) Then
		multiTimer = 0							'Reset timer
		if (multiCount) Then
			leftRGB(0) = leftRGB(0) + 1
			leftRGB(1) = leftRGB(1) - 1
			leftRGB(2) = leftRGB(2) + 1
			rightRGB(0) = rightRGB(0) - 1
			rightRGB(1) = rightRGB(1) + 1
			rightRGB(2) = rightRGB(2) - 1
			if (leftRGB(0) = 250) Then
				multiCount = 0					'Change direction
			End If
		Else
			leftRGB(0) = leftRGB(0) - 1
			leftRGB(1) = leftRGB(1) + 1
			leftRGB(2) = leftRGB(2) - 1
			rightRGB(0) = rightRGB(0) + 1
			rightRGB(1) = rightRGB(1) - 1
			rightRGB(2) = rightRGB(2) + 1
			if (leftRGB(0) = 100) Then
				multiCount = 1					'Change direction
			End If
		End If
		doRGB()
	End If
End Sub

sub AutoPlunge(whatTime)						'Loads a ball and shoots it. If called again before sequence completes, it will enqueue additional balls
	whatTime = Int(whatTime/cycleAdjuster)			'EP- adjusting it so here so I don't have to do this every time this sub is called
	if (plungeTimer) Then						'Already active?
		ballQueue = ballQueue + 1				'Add a ball to the queue, plunge it once current ball is launched
		Exit Sub								'Abort
	End If
	if (whatTime < Int(25002/CycleAdjuster)+2) Then	'Not the minimum? Set it to minimum
		whatTime = Int(25002/CycleAdjuster)+2
	End If
	plungeTimer = whatTime
End Sub

sub balconyApproach()						'Fresh hit on right orbit? (Didn't roll down from ORBS or back from balcony)
	Dim X
	animatePF 230, 10, 0
	if (hellMB and minion(player) < 100) Then
		tourGuide 0, 8, 4, 50000, 1									'Check for GHOST CATCH
	End If
	if (Mode(player) = 6) Then										'Prison?
		tourGuide 0, 6, 4, 25000, 1									'Check that part of the tour!
	End If
	if (hotProgress(player) > 29 and hotProgress(player) < 40) Then	'Fighting the Hotel Ghost? (can't do tour during the Control Box search)
		tourGuide 1, 5, 4, 25000, 1									'Check that part of the tour!		
	End If
	if (Mode(player) = 4) Then										'War fort?
		x = random(8)
		playSFX 0, "W", "5", 65 + x, 210							'Random Army Ghost lines
		if (tourGuide(0, 4, 4, 25000, 0) = 0) Then
			video "W", "5", 65 + x, allowSmall, 79, 250				'Synced taunt video
		End If														'Check that part of the tour (no WHOOSH sound needed)		
	End If
	if (barProgress(player) > 69 and barProgress(player) < 100) Then			'Haunted Bar?
		tourGuide 0, 3, 4, 25000, 1									'Check that part of the tour!
	End If
	if (Mode(player) = 1) Then										'Hospital?
		tourGuide 1, 1, 4, 25000, 1									'Check that part of the tour!
	End If
	if (skillShot) Then												'On the off chance it somehow gets by the ORB rollovers on a launch...			
		if (skillShot = 2) Then										'Did we hit the Skill shot?
			skillShotSuccess 1, 0									'Success!
			DOF 117, 2
		Else
			skillShotSuccess 0, 255									'Failure, so just disable it
		End If
	End If	
	if (Advance_Enable) Then										'Are we trying to advance theater?
		playSFX	0, "T", "9", "Y", 200								'Run and jump sound
		video "T", "9", "Y", allowSmall, 35, 200					'Run and jump animation
	End If
	if (deProgress(player) > 9 and deProgress(player) < 100) Then	'Trying to weaken demon
		DemonCheck(4)
	End If
	if (hotProgress(player) = 20)	Then							'Searching for the Control Box?
		BoxCheck(3)													'Check / flag box for this location
	End If
	if (Mode(player) = 7) Then										'Are we in Ghost Photo Hunt?
		photoCheck(4)
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then			'Theater Ghost?	
		'TheaterPlay(0)					'Incorrect shot, ghost will bitch!
		'Sweet Jumps!
		playSFX 0, "T", "9", "Y", 200								'Run and jump sound
		video "T", "9", "Y", allowSmall, 35, 210					'Run and jump animation
	End If
	if (minionMB > 9) Then						'Minion Jackpot increase?
		minionJackpotIncrease()
		lightningStart(Int(50000/CycleAdjuster))
		MagnetSet(100)
	End If					
End Sub

sub balconyJump()													'What happens when you successfully make the Balcony Jump
	'There are 4 possible things that happen when you make the Balcony Jump
	'1: No Mode Active / Theater Not started = Show combo, advance theater
	'2: Theater Active = Do not show combo (but light it as one) Advance Super Jumps, add 500k per jump. Not combo timer dependent (but mode itself is timed)
	'3: No Mode Active / Theater Completed = Do not show combo, advance Super Jumps, add 100k per jump with combo timer active
	'4: Other Mode Active = Show combo, normal logic (most logic uses the balcony approach, but make the jump to score/light combo)
	if (theProgress(player) < 3 and Advance_Enable) Then			'CASE 1: Are we eligible to advance Theater?
		comboCheck(4)												'Normal combo check
		TheaterAdvance()											'Advance Theater
		Exit Sub
	End If
	if (theProgress(player) = 100 and Advance_Enable) Then		'CASE 3: Theater has been completed?
		sweetJumpBonus = sweetJumpBonus + 100000				'100k added per jump. Resets when combo times out.
		sweetJump = sweetJump + 1								'So making shot ALWAYS awards at least 100k, and that increases if you combo shots together
		if (sweetJump > 12) Then								'Limit the animations/SFX, but no limits of total # of Sweet Jumps and bonus value
			sweetJump = 12										'You only get 15 seconds per shot in theater mode, so unless you make a jump a second
		End If													'highly unlikely you'll ever hit the limit of 12			
		playSFX 1, "T", "S", 64 + sweetJump, 255				'Whooshing jump sound FX
		video "T", "J", 64 + sweetJump, allowSmall, 30, 255		'Jump Complete video
		showValue sweetJumpBonus, 0, 1
		Exit Sub
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then		'During Theater mode, spam this shot for Sweet Jumps!
		sweetJumpBonus = sweetJumpBonus + 500000				'500k more per jump (worth more in actual mode. Resets when you add time by hitting ghost. Risk/reward!
		sweetJump = sweetJump + 1
		if (sweetJump > 12) Then								'Limit the animations, but no limits of total # of Sweet Jumps
			sweetJump = 12										'You only get 15 seconds per shot in theater mode, so unless you make a jump a second
		End If													'highly unlikely you'll ever hit the limit of 12			
		playSFX 1, "T", "S", 64 + sweetJump, 255				'Whooshing jump sound FX
		video "T", "J", 64 + sweetJump, allowSmall, 30, 255		'Jump Complete video
		showValue sweetJumpBonus, 0, 1
		Exit Sub
	End If
	'If in another mode, or Theater is lit but not collected, prompt standard combos
	comboCheck(4)													'Normal combo check
	comboVideoFlag = 0												'Nothing active? Reset video combo flag	
	AddScore(5000)													'Some points
	'Nothing going on default prompt
	video "C", "G", "E", allowSmall, 39, 250						'Regular Combo to the Right ->	
	playSFX 2, "A", "Z", "Z", 255									'Whoosh!		
End Sub

sub ballElevatorLogic()													'What happens when a ball goes in the Elevator on second floor
	'Serial.println("BALL IN HELLAVATOR")
	if (Mode(player) <> 3) Then											'Prevents Ghost Whore from taunting you when loading MB in her mode
		ghostLooking(165)
	End If
	if (HellBall = 0) Then												'Ball just went into Hellavator, and we're allowed to lock balls?
		if (hotProgress(player) = 30) Then								'Waiting for Jackpot Enable shot?
			HellBall = 10												'Set flag for ball Transit
			ElevatorSet hellDown, 20									'Move elevator down
			light 41, 0													'Flasher OFF
			HotelLightJackpot()
			Exit Sub
		End If
		if (theProgress(player) = 10) Then								'Waiting for first shot in Theater Mode?
			HellBall = 10												'Set flag for ball Transit
			TheaterPlay(1)
			Exit Sub
		End If
		if (deProgress(player) = 4) Then								'Third locked ball for Demon Mode?
			DemonLock3()
			Exit Sub
		End If
		if (hotProgress(player) = 3 and Advance_Enable = 1) Then		'Ready to start Hotel Mode?
			HotelStart1()
			Exit Sub													'So the ghost doesn't move
		End If
		if (hellLock(player) = 1) Then									'Can we lock a ball? We don't care where the elevator actually is (that caused a bug)
			light 41, 0													'Turn off flasher
			HellBall = 10												'Set flag for ball Transit
			ElevatorSet hellDown, 150									'Move elevator down (was 175)
			light 25, 7													'Current state is SOLID
			blink(24)													'Other state BLINKS
			light 30, 0													'Lock is NOT lit				
			if (multiBall AND multiballHell) Then							'Minion MB is a MB without hell locks, but if Hell is enabled, we must be in Hell MB, or Hell MB + Minion MB
				video "Q", "J", "D", 1, 20, 255					'Jackpot!
				playSFX 0, "Q", "J", "D", 255	
				showValue hellJackpot(player), 40, 1
				flashCab 255, 255, 255, 50
				strobe 26, 5
				if (hellMB) Then
					customScore "Q", "B", "A", allowAll OR loopVideo, 90	'Custom Score: Ramp increase, Ghost Catch
				End If
			Else														'Only increase if we AREN'T in a multiball
				callHits = 0											'Reset # of Call Hits
				AddScore(50000)											'Some points
				lockCount(player) = lockCount(player) + 1				'Increase count to Multiball. Need 3.
				killQ()
				video "Q", "A", lockCount(player), 0, 56, 255 		'Show people going down in an elevator, or MB starting animation
				light 26, 0												'Clear hotel progress lights
				light 27, 0
				light 28, 0
				light 41, 0												'Turn off flasher
				blink(30)
				'Set lights
				if (lockCount(player) = 1) Then							'One ball locked
					light 26, 7
					flashCab 255, 0, 255, 100
				End If
				if (lockCount(player) = 2) Then							'Two balls locked?
					light 26, 7
					light 27, 7
					flashCab 255, 0, 255, 100
				End If
				if (lockCount(player) = 3) Then							'Three balls locked?
					blink 26
					blink 27
					blink 28						
					multiBallStart(1)						
				Else
					playSFX 0, "Q", "A", lockCount(player), 255	'Ball 1 or 2 LOCKED!
					animatePF 74, 30, 0									'Vertical lock swoosh
				End If
			End If			
		End If	
	End If
End Sub

sub ballExitElevatorLogic()
	'Serial.println("HELL EXITED")
	HellBall = 0														'Clear flag
	if (hellLock(player)) Then											'Can balls be locked?
		if (multiBall = 0 and lockCount(player) < 3) Then				'Haven't started multiball yet, but we are able to?
			'Once a Ball is locked, revert the Ramp Lights to HOTEL state
			light 25, 7													'Current state is SOLID
			blink(24)													'Other state BLINKS
			light 30, 0													'Lock is NOT lit now (elevator is down)
			if (Advance_Enable) Then									'We're not in a mode? See if we paint Hotel Lights...
				if (hotProgress(player) < 100) Then						'Able to advance hotel?
					if (hotProgress(player) = 0) Then					'Show how far it is
						pulse(26)
						light 27, 0
						light 28, 0
						light 29, 0		
					End If
					if (hotProgress(player) = 1) Then
						light 26, 7
						pulse(27)
						light 28, 0
						light 29, 0		
					End If
					if (hotProgress(player) = 2) Then
						light 26, 7
						light 27, 7
						pulse(28)
						light 29, 0		
					End If		
					if (hotProgress(player) = 3) Then
						light 26, 7
						light 27, 7
						light 28, 7
						pulse(29)		
					End If	
				Else													'Hotel complete? No lights. (they didn't leave the light on for ya)
					light 26, 0
					light 27, 0
					light 28, 0
					light 29, 0
				End If		
			End If	
		End If									
	End If		
End Sub

sub ballSave()														'Call this to set (enable) Ball Save. Time can vary per player
	if (saveTimer < ((saveCurrent(player) + 2) * cycleSecond)) Then	'If you're awarded a 30 second ball save, don't want a new one that's less! 8-22-14 fix
		saveTimer = ((saveCurrent(player) + 2) * cycleSecond)		'Default is 5 seconds, can be changed in menu 8-22-14 fix
		'blink(56)													'Blink the SPOOK AGAIN light
	End If
	spookCheck()													'See what to do with the light
End Sub

sub ballSaveScoop()													'If scoop shoots right down the drain, you get ball back (2 second "silent" ball save)
	Dim calculateTime
	calculateTime = scoopSaveStart * cycleMilliSecond
	if (saveTimer < calculateTime) Then								'1.5 seconds - 2 is a BIT too much
		saveTimer = calculateTime									'Now it's user defined, in milliseconds
		'saveTimer = 18000											'Just low enough to prevent light from turning on
	End If
	spookCheck()													'See what to do with the light
End Sub

sub ballSearch()									'Can't find balls? This routine tries to find them
	if (Sw22) Then									'Ball in Basement Scoop?
'		Coil(ScoopKick, scoopPower)
		KiVUK1.Enabled = 0
		MoveBall BallMover2, KiVUK1, KiVUK3, 205, 25, 0
		Playsound SoundFX("ballrelease")
		DOF 122, 2
		KiVUK1.TimerEnabled = 1						'Start a timer that will re-enabled the VUK very shortly after the kick-out
		Sw22 = 0
		'Serial.println("Ball BASEMENT SCOOP")
	End If
	if (Sw23) Then									'Ball behind door?
'		Coil(LeftVUK, vukPower)
		VUKKicker KiDoor, vukPower					'EP- My version of the VUK kicker
		'Serial.println("Ball LEFT VUK")
	End If
	if (Sw57) Then									'Ball in shooter lane?
		'Serial.println("Ball Search KICK")
'		Coil(Plunger, plungerStrength)				'Kick it out!
		AutoPlunger.AutoFire
		DOF 124, 2		
		'Serial.println("Ball SHOOTER LANE")
	End If
End Sub

sub ballSearchDebounce(onOff)						'EP- No debounce needed
End Sub

sub ballClear()								'Like ball search, clears out locked balls
End Sub

'Functions for Bar Ghost Mode 4........................
sub BarAdvance()													'X number of pops advances bar
	AddScore(popScore)
	areaProgress(player)  = areaProgress(player)  +  1					'Total mode progress	
	barProgress(player)  = barProgress(player)  +  1					'Increment Bar Progress
	flashCab 0, 255, 0, 10					'Flash the GHOST BOSS color
	if (barProgress(player) > 0 and barProgress(player) < 26) Then ' and centerTimer = 0) 				'If we haven't filled it yet, show the progress
		video "B", "A", BarProgress(player)+64, allowBar OR allowSmall OR preventRestart, 40, 250
'		showProgressBar(4, 3, 12, 26, barProgress player) * 4, 4
'		showProgressBar(5, 10, 12, 27, barProgress player) * 4, 2				
	End If
	if (barProgress(player) = 6) Then
		playSFX 0, "B", "1", random(4) + 65, 250 'Advance sound 1					
		Exit Sub
	End If				
	if (barProgress(player) = 18) Then
		playSFX 0, "B", "2", random(4) + 65, 250 'Advance sound 2					
		Exit Sub
	End If
	if (barProgress(player) = 26) Then			'Did we fill the bar?	
		killQ()
		stopVideo(0)
		video "B", "4", "0", 0, 90, 255		'Prompt for Bar Ghost Lit (can override Center Shot
		playSFX 0, "B", "3", random(4) + 65, 250 'Advance sound 3	
		'centerTimer = 25000					'Prevents pop bumper jackpot from overiding prompt video		
		barProgress(player) = 50				'50 indicates Mode is ready to start.			
		popLogic(3)							'Pops won't do anything else until you start the mode
		spiritGuideEnable(0)	
		showScoopLights()						'Update the Scoop Lights
		Exit Sub
	End If
	popToggle()	
	stereoSFX 1, "B", "Z", random(3) + 65, 100, leftVolume, rightVolume
End Sub

sub BarStart()														'What happens when we shoot the scoop to start Bar Mode 3
	light 45, 0														'Turn off the mode start light in player bank
	restartKill 3, 1												'In case we got the Restart	
	comboKill()														'So combo lights don't appear after the mode	
	storeLamp(player)												'Store the state of the Player's lamps
	allLamp(0)														'Turn off the lamps
	spiritGuideEnable(0)											'No spirit guide during Bar
	modeTotal = 0													'Reset mode points	
	AddScore startScore/2
	minionEnd(0)													'Disable Minion mode, even if it's in progress
	setGhostModeRGB 0, 0, 255										'Blue mode color
	setCabModeFade 0, 63, 0, 200									'Set mode color to DIM GREEN, fade to that color
	popLogic(3)														'Set pops to EVP
	Mode(player) = 3												'Ghost whore mode start!
	Advance_Enable = 0												'Mode has started, others can't
	DoorSet DoorOpen, 100
	light 45, 7														'Turn BAR start light SOLID
	blink(60)														'Blink the Mode Light during battle.
	blink(17)														'Blink the targets for the Ghost Whore
	blink(18)
	blink(19)
	Light 16, 0
	barProgress(player) = 60										'Set flag, ghost waiting to be touched!
	jackpotMultiplier = 1											'Reset this just in case
	videoModeCheck()
	loopCatch = catchBall											'Flag that we want to catch the ball in the loop
	'Serial.println("GHOST START")
	'VOICE CALL, GHOST APPEARS
	TargetTimerSet 50000, TargetDown, 100							'Put targets down slowly so we notice.
	killQ()															'Disable any Enqueued videos
	video "B", "4", "A", 1, 120, 255								'Show the Ghost!
	playSFX 0, "B", "4", random(3) + 65, 255						'Mode start dialog. Come resist my charms!	
'	playMusic "B", "1"												'Boss battle music!
	musicplayer "bgout_B1.mp3"
	customScore "B", "1", "A", allowAll OR loopVideo, 36			'Shoot the Ghost custom score prompt
';	numbers 8, numberScore OR 2, 0, 0, player						'Show player's score in upper left corner
';	numbers 10, 9, 88, 0, 0											'Ball # upper right	
	ScoopTime = Int(80000/CycleAdjuster)							'Flag to kick the ball back out
	hellEnable(0)													'We can lock balls during this mode, but not until we trap the ball
	showProgress 1, player											'Show the progress, Active Mode style
	dirtyPoolMode(0)												'Allow balls to be trapped
	skip = 30														'Set skip mode to Bar Ghost
End Sub

sub BarLogic()														'What happens during Ghost Battle Mode
	Dim X
	if (barProgress(player) > 69 and barProgress(player) < 80) Then
		modeTimer = modeTimer + 1
		if (modeTimer = Int(80000/CycleAdjuster)) Then				'Random ghost taunt?						
			playSFX 0, "B", "8", 65 + random(8), 255				'Will not override advance dialog
			video "B", "8", "A", allowSmall, 45, 254				'Will not override video		
			'MagnetSet(200)
		End If
		if (modeTimer > Int(160000/CycleAdjuster)) Then				'Kaminski prompt?
			modeTimer = 0											'Reset timer
			playSFX 0, "B", "7", 65 + random(8), 255				'Will not override advance dialog
			video "B", "7", "A", allowSmall, 65, 254				'Will not override video					
			'MagnetSet(200)
		End If
	End If
	if (barProgress(player) > 79 and barProgress(player) < 100) Then		'Battling Ghost whore multiball!
		modeTimer = modeTimer + 1
		if (modeTimer = Int(70000/CycleAdjuster)) Then
			lightningStart(1)			'Do some lightning!		
			x = random(2)
			if (x) Then
				playSFX 0, "B", "B", random(10), 255				'Team Leader commanding ghost to leave and stuff
			Else
				playSFX 1, "L", "G", random(8), 255					'Random lightning		
			End If				
		End If		
		if (modeTimer > Int(100000/CycleAdjuster)) Then
			modeTimer = 0											'Reset timer
		End If
	End If
End Sub

sub BarTrap()														'What happens when you shoot the ghost and she captures your teammate
	if (restartTimer) Then
		restartKill 3, 1
		comboKill()													'So combo lights don't appear after the mode	
		storeLamp(player)											'Store the state of the Player's lamps
		allLamp(0)													'Turn off the lamps
		showProgress 1, player										'Show the Main Progress lights
		spiritGuideEnable(0)										'No spirit guide during Bar			
		modeTotal = 0												'Reset mode points	
		minionEnd(0)												'Disable Minion mode, even if it's in progress
		setGhostModeRGB 0, 0, 255									'Blue mode color
		popLogic(3)													'Set pops to EVP
		Mode(player) = 3											'Ghost whore mode start!
		Advance_Enable = 0											'Mode has started, others can't
		DoorSet DoorOpen, 100
		tourReset(58)										'Reset the Tour bits			
'		playMusic "B", "1"											'Boss battle music!
		musicplayer "bgout_B1.mp3"
	End If
	setCabModeFade 0, 255, 0, 200									'Turn lighting GREEN (with envy)
	AddScore startScore/2											'Points for getting trapped
	barProgress(player) = 70										'Advance the mode.
	dirtyPoolMode(0)												'Disable dirty pool check (since we DO want to trap the ball)
	trapTargets = 1													'A ball is trapped!
	modeTimer = 0													'Reset mode timer
	activeBalls = activeBalls - 1									'Remove an active ball
	killQ()															'Disable any Enqueued videos
	video "B", "6", "A", 1, 135, 255								'Kaminski trapped!
	playSFX 1, "B", "6", random(4) + 65, 255						'You're mine now sugar!
	pulse(17)														'Now pulse the lights
	pulse(18)
	pulse(19)
	hellEnable(1)													'We can lock balls during this mode, but not until we trap the ball
	customScore "B", "1", "B", allowAll OR loopVideo, 33			'Clear Targets for Multiball custom message
'	numbers 8, numberScore OR 2, 0, 0, player						'We re-send these in case a quick restart occurred EP- can't discern what these are
'	numbers(10, 9, 88, 0, 0)										'Ball # upper right	
';	numbers "", Ball, "", ""
	tourReset(58)													'Tour: Left orbit, door, up middle, right orbit.
																	'Hotel path: Can lock balls
																	'Scoop: Steals kegs		
	ghostAction = Int(140000/CycleAdjuster)
	AutoPlunge(70000)												'Set flag to launch second ball	
	skip = 35
End Sub

sub BarTarget(whichTarget)							'Logic for determining which targets in Bar Ghost mode have been cleared
	ghostAction = Int(20000/CycleAdjuster)							'Set WHACK routine.
	MagnetSet(100)
	ghostFlash(100)
	if (gTargets(whichTarget) = 1) Then								'Already hit that one?
		playSFX 0, "B", "7", 83 + random(8), 255					'Will not override advance dialog
		video "B", "8", "D", allowSmall, 45, 255					'"Clear Flashing Targets to start Multiball"	
		AddScore(25000)												'Some points		
	Else
		targetsHit = targetsHit + 1									'Increase how many targets we've hit
		if (targetsHit = 2) Then									'Almost ready?
			customScore "B", "1", "C", allowAll OR loopVideo, 32	'Clear Targets MULTIBALL READY!!!
		End If
		gTargets(whichTarget) = 1									'Set the flag that we hit this
		light 17 + whichTarget, 7									'Make light SOLID
		AddScore(250000)
		modeTimer = 0												'Reset timer to asub overlap
		if (gTargets(0) = 1 and gTargets(1) = 1 and gTargets(2) = 1) Then				'Cleared them all?
			BarMultiball()											'Begin multiball
		Else
			playSFX 0, "B", "5", 88 + random(3), 255				'Ghost yelp!
			video "B", "8", "E", allowSmall, 23, 255				'Ghost whacked! (or maybe life bar?)
			videoQ "B", "8", 65 + targetsHit, allowSmall, 30, 200	'How many hits are left			
		End If
	End If
End Sub

sub BarMultiball()													'When you free your teammate and multiball to bash the ghost
	spiritGuideEnable(0)
	ghostAction = 0
	kegsStolen = 0													'Shooting the scoops lets you steal up to 10 kegs of beer for bonus points
	whoreJackpot = 0												'Reset this per instance
	AddScore(winScore)												'Points for beating ghost
'	playMusic "G", "S"
	musicplayer "bgout_GS.mp3"
	modeTimer = 0													'Reset timer for exorcist quotes
'	ModeWon(player) |= 1 << 3										'Set BAR WON bit for this player.	
	ModeWon(player) = ModeWon(player) OR 8
	if (countGhosts() = 6) Then										'This the final Ghost Boss? Light BOSSES solid!
		light 48, 7
	End If
'	swDebounce(24) = 50000											'Temporarily set Ghost Hit debounce really high so ball release won't trigger a Jackpot
	barProgress(player) = 80										'Set flag for Ghost Whore Multiball
	light 60, 7														'Bar mode light solid because A Winner Is You!
	pulse(16)														'Pulse "Make Contact"
	strobe 17, 3
	pulse(47)														'Pulse Scoop Camera for beer stealing
	TargetTimerSet 10, TargetDown, 50								'Put targets down fairly quickly
	trapTargets = 0													'Ball is no longer trapped
	activeBalls = activeBalls + 1
	killQ()															'Disable any Enqueued videos	
	playSFX 0, "B", "9", 65 + random(4), 255						'I'm Free! Let's get her dialog
	video "B", "9", "A", 1, 121, 255 								'Play Mad Ghost video
	'videoQ('B', '9', 'B', 2, 0, 255) 								'Jackpot Prompt
	manualScore 0, EVP_Jackpot(player) + 75000						'Set what next Jackpot is worth, boss value + (Whore Hits * 75k)
	customScore "B", "1", "D", allowAll OR loopVideo, 36			'Custom Score: Hit ghost for JACKPOTS!
'	numbers(8, numberScore | 2, 0, 0, player)						'Put player score upper left
	numbers PlayerScore(player), "", "", ""
'	numbers(9, numberScore | 2, 72, 27, 0)							'Use Score #0 to display the Jackpot Value bottom off to right
	numbers "", "", "", EVP_Jackpot(player)
'	numbers(10, 9, 88, 0, 0)										'Ball # upper right
	numbers "", Ball, "", ""
	dirtyPoolMode(1)
	multipleBalls = 1												'When MB starts, you get ballSave amount of time to loose balls and get them back
	ballSave()														'That is, Ball Save only times out, it isn't disabled via the first ball lost		
End Sub

sub BarWin()														'When down to last ball, mode 3 is won
	DOF 134, 2
	Dim x
	if (multiBall) Then												'Was a MB stacked?
		multiBallEnd(1)												'End it, with flag that it's ending along with a mode
	End If
	multipleBalls = 0
	tourClear()														'Clear the tour lights / values
	loadLamp(player)
	comboKill()
	spiritGuideEnable(1)
	ghostModeRGB(0) = 0												'Fade out ghost
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(200/cycleAdjuster)
	ghostFadeAmount = Int(200/CycleAdjuster)
	lightningKill()
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset to default color
	if (countGhosts() = 5) Then										'Is this the last Boss Ghost to beat?
		blink(48)													'Blink that progress light
	End If
	light 16, 0														'Turn off "Make Contact"
	light 17, 0
	light 18, 0	
	light 19, 0
	light 60, 7														'Turn Bar Mode solid because we won!
	light 45, 0														'Make sure BAR START is off
	Mode(player) = 0												'Set mode active to None
	barProgress(player) = 100										'Flag that reminds us this mode has been won
	playSFX 0, "B", "9", 89 + random(2), 255						'I'm Free! Let's get her dialog
'	playMusic "M", "2"												'Normal music
	musicplayer "bgout_M2.mp3"
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()
	killQ()															'Disable any Enqueued videos	
	video "B", "0", "Z", noExitFlush, 59, 255 						'Play Death Video
';	numbersPriority(0, numberFlash | 1, 255, 11, modeTotal, 233)	'Load Mode Total Points
	numbers "", "", ModeTotal, ModeTotal
	modeTotal = 0													'Reset mode points		
	videoQ "B", "0", "V", noEntryFlush OR 3, 45, 233		'Mode Total:	
'	ModeWon(player) |= 1 << 3										'Set BAR WON bit for this player.
	ModeWon(player) = ModeWon(player) OR 8
	ghostsDefeated(player) = ghostsDefeated(player) + 1				'For bonuses
	Advance_Enable = 1
	if (countGhosts() = 2 or countGhosts() = 5) Then				'Defeating 2 or 5 ghosts lights EXTRA BALL
		extraBalllight 2											'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random(2), 255)	'"Extra Ball is Lit!"			
	End If	
	demonQualify()													'See if Demon Mode is ready
	checkModePost()
	hellEnable(1)
	for x=0 To 5													'Make sure the MB lights are off
		light 26 + x, 0
	Next
	showProgress 0, player											'Show the progress, Active Mode style
	comboEnable = 1													'OK combo all you want
End Sub

Function BarFail()													'Returns a 1 if we can try again, 0 if not
	Dim x
	multipleBalls = 0
	tourClear()														'Clear the tour lights / values
	loadLamp(player)
	spiritGuideEnable(1)
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(100/CycleAdjuster)
	ghostFadeAmount = Int(100/CycleAdjuster)
	lightningKill()
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset to default color
'	if (ModeWon(player) & (1 << 3)) Then							'Did we win this mode before?
	If ((ModeWon(player) AND 8) = 8) Then
		light 60, 7													'Make Hospital Mode light solid, since it HAS been won
	Else
		light 60, 0													'Haven't won it yet, turn it off
	End If
	light 16, 0														'Turn off "Make Contact"
	light 17, 0
	light 18, 0	
	light 19, 0
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	Mode(player) = 0												'Set mode active to None	
	Advance_Enable = 1	
	hellEnable(1)	
	TargetSet(TargetDown)											'Release the ball and let it drain, or be caught by player!	
	trapTargets = 0
	'BROKEN
	if (barProgress(player) = 60) Then								'Didn't even hit the ghost to start?
		'dirtyPoolMode(1)											'Don't want to trap balls anymore
		loopCatch = 0												'No longer want to trap ball
		checkModePost()												'In this condition, you lose your ball
'		if (modeRestart(player) & (1 << 3)) Then					'Able to restart Bar?
		if ((modeRestart(player) AND 8) = 8) Then
'			modeRestart(player) &= ~(1 << 3)						'Clear the restart bit
			modeRestart(player) = (modeRestart(player) AND 247)
			barProgress(player) = 50
			pulse(45)												'Pulse BAR GHOST start light
			popLogic(3)												'Set pops to EVP
		Else
			barProgress(player) = 0									'Gotta start over!
			if (fortProgress(player) < 50) Then						'Haven't completed the Fort yet?
				popLogic(1)											'Set pops to advance Fort
			Else
				popLogic(2)											'Else, have them re-advance Bar Ghost until we get it
			End If		
			light 45, 0												'Turn off BAR GHOST start light
		End If
		BarFail = 0
		Exit Function													'In this condition, you lose your ball
	End If
	'Else, you must have started the Bar Fight!
'	if (modeRestart(player) & (1 << 3) and tiltFlag = 0) Then		'Able to restart Bar?
	if (((modeRestart(player) AND 8) = 8) AND tiltFlag = 0) Then
'		modeRestart(player) &= ~(1 << 3)							'Clear the restart bit	
		modeRestart(player) = (modeRestart(player) AND 247)
		restartBegin 3, 11, 25000									'Enable a restart!		
		barProgress(player) = 60									'Waiting for Ghostly Embrace!
		loopCatch = catchBall										'Flag that we want to catch the ball in the loop	
		dirtyPoolMode(0)											'Disable dirty pool, like Ghost Start does
		doorLogic()													'Since we opened the door, see what we're supposed to do with it if mode ends
		blink(17)
		blink(18)
		blink(19)
		activeBalls = activeBalls + 1								'Count the ball we just released		
'		playMusic "H", "2"											'Hurry Up Music!		
		musicplayer "bgout_H2.mp3"
		killQ()														'Disable any Enqueued videos	
		video "B", "0", "Y", 1, 109, 255 							'Mode fail! Shoot door to restart!
		playSFX 0, "B", "R", 65 + random(6), 255					'You've got 5 seconds to come back honey!
		BarFail = 1
		Exit Function													'Flag to prevent a drain!
	Else															'End mode, and let the ball drain
		barProgress(player) = 0										'Gotta start over				
		if (fortProgress(player) < 50) Then							'Haven't completed the Fort yet?
			popLogic(1)												'Set pops to advance Fort
		End If		
		dirtyPoolMode(1)											'Don't want to trap balls anymore
		checkModePost()
		TargetSet(TargetDown)										'Release the ball...
		TargetTimerSet 20000, TargetUp, 10							'and put targets back up after a bit				
		showProgress 0, player
		BarFail = 0
		Exit Function
	End If
	for x=0 To 5													'Make sure the MB lights are off
		light 26 + x, 0
	Next
	showProgress 0, player											'Show the progress, Active Mode style
	comboEnable = 1													'OK combo all you want
End Function
'Functions for Bar Ghost Mode 4........................

sub callButtonLogic()												'What to do when player hits Call Elevator button. Mostly, when you CAN'T control it for MB
	if (hellLock(player) = 0) Then
		AddScore(10000)
		playSFX 2, "H", "0", "0", 100								'Door clunking sound
		Exit Sub
	End If
	if (Mode(player) = 1 and patientStage) Then						'In hospital, trying to poison ghosts?
		AddScore(10000)	
		playSFX 2, "H", "0", "0", 100								'Door clunking sound
		Exit Sub													'Can't move it
	End If
	if (Mode(player) = 7) Then										'Hotel mode uses elevator too much, so no MB with it
		AddScore(10000)	
		playSFX 2, "H", "0", "0", 100								'Door clunking sound
		Exit Sub
	End If
	if (theProgress(player) > 3 and theProgress(player) < 100) Then	'Doing Theater mode?
		AddScore(10000)
		playSFX 2, "H", "0", "0", 100								'Door clunking sound
		Exit Sub
	End If
	if (hotProgress(player) > 2 and hotProgress(player) < 100) Then	'Doing or about to start Hotel mode?
		AddScore(10000)	
		playSFX 2, "H", "0", "0", 100								'Door clunking sound
		Exit Sub
	End If	
	if (deProgress(player) > 0 and deProgress(player) < 100) Then	'In wizard mode?
		AddScore(10000)	
		playSFX 2, "H", "0", "0", 100								'Door clunking sound
		Exit Sub
	End If
	'If none of those, then you can control it
	if (HellLocation = hellDown) Then								'If Hell was DOWN, move it UP	
		AddScore(25000)
		if (multiBall AND multiballHell) Then						'Hellavator multiball mode active?
			video "Q", "J", "A", 1, 42, 200							'Jackpot ready!
			playSFX 2, "Q", "J", "A", 200
			blink(41)												'Hell flasher
			strobe 26, 4											'Strobe the first 4 lights
			blink(30)												'Blink LOCK. Sort of makes sense.
			light 24, 0												'In MB, once up, Hellavator can't be moved
			light 25, 0												'So turn off both lights		
			if (hellMB) Then
				customScore "Q", "B", "B", allowAll OR loopVideo, 30		'Custom Score: JACKPOT READY!
			End If
		Else
			light 24, 7												'Current state is SOLID
			blink(25)												'Other state BLINKS		
			video "Q", "A", "B", 1, 42, 200							'Hellavator Lock is Lit!
			playSFX 2, "Q", "A", "B", 210
			pulse(30)												'Elevator UP, Lock is lit! (and so am I!)
			blink(41)												'Flash Hellavator flasher
			light 26, 0												'Clear hotel progress lights
			light 27, 0
			light 28, 0
																	'Show multiball progress
			if (lockCount(player) = 0) Then							'No balls locked?
				blink(26)											'Blink "1"
			End If
			if (lockCount(player) = 1) Then							'One ball locked already?
				light 26, 7											'1 solid, blink 2
				blink(27)
			End If
			if (lockCount(player) = 2) Then							'Two balls locked?
				light 26, 7											'1 and 2 solid, blink 3
				light 27, 7
				blink(28)
			End If
		End If
		ElevatorSet hellUp, 100									 	'Send Hellavator to 2nd Floor.
	End If
	if (HellLocation = hellUp) Then									'If Hell was UP, move it DOWN (unless Hell MB active awaiting Jackpot)
		AddScore(25000)
		if (multiBall) AND (multiballHell) Then						'Hellavator multiball mode active? Don't let button do ANYTHING (keep hellavator UP)
			video "Q", "A", "6", 1, 30, 200							'Right Ramp Builds value!
			playSFX 2, "H", "0", "0", 100							'CLUNK!
			strobe 26, 5											'Strobe first 5 lights	
		Else
			light 25, 7												'Current state is SOLID
			blink(24)												'Other state BLINKS					
			ElevatorSet hellDown, 100 								'Send Hellavator to 1st Floor.		
			light 26, 0												'Turn off lights. We'll rebuild them for Hotel progress
			light 27, 0
			light 28, 0	
			light 29, 0
			light 30, 0												'Turn off LOCK
			light 41, 0												'Turn off Hell Flasher
			playSFX 2, "Q", "A", "A", 210							'Sound no matter what!
			if (Advance_Enable) Then								'Modes can be advanced, and Hotel hasn't been won yet?
				if (hotProgress(player) < 100) Then					'Able to advance hotel?
					if (hotProgress(player) = 0) Then
						pulse(26)
						light 27, 0
						light 28, 0
						light 29, 0		
					End If
					if (hotProgress(player) = 1) Then
						light 26, 7
						pulse(27)
						light 28, 0
						light 29, 0		
					End If
					if (hotProgress(player) = 2) Then
						light 26, 7
						light 27, 7
						pulse(28)
						light 29, 0		
					End If		
					if (hotProgress(player) = 3) Then
						light 26, 7
						light 27, 7
						light 28, 7
						pulse(29)		
					End If	
					video "Q", "A", "A", 1, 42, 200					'Advance Hotel Open!
				Else												'Hotel already complete!
					light 26, 0
					light 27, 0
					light 28, 0
					light 29, 0
					video "Q", "A", "C", 1, 42, 200					'Path Open!
				End If
			Else
				video "Q", "A", "C", 1, 42, 200						'Path Open!				
			End If	
		End If
	End If
End Sub

sub centerPathCheck()												'When a ball is shot up the middle, and hasn't fallen from the jump ramp
	Dim X
	animatePF 210, 10, 0
	if (hellMB and minion(player) < 100) Then
		tourGuide 1, 8, 2, 50000, 1									'Check for GHOST CATCH
		Exit Sub
	End If	
	if (Mode(player) = 6) Then										'Prison?
		tourGuide 2, 6, 2, 25000, 1									'Check that part of the tour!
		Exit Sub
	End If
	if (hotProgress(player) > 29 and hotProgress(player) < 40) Then	'Fighting the Hotel Ghost? (can't do tour during the Control Box search)
		tourGuide 2, 5, 2, 25000, 1									'Check that part of the tour!	
		Exit Sub
	End If
	if (Mode(player) = 4) Then										'War fort?
		Dim Y
		X = random(8)
		If X=0 Then Y=79
		If X=1 Then Y=81
		If X=2 Then Y=50
		If X=3 Then Y=47
		If X=4 Then Y=49
		If X=5 Then Y=110
		If X=6 Then Y=77
		If X=7 Then Y=67
		playSFX 0, "W", "5", 65 + x, 210							'Random Army Ghost lines
		if (tourGuide(2, 4, 2, 25000, 0) = 0) Then
			video "W", "5", 65 + x, allowSmall, Y, 250				'Synced taunt video
		End If														'Check that part of the tour (no WHOOSH sound needed)		
		Exit Sub
	End If
	if (barProgress(player) > 69 and barProgress(player) < 100) Then		'Haunted Bar?
		tourGuide 1, 3, 2, 25000, 1									'Check that part of the tour!
		Exit Sub
	End If
	if (Mode(player) = 1) Then										'Hospital?
		tourGuide 2, 1, 2, 25000, 1									'Check that part of the tour!
		Exit Sub
	End If
	if (deProgress(player) > 9 and deProgress(player) < 100) Then	'Trying to weaken demon
		DemonCheck(2)
		Exit Sub
	End If
	if (hotProgress(player) = 20)	Then							'Searching for the Control Box?
		BoxCheck(2)													'Check / flag box for this location
		Exit Sub
	End If
	if (Mode(player) = 7) Then										'Are we in Ghost Photo Hunt?
		photoCheck(2)
		Exit Sub
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then		'Theater Ghost?		
		TheaterPlay(0)												'Incorrect shot, ghost will bitch!
		Exit Sub
	End If
	AddScore(50000)													'50k points up the center to make shot satisfying!
	playSFX 2, "E", "Z", 1 + random(3), 225							'Default Thunder sound!	
	lightningStart(Int(5998/CycleAdjuster)-1)						'Lightning FX	
End Sub

sub checkRoll()															'Check GLIR rollovers for completion
	'Set GLIR lights to what they should be
	laneChange()
	'playSFX(0, 'F', '1', 'J', 200)										'Negative rollover sound with BEEPS
	'playSFX(0, 'F', '1', 'K', 200)										'Negative rollover sound NO BEEPS	
	if (Mode(player) = 7) Then											'Are we IN a photo hunt?
'		if (rollOvers(player) = B11111111) Then
		If ((rollOvers(player) AND 255) = 255) Then
			AddScore(100000)											'100k points for spelling it during Photo Hunt!
			rollOvers(player) = 0										'Clear rollovers
			blink(52)													'Blink GLIR for a bit
			blink(53)
			blink(54)
			blink(55)
			displayTimerCheck(89999)									'Check if anything was running, set new value
			playSFX 2, "F", "1", "M", 201								'Modified version of the "X More to Light Photo Hunt" sound
		End If
		Exit Sub														'OK, don't get to do anything else!
	End If
'	if (GLIR(player) > 0 and rollOvers(player) = B11111111) Then		'GLIR spelled, not triggered yet?
	if (GLIR(player) > 0 AND ((rollOvers(player) AND 255) = 255)) Then
		if (GLIRlit(player) = 0) Then									'Haven't earned a Photo Hunt yet?
			GLIR(player) = GLIR(player) - 1								'Decrease spell counter
			if (GLIR(player) = 0) Then									'Did we spell GLIR enough times?
				if (GLIRneeded(player) < 9) Then
					GLIRneeded(player) = GLIRneeded(player) + 1			'Increase target #	needed, max is 9		
				End If
				GLIR(player) = GLIRneeded(player)						'Set counter to new target #
				GLIRlit(player) = 1										'Flag set - can be started!			
				rollOvers(player) = 0									'Clear rollovers
				blink(52)												'Blink GLIR for a bit
				blink(53)
				blink(54)
				blink(55)
				displayTimerCheck(89999)								'Check if anything was running, set new value				
				AddScore(20000)
				'Can it be started, or must we wait?
				if (Mode(player) = 0 and Advance_Enable = 1) Then		'Able to start a mode?		
					playSFX 0, "F", "1", 65 + random(4), 200			'"Photo Hunt is Lit!" prompt. Higher priority, will override normal rollover sound
					video "F", "1", "A", 1, 45, 200						'GLIR, photo hunt is lit!							
					showScoopLights()									'Update scoop lights	
					animatePF 30, 14, 0									'GLIR whoosh animation	
				Else													'Have to wait until mode is over?
					playSFX 0, "F", "1", 69 + random(4), 200			'Ghost Locating Infrared Ready!					
					video "F", "1", "B", allowSmall, 45, 200			'Photo Hunt ready after mode ends					
				End If
			Else														'Reset GLIR lights, prompt how many more spells to light PHOTO HUNT
				playSFX 2, "F", "1", "I", 200							'Need to spell it again sound FX		
				video "F", "S", GLIR(player), allowSmall, 37, 200		'SPELL GLIR X MORE TIMES TO LIGHT PHOTO HUNT
				AddScore(20000)
				rollOvers(player) = 0									'Clear rollovers
				blink(52)												'Blink GLIR for a bit
				blink(53)
				blink(54)
				blink(55)
				displayTimerCheck(89999)								'Properly end anything that may already be using the timer	
			End If
		Else															'If we already lit Photo Hunt, just award points
			playSFX 2, "F", "1", "M", 200								'Modified version of the "X More to Light Photo Hunt" sound	
			AddScore(20000)
			rollOvers(player) = 0										'Clear rollovers
			blink(52)													'Blink GLIR for a bit
			blink(53)
			blink(54)
			blink(55)
			displayTimerCheck(89999)									'Properly end anything that may already be using the timer	
		End If
	Else
';		video "F", "X", 64 + (rollOvers(player) AND 15), allowSmall, 0, 200		'Show what letters we have earned thus far (whenever a rollover is hit, even if hit already)
	End If
End Sub

sub laneChange()									'Changes lighted lanes when flippers pressed
	if ((rollOvers(player) AND 136) = 136) Then		'B10001000) Then			'G lit?
		light 52, 7
	Else
		light 52, 0
	End If
	if ((rollOvers(player) AND 68) = 68) Then			'B01000100) Then			'L lit?
		light 53, 7
	Else
		light 53, 0
	End If
	if ((rollOvers(player) AND 34) = 34) Then			'& B00100010) Then			'I lit?
		light 54, 7
	Else
		light 54, 0
	End If
	if ((rollOvers(player) AND 17) = 17) Then			'B00010001) Then			'R lit?
		light 55, 7
	Else
		light 55, 0
	End If
	if ((orb(player) AND 36) = 36) Then				'B00100100) Then				'O lit?
		light 32, 7
	Else
		light 32, 0
	End If
	if ((orb(player) AND 18) = 18) Then				'B00010010) Then				'R lit?
		light 33, 7
	Else
		light 33, 0
	End If
	if ((orb(player) AND 9) = 9) Then					'B00001001) Then				'B lit?
		light 34, 7
	Else
		light 34, 0
	End If		
End Sub

sub checkModePost()					'After a mode is over, check to see if we need to do anything
	doorLogic()								'Figure out what to do with the door
	checkRoll()								'See if we enabled GLIR Ghost Photo Hunt during that mode
	elevatorLogic()							'Did the mode move the elevator? Re-enable it and lock lights
	targetLogic(1)							'Where the Ghost Targets should be, up or down. In most cases, also see if we should reset Minion
	popLogic(0)								'Figure out what mode the Pops should be in
End Sub

sub checkOrb(videoYes)												'See if ORB has been completed					
	if ((orb(player) AND 63) <> 63) Then							'Not all ORB lanes complete?
		if (videoYes) Then
			playSFX 1, "O", "R", random(2) + 65, 100				'The orb that will repopulate the Earth. Nobody knows how it works. Only that it does.	
			video "O", "R", 64 + (orb(player) AND 7), allowSmall, 40, 250		'Play video of what IS lit. Lower than skill shot priority
		End If
		laneChange()
	Else															'All lit? Advance multipler!
		if (bonusMultiplier < 1) Then								'9 is the limit
			bonusMultiplier = 1										'If for some reason it's ZERO, make sure it's at least 1
		End If
		bonusMultiplier = bonusMultiplier + 1						'Increase the Bonus Multipler
		if (bonusMultiplier = 10) Then								'9 is the limit
			bonusMultiplier = 9
		End If
		playSFX 1, "O", "R", "C", 110								'Rollover + WIN sound! (Slightly higher priority)
		video "O", "R", bonusMultiplier, allowSmall, 60, 250 	'Play OR ASCII 48 + multipler (2-9 ASCII)		
		blink(32)													'Blink ORB
		blink(33)
		blink(34)
		displayTimerCheck(44999)									'Properly end anything that may already be using the timer			
		orb(player) = 0												'Clear player's ORB variable so it can be reset even during the flash
	End If
End Sub

sub comboCheck(whichShot)
	comboVideoFlag = 0												'The default is a standard combo. If we're not in a mode, and that shot advance is complete, it'll do Ghost Catch Combo instead
	if (comboTimer > 0 and comboShot = whichShot) Then				'Did we hit the Combo?
		comboVideoFlag = 1											'Default is a normal combo, but check if it isn't
		if (Advance_Enable) Then									'Not in a mode? See if this shot advance has been completed yet
			Select Case whichShot
				case 0:												'Left orbit, and Prison is complete?
					if ((ModeWon(player) AND 64) = 64) Then
						comboVideoFlag = 0
						video "C", "G", "A", allowSmall OR noEntryFlush OR noExitFlush, 12, 255	'Left net catch					
					End If
				case 1:
					if ((ModeWon(player) AND 2) = 2) Then				'Door VUK, and Hospital is complete?
						comboVideoFlag = 0
						video "C", "G", "A", allowSmall OR noEntryFlush OR noExitFlush, 12, 255	'Left net catch					
					End If
				case 2:
						'comboVideoFlag = 0
						'video('C', 'G', 'A' + random(2), allowSmall, 0, 255)	'Left or right net catch						
				case 3:
					if ((ModeWon(player) AND 32) = 32) Then				'Hotel path, and hotel complete?
						comboVideoFlag = 0
						video "C", "G", "B", allowSmall OR noEntryFlush OR noExitFlush, 12, 255	'Right net catch					
					End If
				case 4:
					if ((ModeWon(player) AND 4) = 4) Then				'Theater jump, and Theater complete?
						comboVideoFlag = 0
						video "C", "G", "B", allowSmall OR noEntryFlush OR noExitFlush, 12, 255	'Right net catch					
					End If
			End Select
		End If
		if (comboVideoFlag) Then											'Default combo?
			if (whichShot = 2) Then										'Combo up middle? Make sure it won't override Pop Graphics and mess up numbers or progress bars
				comboVideoFlag = 0										'Allow pops video to override Combo indicator
			End If
			videoCombo "C", "O", comboCount, allowSmall OR noEntryFlush OR noExitFlush, 10, 255	'Combo Video (1x to 5x)
			playSFX 1, "C", "O", comboCount, 200					'Combo sound FX	
			if (comboCount = 9) Then										'Max combo? Double points, reset # combos
				AddScore((comboCount * comboScore) * 2)
				comboCount = 1
			Else														'Normal points, increase combos
				AddScore(comboCount * comboScore)
				comboCount = comboCount + 1
			End If						
		Else
			killQ()			
			videoQ "C", "G", comboCount, allowSmall OR noEntryFlush OR noExitFlush, 12, 255		'Enqueue Combo X Indicator (1 to 9) to appear after Net Catch
			playSFX 1, "C", "C", 65 + random(10), 200					'Net whoosh + scream Combo sound FX
			if (comboCount = 9) Then										'Max combo? Double points, reset # combos
				AddScore((comboCount * comboScore) * 2)
				comboCount = 1
			Else														'Normal points, increase combos
				AddScore(comboCount * comboScore)
				comboCount = comboCount + 1
			End If
		End If
		light photoLights(comboShot), 0							'Turn that light off			
	End If
End Sub

sub comboKill()
	Dim X
	if (comboTimer) Then
		for x=0 To 5
			light photoLights(x), 0				'Turn off the 6 camera positions	
		Next
		light photoLights(comboShot), 0			'Turn off existing light, if any
		comboCount = 1							'Reset # of combos
		comboVideoFlag = 0						'Reset video flag
		comboShot = 99							'Set target shot to out of range	
		comboTimer = 0
	End If
End Sub

sub comboSet(whichShot, howMuchTime)			'Sets the next combo shot, and how much time you get for it
	if (comboEnable = 0) Then
		Exit Sub
	End If
	if (comboTimer) Then
		if (tourLights(comboShot) = 0) Then		'If the previous Combo Icon isn't flashing for a Tour mode...
			light photoLights(comboShot), 0		'Turn off previous Combo Shot Lamp			
		End If
	End If
	comboTimer = howMuchTime					'Set timer. Default was 80000 cycles, about 3.5 seconds
	comboShot = whichShot						'Set location of what shot to hit for combo
	blink(photoLights(comboShot))				'Blink that light!
End Sub

Function countBalls()							'Counts the balls in the trough
	Dim x:x = 0									'Balls found
	Dim y
'	int xx = B00001000							'Bit to check
'	while (xx != B10000000) Then
'		if (xx & switches(7)) Then					'Is there a ball there?
'			x = '			x - 1								'Up the count
'		End If
'		xx <<= 1								'Bitshift to the left
'	End If
'	return x									'Return how many balls we found
	If Sw59=1 Then x = x + 1
	If Sw60=1 Then x = x + 1
	If Sw61=1 Then x = x + 1
	If Sw62=1 Then x = x + 1
	countBalls = x
End Function

Function countGhosts()							'Returns how many Ghost Bosses have been beaten
	Dim howMany:howMany = 0
	Dim bitChecker:bitChecker = 65
	Dim x
	for x=0 To 5
		if ((ModeWon(player) AND bitChecker) = bitChecker) Then
			howMany = howMany + 1
		End If
'		bitChecker >>= 1
		bitChecker = ShiftRight(bitChecker, 1)
	Next
	CountGhosts = howMany
'	return howMany
End Function

'Functions for Demon Battle Wizard Mode 10........................
sub DemonLock1()											'Wizard Mode Started!
	AddScore(advanceScore)
	comboKill()
	storeLamp(player)										'Store and clear lamps
	allLamp(0)
	minionEnd(0)
	DoorSet DoorClosed, 500									'Close the door slowly
	trapDoor = 1											'Flag that ball should be trapped behind door	
	TargetTimerSet cycleSecond, TargetDown, 250				'Put the targets down
	ElevatorSet hellDown, 100				 				'Send Hellavator to 1st Floor.
	light 41, 0
	updateRollovers()										'Update ORB and GLIR
	if (wiki(player) < 255) Then
		pulse(0)
	Else
		light 0, 7
	End If
	if (tech(player) < 255) Then
		pulse(1)
	Else
		light 1, 7
	End If	
	if (psychic(player) < 255) Then
		pulse(51)
	Else
		if (scoringTimer) Then								'Double scoring active so the light blinks	
			blink(51)	
		Else
			light 51, 7										'Completed, so it's solid			
		End If	
	End If
	'comboEnable = 0							'Combos during Control Box search would be confusing, so no	
	deProgress(player) = 2						'Starting to lock balls
	Advance_Enable = 0							'Mode started, disable advancement until we are done
	minionEnd(0)								'Disable Minion mode, even if it's in progress
	Mode(player) = 10							'Set DEMON mode ACTIVE for player	
	spiritGuideEnable(0)						'No spirit guide
	hellEnable(0)								'No hellavator
	light 24, 0									'Hellavator Call buttons OFF
	light 25, 0
	light 13, 7									'Fight Demon light SOLID!	
	blink(17)									'Flash ghost targets
	blink(18)
	blink(19)
	blink(63)									'Blink DEMON MODE light!
	GIpf(96)
'	playMusic "D", "1"							'Wind, rusty swing set
	musicplayer "bgout_D1.mp3"
	playSFX 0, "D", "A", 65 + random(3), 255	'Mode start dialog
	killQ()										'Disable any Enqueued videos
	video "D", "A", "A", allowSmall, 130, 255	'Ghost & Swing Set
	'numbers(0, numberScore OR 2, 0, 27, player)	'Put player score
	'videoQ('D', 'A', 'B', allowSmall OR loopVideo, 0, 255)	'Ghost & Swing Set, looping with Prompt
	customScore "D", "A", "B", allowSmall OR loopVideo, 130		'Ghost & Swing Set prompt loop
';	numbers(8, numberScore OR 2, 0, 27, player)					'Put player score	
	activeBalls = activeBalls - 1				'Remove a ball from being "Active"
	AutoPlunge(67500)							'Set flag to launch second ball		
	deProgress(player) = 2						'We've locked the first ball. Now shoot for GHOST
	dirtyPoolMode(0)							'Switching to manual
	videoModeCheck()
	loopCatch = catchBall						'Set flag that we want to catch the ball in the loop
	setCabModeFade 32, 0, 0, 350				'Set mode color to DIM RED
End Sub

sub DemonLock2()
	AddScore(advanceScore)
	dirtyPoolMode(0)							'Disable dirty pool check (since we DO want to trap the ball)
	comboKill()
	'MagnetSet(100)							'Hold the ball	
	'TargetTimerSet(5000, TargetUp, 1)		'Put the targets up quickly
	trapTargets = 1							'A ball is trapped on purpose!
	ElevatorSet hellUp, 300					'Send Hellavator UP
	blink(41)
	light 16, 0								'Turn off ghost lights
	light 17, 0	
	light 18, 0
	light 19, 0
	GIpf(32)
	modeTimer = Int(55000/cycleAdjuster)	'Set high so timer doesn't decrement much during video
	strobe 26, 6							'Strobe the HELLAVATOR shot
'	playMusic "D", "2"						'LOUDER Wind, rusty swing set
	musicplayer "bgout_D2.mp3"
	playSFX 0, "D", "B", 65 + random(3), 255		'Mode start dialog
	killQ()									'Disable any Enqueued videos
	video "D", "B", "A", allowSmall, 130, 255		'Ghost & Swing Set
	'videoQ('D', 'B', 'B', allowSmall OR loopVideo, 0, 255)	'Loop it!
	customScore "D", "B", "B", allowAll OR loopVideo, 85		'Ghost & Swing Set prompt loop
';	numbers(8, numberScore OR 2, 0, 27, player)					'Put player score
	activeBalls = activeBalls - 1			'Remove a ball from being "Active"
	AutoPlunge(67500)						'Set flag to launch second ball		
	deProgress(player) = 4					'We've locked the second ball. Now shoot for HELLAVATOR
	setCabModeFade 64, 0, 0, 500			'Set mode color to MEDIUM red
End Sub

sub DemonLock3()
	Dim X, Y
	killScoreNumbers()						'Don't leave score onscreen during demon intro
	comboKill()
	comboEnable = 0							'Combos during Demon Hunt would be confusing
	setGhostModeRGB 255, 0, 00				'Red demon ghost
	HellBall = 10							'Flag to say elevator is in transit
	ElevatorSet hellDown, 200				'Move elevator down
	light 41, 0
	DoorSet DoorOpen, 50					'Close the door
	light 26, 0								'Turn off strobing Hellavator lights
	light 13, 0								'Turn off FIGHT DEMON
	GIpf(0)
	x = random(3)							'Make sure audio and video match
	If x = 0 Then Y = 232
	If x = 1 Then Y = 255
	If x = 2 Then Y = 223
	killQ()									'Disable any Enqueued videos		
'	playMusic "D", "E"						'Until we get final music ready
	musicplayer "bgout_DE.mp3"
	playSFX 0, "D", "C", 65 + x, 255		'Demon start dialog
	video "D", "C", 65 + x, allowSmall, Y, 255
	customScore "D", "D", 76 + random(1), allowSmall OR loopVideo, 120		'Ghost & Swing Set prompt loop
';	numbers(8, numberScore OR 2, 0, 0, player)				'Put player score	
	numbers "", Ball, "", ""				'Ball # upper right
	activeBalls = activeBalls - 1			'Remove a ball from being "Active"
	deProgress(player) = 8					'Waits for tunnel ball to hit scoop. Then, IT'S ON!
End Sub

sub DemonStart()
	Dim X
	comboKill()
	modeTotal = 0							'Reset mode points
	x = 0
	for x=57 To 62							'Pulse the MODE LIGHTS to serve as Demon Health Bar
		pulse(x)
	Next
	AutoPlunge(90000)						'Set flag to launch 4th ball		
	LeftTimer = Int(85000/CycleAdjuster)	'Kick out the left VUK ball
	ScoopTime = Int(80000/CycleAdjuster)	'Kick out right scoop ball
	TargetTimerSet 80000, TargetDown, 1		'Put the targets down to release ball
	trapTargets = 0							'No balls are trapped
	trapDoor = 0
	dirtyPoolMode(1)						'In case a ball gets up there somehow
	for x=0 To 5
		photoLocation(x) = 0				'Clear Control Box locations	
		light photoLights(x), 0				'Turn off the 6 camera positions
		light photoLights(x) - photoStrobe(x), 0		'Turn off the Strobe
	Next
	photoCurrent = random(5)				'Random location, but NOT the scoop to start							
	photoLocation(photoCurrent) = 255																'Set which one has the DEMON SHOT
	'pulse(photoLights(photoCurrent))
	strobe photoLights(photoCurrent) - photoStrobe(photoCurrent), photoStrobe(photoCurrent) + 1		'Strobe as many under it as we can
	demonLife = 6							'How many hits are left to go. Win when this reaches 0
	modeTimer = Int(95000/CycleAdjuster)	'At the start, this times how long until Target bank goes back up (about one second after they go down)
	DoctorTimer = Int(80000/CycleAdjuster)	'How much time you've got before the shot moves. They move faster the more you collect
	activeBalls	= activeBalls + 3			'Add the balls we've just released. The one being autolaunched will make it 4
	multipleBalls = 1						'Brief ball saver
	saveTimer = Int(180000/CycleAdjuster)	'Manually set this high as we wait for balls
	spookCheck()							'Check what to do with Spook Light
	'blink(56)								'Blink the SPOOK AGAIN light
	deProgress(player) = 9					'DEMON BATTLE has begun! This will also cause the Left VUK ball to be kicked out (once leftTimer reaches 0)
	setCabModeFade 255, 0, 0, 200			'Red lightning
End Sub

sub DemonState()							'What sort of looping video the demon should be doing
	if (deProgress(player) = 20) Then				'Jackpots FTW?			
		if (activeBalls = 1) Then
			customScore "D", "Z", "Y", allowSmall OR loopVideo, 130		'Demon almost dead, Hit to Win!			
		Else
			customScore "D", "J", 5 + random(1), allowSmall OR loopVideo, 150		'Weak demon defense, left or right, Hit for JACKPOTS!
		End If
	Else
		customScore "D", "D", 76 + random(1), allowSmall OR loopVideo, 120			'Normal Demon Defense, left or right, plus Prompt
	End If
	'numbers(0, numberScore OR 2, 0, 0, player)				'Show small player's score in upper left corner of screen
End Sub

sub DemonMove()										'Change the demon's position
	Dim X
	x = 0
	for x=0 To 5
		photoLocation(x) = 0						'Clear Control Box locations	
		light photoLights(x), 0						'Turn off the 6 camera positions
		light photoLights(x) - photoStrobe(x), 0	'Turn off the Strobe
	Next
	x = photoCurrent								'Set x to current location, so WHILE LOOP will execute at least once
	Do while (x = photoCurrent)						'Don't select same location twice - so it always MOVES
		x = random(5)								'Don't select scoop			
	Loop
	photoCurrent = x									'Update current location		
	photoLocation(photoCurrent) = 255								'Set which one has the DEMON SHOT
	'pulse(photoLights(photoCurrent))
	strobe photoLights(photoCurrent) - photoStrobe(photoCurrent), photoStrobe(photoCurrent) + 1	'Strobe the shot!
End Sub

sub DemonCheck(whichSpot)
	killQ()											'A lot of queued videos going on, so clear it each time
	if (deProgress(player) = 20) Then
		AddScore(50000)								'Some points
		if (activeBalls = 1) Then					'Only score jackpots with 2 or more balls
			video "D", "Z", "X", allowSmall OR loopVideo, 130, 255	'Hit Demon To Win!
			numbers PlayerScore(player), "", "", ""	'Show small player's score in upper left corner of screen			
		Else
			video "D", "J", "0", 0, 45, 230			'Hit Demon for Jackpots! (lower priority than Jackpot Advance display)
			DemonState()			
		End If
		playSFX 2, "A", "Z", "Z", 255				'Whoosh!
		Exit Sub									'Don't do this other stuff	
	End If
	if (photoLocation(whichSpot) = 255) Then		'Did we hit the demon?
		ghostFlash(50)
		flashCab 255, 255, 255, 10					'Flash from black to Default Mode Color	
		AddScore(activeBalls * 1000000)				'The more balls, the more points
		light demonLife + 56, 0						'Turn OFF the current light
		demonLife = demonLife - 1					'Decrement
		if (demonLife = 0) Then						'Did we beat him?
			lightSpeed = 1							'Back to normal
			DemonDefeated()
		Else										'Normal whacking dialog
			playSFX 0, "D", demonLife, 65 + random(3), 255		'Random dialog, 3 for each of the hits (5-1)
			if (whichSpot < 3) Then
				video "D", "D", "J", noExitFlush, 44, 255		'Looks left
			Else
				video "D", "D", "K", noExitFlush, 44, 255		'Looks right
			End If
			videoQ "D", "D", demonLife, noExitFlush OR noEntryFlush, 45, 255
			DemonState()
			DemonMove()											'Change camera location
			DoctorTimer = DoctorTimer - Int(7000/CycleAdjuster)	'Targets move faster as you collect them
			modeTimer = DoctorTimer								'Reset timer before next move
			lightSpeed = lightSpeed + 1							'Increase!
		End If		
	Else														'Miss!
		playSFX 0, "G", "T", 65 + random(18), 255				'Taunt player
		video "D", "D", 69 + random(1), allowSmall OR noExitFlush, 60, 255		'Demon taunt (left or right)
		DemonState()
		AddScore(2500)											'A few points
		flashCab 16, 0, 0, 75									'Darker red flash
	End If
End Sub

sub DemonJackpot()
	killQ()
	ghostFlash(50)
	jackpotMultiplier = activeBalls
	AddScore(EVP_Jackpot(player) * jackpotMultiplier)			'The 'mo balls the 'mo betta!
	playSFX 0, "D", "J", 65 + random(7), 255					'Jackpot sounds!
	video "D", "J", activeBalls, noExitFlush, 50, 255			'Bonus based off how many balls we have
	showValue EVP_Jackpot(player) * jackpotMultiplier, 40, 1	'Show what jackpot value was
	DemonState()
	ghostAction = Int(20000/CycleAdjuster)						'Set WHACK routine.	
	lightningStart(Int(100000/CycleAdjuster))										'Demon lightning!
End Sub

sub DemonDefeated()												'After you hit the 6 strobing shots, Defenses are down and it's JACKPOT time!
	Dim X
	TargetTimerSet 10, TargetDown, 1							'Put the targets down to release ball	
	for x=0 To 5
		photoLocation(x) = 0									'Clear Control Box locations	
		light photoLights(x), 0									'Turn off the 6 camera positions
		light photoLights(x) - photoStrobe(x), 0				'Turn off the Strobe
	Next
	deProgress(player) = 20										'Now we're in BASH MODE!	
	strobe 17, 3												'Strobe targets and pulse JACKPOT
	pulse(16)
	light 63, 7													'DEMON BATTLE solid
	GIpf(224)												'Lights back on!
	killQ()
	playSFX 0, "D", "0", 65 + random(3), 255					'Defeat Dialog!
	video "D", "D", "0", noExitFlush, 60, 255					'Demon sad
	if (activeBalls = 1) Then
		'videoQ('D', 'Z', 'X', allowSmall OR loopVideo OR noEntryFlush, 0, 255)	'Demon almost dead, Hit to Win!	
		'numbers(0, numberScore OR 2, 0, 0, player)				'Show small player's score in upper left corner of screen	
		customScore "D", "Z", "X", allowSmall OR loopVideo, 130		'Hit demon to WIN, then Demon on Ropes
		'DemonState()
		loopCatch = catchBall													'Set that we're ready to catch the final ball
		multipleBalls = 0
	Else
		'videoQ('D', 'J', '0', noEntryFlush OR noExitFlush, 0, 255)				'Hit Demon for Jackpots!	
		DemonState()
		multipleBalls = 1												'When MB starts, you get ballSave amount of time to loose balls and get them back
	End If	
	dirtyPoolMode(0)							'Disable dirty pool check (since we DO want to trap the ball)		
'	playMusic "G", "S"						'One more time!
	musicplayer "bgout_GS.mp3"
	ballSave()														'That is, Ball Save only times out, it isn't disabled via the first ball lost	
End Sub

sub DemonWin()
	DOF 134, 2
	killQ()
	videoPriority(0)
	ghostFlash(50)
	lightningStart(1)
	AddScore(EVP_Jackpot(player) * (ballsPerGame - ball))		'Extra points if you complete this on balls 1 or 2
	if (scoringTimer) Then										'Done?
		scoreMultiplier = 1										'Multiplier done
		scoringTimer = 0										'Reset timer
		animatePF 0, 0, 0										'Kill animations
		light 51, 7												'Light Psychic solid (done)
	End If
	killScoreNumbers()											'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()		
	video "D", "0", "1", 0, 513, 255							'Demon Death + End Credits	
	allLamp(0)
	light 63, 7													'All lights off but DEMON BATTLE solid
	ghostAction = Int(20000/CycleAdjuster)						'Set WHACK routine.	
	musicVolume(0) = 80											'Temp volume increase
	musicVolume(1) = 80
	volumeSFX 3, musicVolume(0), musicVolume(1)
	setCabModeFade defaultR, defaultG, defaultB, 2000			'Reset to default color	
'	playMusic "T", "E"											'Until we get final music ready	
	musicplayer "bgout_TE.mp3"
	TargetSet(TargetUp)											'Trap ball using targets
	deProgress(player) = 50										'Flag that mode is won!
	animatePF 179, 10, 1										'Center explode!	
	modeTimer = Int(300000/CycleAdjuster)
End Sub

sub DemonFailLock()												'What happens if you fail while trying to lock the 3 balls
	loopCatch = 0												'Not trying to catch the ball
	killTimer(0)
'	Coil(LeftVUK, vukPower)										'Kick out Ball 1
	VUKKicker KiDoor, vukPower
	TargetSet(TargetDown)										'Release Ball 2
	trapTargets = 0												'No balls are supposed to be trapped now
	trapDoor = 0
	DemonFail()	
End Sub

sub DemonFailBattle()											'What happens if you fail while trying to clear the shots and get to demon
	multipleBalls = 0
	TargetSet(TargetDown)										'Make sure balls don't get trapped
	lightningKill()
	DemonFail()
End Sub

sub DemonFail()													'General fail conditions for Demon Mode
	modeTimer = 0
	lightSpeed = 1												'Set this back to normal
	killNumbers()
	setGhostModeRGB 0, 0, 0										'Turn off the Red Ghost
	setCabModeFade defaultR, defaultG, defaultB, 100			'Reset cabinet color
	comboEnable = 1												'Allow combos
	loadLamp(player)
	spiritGuideEnable(1)										'Allow Spirit Guide again
	light 16, 0													'Turn off Ghost lights
	light 17, 0
	light 18, 0
	light 19, 0
	light 26, 0													'Turn off the strobing Hellavator lights
	light 63, 0													'Haven't won it yet, turn it off
	killScoreNumbers()											'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	ghostMove 90, 10											'Turn ghost back to center
	ghostLook = 1												'Ghost will now look around again.
	ghostAction = 0
	deProgress(player) = 1										'You can restart this mode as many times as you want
	blink(13)													'Blink the light to restart
	Mode(player) = 0											'Set mode active to None
	Advance_Enable = 1											'Allow other modes to be started
	doorLogic()													'Figure out what to do with the door
	checkRoll()													'See if we enabled GLIR Ghost Photo Hunt during that mode
	elevatorLogic()												'Did the mode move the elevator? Re-enable it and lock lights
	popLogic(0)													'Figure out what mode the Pops should be in	
End Sub
'END DEMON BATTLE FUNCTIONS

sub demonQualify()												'Check this after every mode, to see if we are ready to light WIZARD MODE DEMON BATTLE
	'All bosses beaten, MB started once, 3 minions defeated, and Photo Hunt completed once?
	'if (hitsTolight player) > 1 and minionsBeat(player) > minionMB1 Then
	if (ModeWon(player) = 126 and photosNeeded(player) > 3 and hitsTolight(player) > 1 and minionsBeat(player) > minionMB1) Then
		deProgress(player) = 1									'TEST DEMON MODE READY TO START
		blink(13)												'BLINK LIGHT
		doorLogic()
		videoSFX "D", "0", "0", allowSmall, 0, 255, 0, "A", "Z", 90 + random(2), 255
	End If
End Sub

sub doGhostActions()
	if (ghostAction < Int(10001/CycleAdjuster)+1) Then				'Guarding door?
		ghostAction = ghostAction - 1
		if (ghostAction = Int(5000/CycleAdjuster)) Then
			ghostMove 5, 50
		End If
		if (ghostAction = 1) Then
			ghostMove 15, 50
			ghostAction = Int(10000/CycleAdjuster)			
		End If
		Exit Sub
	End If
	if (ghostAction > Int(10000/CycleAdjuster) and ghostAction < Int(20001/CycleAdjuster)+1) Then				'Hit condition?
		ghostAction = ghostAction - 1
		if (ghostAction = Int(19999/CycleAdjuster)) Then
			ghostMove 130, 5
		End If
		if (ghostAction = Int(18000/CycleAdjuster)) Then
			ghostMove 110, 5
		End If
		if (ghostAction = Int(17000/CycleAdjuster)) Then
			ghostMove 120, 5
		End If
		if (ghostAction = Int(16000/CycleAdjuster)) Then
			ghostMove 80, 5
		End If
		if (ghostAction = Int(15000/CycleAdjuster)) Then
			ghostMove 70, 5
		End If
		if (ghostAction = Int(14000/CycleAdjuster)) Then
			ghostMove 80, 5
		End If
		if (ghostAction = Int(12000/CycleAdjuster)) Then
			ghostMove 75, 5
		End If
		if (ghostAction = Int(10001/CycleAdjuster)+1) Then
			ghostMove 90, 5
			ghostAction = 0
		End If
		Exit Sub
	End If
	if (ghostAction > Int(39999/CycleAdjuster) and ghostAction < Int(10000/CycleAdjuster)) Then				'War Fort Ball Hold?
		ghostAction = ghostAction - 1

		if (ghostAction = Int(99999/CycleAdjuster)) Then
			ghostMove 110, 5
		End If
		if (ghostAction = Int(97999/CycleAdjuster)) Then
			ghostMove 70, 5
		End If
		if (ghostAction = Int(95999/CycleAdjuster)) Then
			ghostMove 110, 5
		End If
		if (ghostAction = Int(93999/CycleAdjuster)) Then
			ghostMove 90, 5
		End If		
		if (ghostAction = Int(76000/CycleAdjuster)) Then
			ghostMove 170, 20
		End If		
		if (ghostAction = Int(70000/CycleAdjuster)) Then
			ghostMove 60, 2
		End If
		if (ghostAction = Int(54000/CycleAdjuster)) Then
			ghostMove 90, 100
			ghostLook = 1						'Allow ghost to look around again	
			if (goldHits > 9 and goldHits < 100) Then					'Still collecting gold?
				ghostAction = Int(209999/CycleAdjuster)
			Else
				ghostAction = Int(319999/CycleAdjuster)								'Else, normal pose			
			End If
		End If		
	End If
	if (ghostAction > Int(100000/CycleAdjuster) and ghostAction < Int(150001/CycleAdjuster)+1) Then			'Holding Ball?
		ghostAction = ghostAction - 1
		if (ghostAction = Int(125000/CycleAdjuster)) Then
			ghostMove 65, 300
		End If
		if (ghostAction = Int(100010/CycleAdjuster)) Then
			ghostMove 115, 300
			ghostAction = Int(150000/CycleAdjuster)			
		End If
		Exit Sub
	End If	
	if (ghostAction > Int(150000/CycleAdjuster) and ghostAction < Int(200000/CycleAdjuster)) Then			'Sexy Dance?
		ghostAction = ghostAction - 1
		if (ghostAction = Int(199990/CycleAdjuster)) Then
			ghostMove 75, 100
		End If
		if (ghostAction = Int(180000/CycleAdjuster)) Then
			ghostMove 105, 50		
		End If
		if (ghostAction = Int(160001/CycleAdjuster)+1) Then
			ghostMove 90, 100	
			ghostAction = 0
		End If		
		Exit Sub
	End If	
	if (ghostAction > Int(210000/CycleAdjuster) and ghostAction < Int(230000/CycleAdjuster)) Then			'Ghost hit, leading into Guarding Door?
		ghostAction = ghostAction - 1
		Select Case ghostAction									'Turn off the lights when we hit them
			case Int(229900/CycleAdjuster):
				ghostMove 140, 2
			case Int(220000/CycleAdjuster):
				ghostMove 125, 50	
			case Int(218000/CycleAdjuster):
				ghostMove 120, 50
			case Int(214000/CycleAdjuster):
				ghostMove 115, 50
			case Int(213000/CycleAdjuster):
				ghostMove 125, 50
			case Int(211000/CycleAdjuster):											'This will lead into ghost turning back towards door
				ghostMove 115, 50
		End Select
		Exit Sub
	End If	
	if (ghostAction > Int(199999/CycleAdjuster) and ghostAction < Int(210001/CycleAdjuster)+1) Then			'Guarding door?
		ghostAction = ghostAction - 1
		if (ghostAction = Int(205000/CycleAdjuster)) Then
			ghostMove 5, 50
		End If
		if (ghostAction = Int(200001/CycleAdjuster)+1) Then
			ghostMove 15, 50
			ghostAction = Int(209999/CycleAdjuster)			
		End If
		Exit Sub
	End If
	if (ghostAction > Int(300000/CycleAdjuster) and ghostAction < Int(320001/CycleAdjuster)+1) Then			'Guarding Front?
		ghostAction = ghostAction - 1
		if (ghostAction = Int(310000/CycleAdjuster)+1) Then
			ghostMove 80, 50
		End If
		if (ghostAction = Int(300001/CycleAdjuster)+1) Then
			ghostMove 100, 50
			ghostAction = Int(319999/CycleAdjuster)			
		End If
		Exit Sub
	End If	
	if (ghostAction > Int(320000/CycleAdjuster) and ghostAction < Int(340000/CycleAdjuster)) Then			'Ghost hit, leading into Guarding Front?
		ghostAction = ghostAction - 1
		Select Case ghostAction							'Turn off the lights when we hit them
			case Int(339990/CycleAdjuster)
				ghostMove 160, 2
			case Int(334000/CycleAdjuster)
				ghostMove 150, 50	
			case Int(330000/CycleAdjuster)
				ghostMove 135, 50
			case Int(328000/CycleAdjuster)
				ghostMove 126, 50
			case Int(326000/CycleAdjuster)
				ghostMove 134, 50
			case Int(324000/CycleAdjuster)											'This will lead into ghost guarding the front
				ghostMove 127, 50
			case Int(322000/CycleAdjuster)											'This will lead into ghost guarding the front
				ghostMove 133, 50
		End Select
		Exit Sub
	End If		
	if (ghostAction > Int(399999/CycleAdjuster) and ghostAction < Int(499999/CycleAdjuster)) Then			'Minion animations?
		ghostAction = ghostAction - 1
		Select Case (ghostAction)									'Turn off the lights when we hit them
			case Int(468000/CycleAdjuster)
				ghostMove 120, 2
			case Int(466000/CycleAdjuster)
				ghostMove 60, 2	
			case Int(464000/CycleAdjuster)
				ghostMove 110, 5
			case Int(462000/CycleAdjuster)
				ghostMove 70, 5
			case Int(460000/CycleAdjuster)
				ghostMove 90, 5					'Centers
				if (minion(player) <> 10 and minion(player) <> 100) Then	'Minion over? End motion
					ghostAction = 0
				End If			
			case Int(450000/CycleAdjuster)							'This will lead into ghost guarding the front
				ghostMove 60, 150
			case Int(425000/CycleAdjuster)											
				ghostMove 120, 150
			case Int(400000/CycleAdjuster)											
				ghostAction = Int(450005/CycleAdjuster)+5
		End Select
		Exit Sub	
	End If
	if (ghostAction > Int(499999/CycleAdjuster) and ghostAction < Int(510000/CycleAdjuster)) Then
		ghostAction = ghostAction - 1
		Select Case ghostAction									'Turn off the lights when we hit them
			case Int(509990/CycleAdjuster)
				ghostMove 70 + (random(2) * 40), 5	'Either goto 70 or 110
			case Int(500000/CycleAdjuster)
				ghostMove 90, 150
				ghostAction = 0				'Cancel motion
		End Select
		Exit Sub			
	End If
End Sub

sub doorDo()												'When ball goes past Door Opto
	ghostLooking(15)
	if (extraLit(player)) Then								'Extra ball available?
		Exit Sub											'Door should be open, let the ball past
	End If													'Other door opto functions won't work until EB is collected (such as Advance Hospital or Confederate Gold)
	if (Mode(player) = 6 and convictState = 1) Then			'Fighting warden ghost and the door is closed?
		video "P", "8", "Y", allowSmall, 45, 255			'Door opens, Prompt for next shot
		playSFX 0, "P", "Y", 65 + random(4), 255
		AddScore(50000)										'50k for hitting the door
		convictState = 2									'Advance state
		DoorSet DoorOpen, 5									'Open door quickly!
		light 14, 0											'Turn off Camera blink
		strobe 8, 7											'Strobe the entire shot
		modeTimer = 0										'Reset this so prompt won't happen for a bit
		Exit Sub											'Abort out so default doesn't occur
	End If
	if ((hotProgress(player) > 29 and hotProgress(player) < 40) and convictState = 1) Then		'Evicting ghosts from the Hotel? Uses same variables as Prison Free Ghost
		video "L", "E", "1", allowSmall, 62, 255			'Door opens, Prompt for next shot
		playSFX 0, "L", "E", 1 + random(4), 255				'Knocking sound, random prompt
		AddScore(50000)										'50k for hitting the door
		convictState = 2									'Advance state
		DoorSet DoorOpen, 200								'Open door slowly
		light 14, 0											'Turn off Camera blink
		strobe 8, 7											'Strobe the entire shot
		Exit Sub											'Abort out so default doesn't occur
	End If
	if (fortProgress(player) > 59 and fortProgress(player) < 100) Then													'Fighting the War Fort?
		if (goldHits < 10) Then								'Not already collecting gold (10) or disabled (100)?
			WarGoldStart()	
		End If
		if (goldHits = 100) Then							'Already beat it?
			killQ()											'Disable any Enqueued videos		
			video "W", "G", "I", allowSmall, 60, 255		'No more gold!
			playSFX 0, "W", "G", random(4), 255				'Ghost lamenting the lack of gold			
'			DoorLocation = DoorClosed - 10					'Put it to be slightly opened
'			myservo(DoorServo).write(DoorLocation)			'Send that value to the servo
			PrDoor.ObjRotZ = DoorClosed + 10
			DoorSet DoorClosed, 1000						'Then make it go back closed			
		End If
		Exit Sub
	End If
	if (Advance_Enable = 1 and hosProgress(player) = 0) Then		 ' and deProgress(player) = 0) Then								'Are we elible to advance modes?
		if (theProgress(player) < 3 or theProgress(player) = 100) Then		'Theater isn't lit, or has been won already? 
			HospitalAdvance()								'Advance Hospital
			Exit Sub
		End If
	End If
	if (hosProgress(player) > 5 and hosProgress(player) < 9 and hosTrapCheck = 0) Then								'Are we trying to bash open the door, and there wasn't a ball search error?
		if (DoctorState = 0) Then							'Evil doctor ghost NOT distracted?
			Dim X
'			DoorSet DoorClosed + 5, 5						'Open door slighty
			PrDoor.ObjRotZ = DoorClosed + 5
			modeTimer = Int(8000/CycleAdjuster)				'Set timer to close it back up
			AddScore(10000)									'A few points
			video "H", "5", "A", allowSmall, 99, 200		'Same video for every clip
			x = random(1)									'50/50 chance it plays CLUNK or CLUNK + Voice Prompt
			if (x) Then
				playSFX 0, "H", "5", "Z", 255				'Play taunts H5A-H5D
			Else
				playSFX 0, "H", "5", random(8) + 65, 255	'Play taunts H5A-H5D
			End If			
		End If
		if (DoctorState = 1) Then							'Evil doctor ghost IS distracted?			
			AddScore(countSeconds * 50000)
			PrDoor.ObjRotZ = DoorClosed + 20				'Open door slighty
			modeTimer = Int(8000/CycleAdjuster)				'Set timer to close it back up
			killTimer(0)									'Kill the timer
			DoctorState = 0									'Reset flag
			doctorHits = 0									'Reset this so there's a prompt next time you hit ghost
			light 8, 0										'Disable strobe state
			ghostAction = Int(5500/CycleAdjuster)			'Re-enable ghost jitters
			if (hosProgress(player) = 6) Then				'First bash?
				video "H", "6", "J", allowSmall, 60, 200	'Ball hits door, ghost blocks, 2 HITS TO GO!
				playSFX 0, "H", hosProgress(player), 74 + random(3), 255
				customScore "H", "7", "E", allowAll OR loopVideo, 30	'Shoot Ghost 2 shots to go!
			End If
			if (hosProgress(player) = 7) Then				'Second bash?
				video "H", "6", "K", allowSmall, 60, 200	'Ball hits door, ghost blocks, 1 HIT TO GO!
				playSFX 0, "H", hosProgress(player), 74 + random(3), 255
				customScore "H", "7", "F", allowAll OR loopVideo, 30	'Shoot Ghost 2 shots to go!
			End If
			hosProgress(player) = hosProgress(player) + 1	'Advance progress. If Hospital Logic section sees this as a "9", we start Multiball Battle!
			if (hosProgress(player) < 9) Then				'Not the winning hit yet?	
				pulse(17)
				pulse(18)
				pulse(19)
				light hosProgress(player) + 1, 7			'Use number to indicate progress
			End If	
		End If
		Exit Sub
	End If
	'Default Action if no other logic. This actually should never happen, but make a CLUNK sound and slightly move door if it does
	if (DoorLocation = DoorClosed) Then						'Door closed when we hit it?
'		DoorLocation = DoorClosed - 10						'Put it to be slightly opened
'		myservo(DoorServo).write(DoorLocation)				'Send that value to the servo
		PrDoor.ObjRotZ = DoorClosed + 10
		DoorSet DoorClosed, 1000							'Then make it go back closed
		playSFX 2, "H", "0", "0", 100						'Door clunking sound
		AddScore(5000)										'Some points
	End If
End Sub

sub doorLogic()														'What to do with the door at the end of a mode
	DoorSet DoorOpen, 5												'The default is OPEN, but...
	if (deProgress(player) = 1) Then								'Eligible to start Demon Battle?
		DoorSet DoorOpen, 5
		Exit Sub
	End If
	if (extraLit(player)) Then										'Extra ball available?
		DoorSet DoorOpen, 5											'Make sure the door is open
		pulse(15)													'Pulse the light
		Exit Sub													'Return out of this, EB always top priority
	Else
		light 15, 0													'If not lit, turn light off (it may have been collected during a mode, thus old lights)
	End If
	if (Mode(player) = 6 or (hotProgress(player) > 29 and hotProgress(player) < 40)) Then	'A mode one-two punching the door?
		if (convictState = 1) Then
			DoorSet DoorClosed, 5
		Else
			DoorSet DoorOpen, 5
		End If
		Exit Sub
	End If
	if (hellMB or minionMB) Then									'Hell MB isn't in progress?
		DoorSet DoorOpen, 25										'Make sure the door is open		
		Exit Sub
	End If
	if (Mode(player) = 4) Then
		if (goldHits = 10) Then										'Stealing confederate gold?
			DoorSet DoorOpen, 5										'Make sure the door is open	
		End If
		if (goldHits < 10 or goldHits = 100 or fortProgress(player) = 59) Then							'Trying to open door, or Gold already complete, or mode just started?
			DoorSet DoorClosed, 5
		End If	
		Exit Sub
	End If
	if (theProgress(player) = 3) Then								'Eligible to start Theater?
		DoorSet DoorOpen, 5											'Make sure door is open	
		Exit Sub
	End If
	if (deProgress(player) = 2) Then								'On second shot of Demon Battle?
		DoorSet DoorClosed, 5										'Door should be closed
		Exit Sub
	End If
	if (hosProgress(player) > 5 and hosProgress(player) < 9) Then	'Are we trying to save our friend in hospital?
		DoorSet DoorClosed, 5										'Door needs to be closed!
		Exit Sub
	End If
	if (hosProgress(player) > 0 and hosProgress(player) < 4) Then	'Advancing Hospital mode?	
		DoorSet DoorOpen, 5											'Make sure door is open	
		if (hosProgress(player) = 3) Then							'Was doctor mode ready?	
			pulse(11)												'Turn off that light for now	
		End If
	End If
	if (hosProgress(player) = 99) Then								'Did we FAIL doctor mode?	
		pulse(11)													'Re-lite it
		DoorSet DoorOpen, 500										'Open door SLOWLY		
		hosProgress(player) = 3										'Set progress to re-enable state
		Exit Sub
	End If
	if (hosProgress(player) = 100 and Advance_Enable = 1) Then		'If hospital mode has been won, keep door open for combos (and to reduce wear)
		DoorSet DoorOpen, 100
		Exit Sub
	End If
	if (hosProgress(player) = 0 and Mode(player) = 0) Then			'Gotta start Hospital? Door is closed, unless we're in a mode in which case, don't touch the door don't touch the door!
		DoorSet DoorClosed, 5
	End If
End Sub

sub DoorSet(dTarget, dSpeed)
	If dSpeed > 300 Then dSpeed = 300								'EP- My error trap
	dSpeed = (((DoorFast)/300) * (300 - dspeed)) + DoorSlow			'EP- I've got to adjust for BH's code
'	if (dSpeed < 1) Then											'Error trap
'		dSpeed = 100												'EP- But, this virtual door moves different than a physical one
'	End If
	if (dTarget > DoorOpen) Then									'Error trap
		dTarget = DoorOpen
	End If
	if (dTarget < DoorClosed) Then									'Error trap
		dTarget = DoorClosed
	End If
	DoorSpeed = dSpeed												'How fast to move
	DoorTarget = dTarget											'Where to move to.
	DoorTimer = 0													'Reset cycle timer
	WaDoor.TimerEnabled = 1
End Sub

sub dirtyPoolCheck()									'If a mode ends with a condition where a ball could be stuck under the ghost, call this routine to check and clear it
	if (dirtyPoolChecker = 0) Then						'We WANT to trap balls behind targets?
		Exit Sub										'Abort!
	End If
	'Serial.println("Dirty Pool Check...")
	dirtyPoolTimer = 1									'Set the timer for Dirty Pool Logic
End Sub

sub dirtyPoolMode(whatToDo)						'0 = Ignore balls trapped behind targets (some modes) 1 = Check for them and remove (normal)
	dirtyPoolChecker = whatToDo
End Sub

sub dirtyPoolLogic()
	dirtyPoolTimer = dirtyPoolTimer + 1
	if (dirtyPoolTimer = 10) Then									'First event?
		 MagnetSet(75)												'Trigger the Magnet
	End If
	if (dirtyPoolTimer > 9 and dirtyPoolTimer < Int(50000/CycleAdjuster)) Then		' and barProgress(player) != 65) Then	'Wait until 20k mark before checking if Bar Trap
		if (Sw24 = 1) Then											'As SOON as something blocks the opto, trigger Dirty Pool
			magFlag = 0												'Disable the Magnet Hold Pulses
			playSFX 0, "D", "P", random(5), 255						'Dirty Pool Prompt!
			AddScore(250000)
			TargetSet(TargetDown)									'Put targets down
			TargetTimerSet 15000, TargetUp, 5	 					'Set timer to put them back up after ball rolls out. It'll check again once they're up
			dirtyPoolTimer = Int(100000/CycleAdjuster)				'Set this to Ball Detected Countdown, so Detected only occurs once		
		End If
	End If
	if (dirtyPoolTimer = Int(20000/CycleAdjuster))	Then			'Enough time to grab ball?
		dirtyPoolTimer = 0											'OK to proceed!
	End If
	if (dirtyPoolTimer = Int(120000/CycleAdjuster)) Then			'If a ball was found, wait about a second and make sure it's rolled out
		dirtyPoolTimer = 0						'Disable it for now, but targets will check again once they're up
	End If
End Sub

sub displayTimerCheck(newTimerValue)								'Call this before using the Display Timer. If something already running, this ends it properly
	newTimerValue = Int(newTimerValue/CycleAdjuster)
	if (newTimerValue) Then											'Setting a new value?	
		if (displayTimer > 0 and displayTimer < Int(45000/CycleAdjuster)) Then			'Was ORB flashing?
'			if (orb(player) & B00100100) Then	'O lit?
			If ((orb(player) AND 36) = 36) Then
				light 32, 7
			Else
				light 32, 0
			End If
			if ((orb(player) And 18) = 18) Then	'R lit?
				light 33, 7
			Else
				light 33, 0
			End If
			if ((orb(player) AND 9) = 9) Then		'B lit?
				light 34, 7
			Else
				light 34, 0
			End If
		End If
		if (displayTimer > Int(45000/CycleAdjuster) and displayTimer < Int(90000/CycleAdjuster)) Then	'Was GLIR flashing as this started?	
			'Set GLIR lights to what they should be
			if ((rollOvers(player) And 136) = 136) Then	'G lit?
				light 52, 7
			Else
				light 52, 0
			End If
			if ((rollOvers(player) AND 68) = 68) Then	'L lit?
				light 53, 7
			Else
				light 53, 0
			End If
			if ((rollOvers(player) AND 34) = 34) Then	'I lit?
				light 54, 7
			Else
				light 54, 0
			End If
			if ((rollOvers(player) AND 17) = 17) Then	'R lit?
				light 55, 7
			Else
				light 55, 0
			End If
		End If	
		displayTimer = newTimerValue			
	Else													'we're just updating both ORB and GLIR
		updateRollovers()
	End If
End Sub

sub Drain(drainType)						'What happens when you drain. Check for ball save, extra balls, DRAIN if neither
	if (lightningGo) Then				'So it won't get stuck on, even during a ball save
		lightningEnd(10)
	End If
	if (tiltFlag) Then					'Were we in a Tilt state when ball drained?
		if (hosProgress(player) > 5 and hosProgress(player) < 90) Then					'Doctor MB, but a Tilt?
			HospitalFail()																'Mode FAIL! Gotta start over
		End If	
		if (barProgress(player) > 59 and barProgress(player) < 100) Then					'Bar MB, but a Tilt?
			BarFail()																	'Mode FAIL! Gotta start over
		End If			
	Else													'Normal drain?
		drainSwitch = drainSwitch + 1									'Increment the drain switch #
		'Serial.print("+Drain Switch = ")
		'Serial.println(drainSwitch, DEC)
		if (saveTimer or (scoreBall = 0 and zeroPointBall = 1)) Then	'Ball save active, or it was a Zero Point Ball with save enabled?
			activeBalls = activeBalls - 1								'Have to subtract one here before AutoPlunge will ADD one.
			AutoPlunge(autoPlungeFast)						'Auto-plunge a freebie ball. Give it a little time in case trough empty
			video "E", "B", "Z", allowSmall, 37, 255		'Keep shooting!
			if (activeBalls = 0) Then							'No MB or anything going on?
				playSFX 0, "Y", "A", random(4), 255			'"Don't Touch that Dial!" (high priority)
			Else
				playSFX 0, "Y", "A", random(4), 150	'"Don't Touch that Dial!" (medium priority)
			End If
			if (multiBall = 0 and multipleBalls = 0) Then	'In multiball, ball save doesn't reset on a ball loss, rather there's an overall "grace period" like AFM the best game EVAH
				saveTimer = 0								'No more ball save for you!
				light 56, 0								'Turn off Spook Again				
			End If
			Exit Sub											'Leave this function
		End If		
		activeBalls = activeBalls - 1				'Decrement # of balls on the playfield
		if (activeBalls = 1) Then			'Down to our last ball?
			'These first 3 modes can stack with normal Hellavator MB. If a Hell MB was active, these win conditions will terminate the multiball
			if (Mode(player) = 1 and hosProgress(player) = 10) Then			'Bashing the Doctor Ghost?		
				HospitalWin()												'Mode complete, reset stuff.			
				Exit Sub
			End If
			if (Mode(player) = 1 and hosProgress(player) > 4 and hosProgress(player) < 9 and hosTrapCheck = 1) Then			'Bashing the Doctor Ghost?		
				HospitalRestart()											'Allow quick restart!			
				Exit Sub
			End If
			if (hotProgress(player) = 30 or hotProgress(player) = 35) Then	'Battling Hotel Ghost?		
				HotelWin()													'Mode complete, reset stuff.		
				Exit Sub
			End If
			if (barProgress(player) = 80) Then								'Ghost Whore battle?		
				BarWin()													'Mode complete, reset stuff.		
				Exit Sub
			End If		
			'If none of those modes were active (finishing up) then we end Multiball normally
			if (multiBall) Then												'If we weren't in a Ghost mode, then end like normal multiball
				multiBallEnd(0)												'We need to check this first because if stacked with other modes, things might not get cleared properly
				Exit Sub
			End If
			if (priProgress(player) > 9 and priProgress(player) < 20) Then		'Last ball, and you didn't free all 3?
				PrisonDrainCheck(0)		
				Exit Sub
			End If		
			if (priProgress(player) = 20) Then								'Last ball and you released all 3?
				PrisonDrainCheck(1)
				Exit Sub
			End If
			if (deProgress(player) = 20) Then									'Down to our last ball? This is the WIN CONDITION!
				killQ()
				video "D", "Z", "X", allowSmall OR loopVideo, 130, 255		'Hit Demon To Win!	text, then Demon on Ropes
'				numbers(0, numberScore OR 2, 0, 0, player)					'Show small player's score in upper left corner of screen
				numbers PlayerScore(player), "", "", ""
				loopCatch = catchBall										'Set that we're ready to catch the final ball
			End If
			playSFX 2, "Q", "Z", "Z", 200									'Else, negative sound!			
			Exit Sub														'Return to main loop
		End If
		if (activeBalls > 1) Then			'2 or more balls still active?
			if (Mode(player) = 6) Then	'Saving our friends?		
				PrisonDrainCheck(1)
				Exit Sub
			End If
			if (deProgress(player) > 9 and deProgress(player) < 100) Then			'Prompt with current multiplier
				killQ()
				video "P", "7", 67 + activeBalls, noExitFlush, 39, 255	
				DemonState()				
			End If		
			playSFX 2, "Q", "Z", "Z", 200						'Negative sound!
			Exit Sub						'Return to main loop
		End If
		if (activeBalls < 1) Then			'Don't let it go below zero
			activeBalls = 0
		End If
		'This has to be after the ActiveBalls check else we'll get a false FAIL condition if MB stacked on Ghost Whore and any balls lost
		if (barProgress(player) > 59 and barProgress(player) < 80) Then	'Haven't saved our friend from Ghost Whore yet?		
			if (BarFail() = 1) Then										'One more chance?
				Exit Sub													'Asub drain!
			End If 'Else, drain!
			activeBalls = 0
		End If	
		if (hosProgress(player) > 4 and hosProgress(player) < 9) Then	'Were we trying to save our friend from Ghost Doctor?
			if (HospitalFail() = 1) Then										'Able to restart it? (returns a 1)
				if (hosTrapCheck = 1) Then									'Were we looking for a ball?
					hosTrapCheck = 0										'Clear flag, do NOT re-add ball #
				Else
					activeBalls = activeBalls + 1							'Count the ball we've freed
				End If
				Exit Sub		
			End If 'Else, drain!
			activeBalls = 0
		End If	
	End If
	if (restartTimer) Then
		restartKill 0, 0			'In case one is active
	End If
	DrainPre()						'Things to do before we start the drain (mostly end active modes)
	killQ()						'Disable any Enqueued videos	
	killNumbers()					'Disable Numbers display
	comboKill()					'Disable any Combos
	AutoEnable = 0					'Disable flippers
	LeftFlipper.RotateToStart
	PlaySound SoundFX("FlipperDown"),0,1,-0.1,0.25
	DOF 101, 0
	RightFlipper.RotateToStart
	PlaySound SoundFX("FlipperDown"),0,1,0.1,0.25
	DOF 102, 0
	run = 1						'Set condition so player can launch the new ball
	callHits = 0					'How many times you've hit Call this ball (resets per player)	
	'rollOvers(player) = 0			'Clear rollovers
	light 52, 0
	light 53, 0
	light 54, 0
	light 55, 0
	ghostMove 90, 10				'Center ghost
	storeLamp(player)											'Store current player's lamps in memory. This is after we've done the Pre-drain stuff, so it's relevant for the next ball
	animatePF 149, 30, 0			'Ball fade animation. Will also cancel out any other animations in progress
	'allLamp(0)													'Turn off all lamps
	scoringTimer = 0											'Terminate any scoring timers
	scoreMultiplier = 1										'Reset multipler
	skip = 0
	drainTries = 0
	if (tiltFlag = 0) Then										'Don't award bonus, or show video / music if tilt
		drainTimer = Int(70005/CycleAdjuster)+5				 '160005							'Timer for Drain events. System keeps running.	
		EOBnumbers 0, areaProgress(player) * 13370			'Send AREA PROGRESS
		EOBnumbers 1, EVP_Total(player) * 250000						'Send EVPS COLLECTED
		EOBnumbers 2, photosTaken(player) * 500250			'Send PHOTOS TAKEN
		EOBnumbers 3, ghostsDefeated(player) * 1000000		'Send GHOSTS DEFEATED
		bonus = (areaProgress(player) * 13370) + (EVP_Total(player) * 250000) + (photosTaken(player) * 500250) + (ghostsDefeated(player) * 1000000)
		bonus = bonus * bonusMultiplier								'Multiply it		
		bonusMultiplier = 1									'Reset multiplier (it's per ball so don't need unique variable per player)		
		'EOBnumbers(4, bonus)									'Send TOTAL BONUS
		AddScore(bonus)										'Before switching players, increase score by bonus	
'		playMusic "B", "F"															'Shorter music	
		musicplayer "bgout_BF.mp3"
		VideoEOB "E", "B", 64 + ball, noEntryFlush OR allowLarge, 131, 255					'Play EOB video
		EOBnumbers 4, bonus										'Send TOTAL BONUS	
		GIpf(192)
		setCabColor 64, 64, 64, 200
	Else
		drainTimer = Int(25000/cycleAdjuster)										'Faster cycle, and skips the audio callout
		cabColor 255, 0, 0, 255, 0, 0
		doRGB()												'Set cab immediately to RED!
		drainTries = 0											'We haven't tried to kick the drain yet
	End If	
End Sub

sub DrainLogic()												'DRAIN functions are in-line. This is the logic that executes
	if (Switch(63) and kickTimer = 0) Then	'Something in the drain, and we didn't just try to kick it?
		drainClear()						'Unload the drain!		
	End If
	if (drainTimer = Int(69000/CycleAdjuster)) Then '178500) Then
		GIpf(128)		
		allLamp(0)							'Turn off all lamps after the Fade Animation has a chance to start
		EOBnumbers 4, bonus					'Send TOTAL BONUS again to make sure EOB numbers are enabled (instead of seeing blank scores)		
	End If	
	if (drainTimer = Int(68000/CycleAdjuster)) Then '177000) Then
		GIpf(0)		
	End If
	if (drainTimer = Int(65000/CycleAdjuster)) Then '177000) Then
		playSFX 0, "Y", 66 + random(3), random(10), 150		'Ball drain quote from 3 Team Members (not super high priority so won't override other dialog)		
	End If
	if ((SwRFlip=1) or (SwLFlip=1)) Then		'Skipping past?
		if (countBalls() = 4 and drainTimer > Int(18000/cycleAdjuster) and drainTimer < Int(69000/cycleAdjuster)) Then	'All balls accounted for, and eligible part of sequence?
			video "E", "C", 64 + ball, 0, 54, 255		'Play ending flash (will also kill EOB numbers))
			drainTimer = Int(17000/CycleAdjuster)		'Speed this up	
'			playMusic "B", "G"							'Ending beat
			musicplayer "bgout_BG.mp3"
			'video('E', 'C', '@' + ball, 0, 0, 255)		'Play ending flash
		End If
	End If	
	if (drainTimer = Int(10001/CycleAdjuster)+1 and countBalls() < 4) Then 'Don't continue until all balls are accounted for
		drainTimer = Int(10100/CycleAdjuster)
	End If
	if (drainTimer = Int(10000/cycleAdjuster)) Then			'Last thing we do... (sped up a little)
		drainTimer = 0
		drainSwitch = 63				'Manually set the drain switch number (will goto 62 once new ball loads)
		GIpf(0)				'In case we somehow got past it
		ballsPlayed = ballsPlayed - 1				'Increase counter
		videoPriority(0)				'Erase video priority
		if (extraBalls) Then				'Have more than zero extra balls?
			extraBalls = extraBalls - 1			'Subtract one...
			loadLamp(player)			'We basically treat this as a drain, but we don't advance the ball # or player #
			playSFX 0, "Y", "A", 5 + random(3), 255 'Same ghost hunter shoots again!
			if (numPlayers = 1) Then
				video "S", "A", "C", 0, 29, 255
			Else
				video "S", "A", "D", 0, 29, 255
			End If
			skillShotNew(0)				'Set up a Skill Shot, but show video AFTER the Shoot Again prompt (0)	
		Else							'No extra balls? Advance balls or player # as normal
			if (numPlayers > 1) Then		'More than 1 player?
				player = player + 1				'Advance which player is up
				if (player > numPlayers) Then  'Past the end?
					player = 1				'Back to Player 1
					ball = ball + 1				'Went through all 4 players, increment ball #
				End If			
				loadLamp(player)			'Load new player's lamps into memory
			End If
			if (numPlayers = 1) Then	
				ball = ball + 1
				loadLamp(player)
			End If
			if (ball < ballsPerGame) Then				'Game not over yet?
				video "K", "9", "9", 0, 1, 255	'STATIC transition
				skillShotNew(1)					'Set up a Skill Shot!
			End If
		End If
		tiltFlag = 0					'Reset this in case we got here from a tilt. We do it here so it won't prevent Match music from playing
		if (ball = ballsPerGame) Then		'Game over? (man?)
		'stopMusic()	'Eventually we'll let GAME OVER handle this
			NoGame2()							'EP- Had to put this here to manually "continue" the main loop since while loops don't work here.
		Exit Sub			'Exit routine, since game is over
		Else
			Update(0)				'Make sure we're not in Attract Mode.			
'			playMusic "L", "1"
			musicplayer "bgout_L1.mp3"
		End If
		'EVP_Total = 0				'Reset single-ball bonuses
		popCount = 0					'Pops per ball (takes 10 to get an EVP)
		GIpf(224)
		sweetJumpBonus = 0				'Reset score (hitting it adds value)
		sweetJump = 0					'Reset video/SFX counter		
		Advance_Enable = 1
		Mode(player) = 0
		badExit = 0					'Haven't gone in VUK yet
		tiltCounter = 0				'Reset to zero		
		comboKill()
		slingCount = 0
		showProgress 0, player		'Set progress lights		
		minionDamage = 1				'Default damage
		checkModePost()				'Set things on the playfield for the new current player
		AutoEnable = 255				'Enable flippers
		ghostLook = 0
		dirtyPoolMode(1)				'Check for Dirty Pool Balls		
		spiritGuideEnable(1)			'Mode 0, it can always be lit
		hellEnable(1)					'Enable the Hellavator on this ball
		GLIRenable(1)					'In case you tilted with GLIR disabled	
		'orb(player) = 0				'Clear player's ORB variable so it can be reset
		scoreBall = 0					'No points scored on this ball as yet
		tiltTimer = 0
		comboEnable = 1												'OK combo all you want
		loadBall()						'Load a new ball	
		flashCab 255, 255, 255, 10			'Flash from black to Default Mode Color		
	End If
End Sub

sub DrainPre()													'Mode-specific things to do at the start of a drain
	if (minion(player)) Then											'In a Minion Battle?
		minionEnd(3)												'End it with drain flag, but allow a restart
	End If
	if (Mode(player) = 7 or Mode(player) = 99) Then					'Were we in GHOST PHOTO HUNT?
		photoFail(1)												'Fail flag 0, meaning failed because of drain
	End If
	if (tiltFlag) Then
		if (multiBall) Then											'We need to do this AFTER photo hunt clear in case they were stacked
			multiBallEnd(0)
		End If	
	End If
	if (hotProgress(player) > 9 and hotProgress(player) < 30) Then		'In hotel mode, but not before multiball? OR, if tilt, also kill it
		HotelFail()
	End If
	if (hotProgress(player) > 29 and hotProgress(player) < 40 and tiltFlag) Then		'Did we tilt out during Multiball?
		HotelFail()
	End If
	if (fortProgress(player) > 49 and fortProgress(player) < 100) Then	'Were we in War Fort mode?
		WarFail()													'End that mode
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then	'Doing the THEATER PLAY?
		TheaterFail(1)												'End that mode, with no animations (1)
	End If
	if (deProgress(player) > 1 and deProgress(player) < 9) Then		'Locking balls to start Demon Battle?
		DemonFailLock()											'Do fail condition for that
	End If
	if (deProgress(player) > 8 and deProgress(player) < 21) Then		'Lost all balls before defeating demon?
		DemonFailBattle()											'Do fail condition for that
	End If
	if (priProgress(player) > 9 and priProgress(player) < 99) Then		'Trying to free friends, or bash the Prison Warden?
		PrisonFail()	
	End If	
End Sub

sub drainClear()
	kickPulse = 0							'Make sure PWM timer is reset
	kickTimer = Int(8000/CycleAdjuster)		'WAS 10,000 'Wait 10k cycles, then kick hold for 10k cycles
	kickFlag = 1							'Set flag that ball is being kicked from the drain
End Sub

sub ElevatorSet(dTarget, dSpeed)
	dSpeed = ((((ElevatorFast - ElevatorSlow)/600) * (600 - dSpeed))/10) + ElevatorSlow
	HellSpeed = dSpeed													'How fast to move
	HellTarget = dTarget												'Where to move to.
	HellTimer = 0														'Reset cycle timer
	TiElevator.Enabled = 1
End Sub

sub elevatorLogic()
	hellEnable(1)								'Losing a mode re-enables the Hellavator Lock
	if (hotProgress(player) = 3) Then				'Able to start Hotel mode?
		ElevatorSet hellUp, 200				'Move the elevator into 2nd floor position	
		blink(41)								'HELL FLASHER
		light 26, 7							'Re-light advance numbers
		light 27, 7
		light 28, 7
		pulse(29)								'Pulse Hotel Ghost
		light 24, 0							'Call button lights off
		light 25, 0
		Exit Sub
	End If
	'Default state is hellavator down, Lock enabled
	ElevatorSet hellDown, 100 				'Send Hellavator to 1st Floor.
	light 41, 0								'Flasher OFF			
	blink(24)									'Blink the UP button							
	light 25, 7								'DOWN is solid, since elevator is there
	light 30, 0								'Turn OFF "Lock" light	
End Sub

sub Enable()											'The kernel calls this every cycle to enable the Watchdog Timer on the solenoids
'	digitalWrite(solenable, 1)							'Pulse enable line
'	digitalWrite(solenable, 0)  						'EP- Since the only place this is called is in at StartGame() and in the mainenance menu I'm gonna do what I think need to be done
	Dim X
	WaSlingLeft.SlingShotThreshold = 5
	WaSlingRight.SlingShotThreshold = 5
	AutoEnable = 255
	For Each X in Bumpers
		X.Force = 15
	Next
End Sub

sub evpPops()
	popCount  = popCount  +  1								'Increase pop counter
	popToggle()								'Toggle left and right
	'Pops will only show video/numbers if we haven't JUST shot up center
	if (popCount < 10) Then														'Not enough for an EVP, normal pop
		EVP_Jackpot(player)  = EVP_Jackpot(player)  +  2030
		sendJackpot(0)					'Send jackpot value to score #0
		AddScore(2030 * popCount)
		video "E", "J", popCount, allowLarge, 30, 239							'Jackpot display, with EVP progress bar
';		numbersPriority(6, 1, 255, 12, EVP_Jackpot player), 239					'Send numbers with current EVP value, and it will only display on videos matching this priority
		numbers "", "", EVP_Jackpot(player), EVP_Jackpot(player)
		stereoSFX 1, "E", "V", 1 + random (3), 200, leftVolume, rightVolume
	End If
	if (popCount = 10) Then														'Enough for an EVP?
		leftVolume = 100														'Center the volume
		rightVolume = 100
		AddScore(5000 * popCount)
		popCount = 0															'Reset pop total
		EVP_Total(player)  = EVP_Total(player)  +  1													'Increase our EVP's this ball
		EVP_Jackpot(player)  = EVP_Jackpot(player)  +  11110
		sendJackpot(0)															'Send current jackpot value to score #0
		AddScore(EVP_Total(player) * 11110)									'Ten times the points for an EVP!	
		if (EVP_Total(player) < EVP_EBtarget(player)) Then							'haven't gotten enough for an EVP yet?
';			video "E", "V", "3", allowSmall, 0, 241										'Higher priority so Score doesn't override
';			numbersPriority(5, 2, 20, 26, EVP_EBtarget(player) - EVP_Total player), 241 	'A small number to show how many EVP's we've gotten in total										
			playSFX 1, "E", "V", 65 + random(8), 201										'Higher priority so regular pops don't override EVP voice				
		Else																				'Guess we got enough for an Extra Ball!
			Select Case allowExtraBalls														'Give whatever the settings allow for Extra Ball
				case 1:														'Allow Extra Balls?
					video "S", "A", "B", allowSmall, 64, 255
					playSFX 0, "A", "X", 67 + random(2), 255				'EXTRA BALL!
					extraBalls  = extraBalls  +  1							'Player gets another ball!
					spookCheck()											'See what to do with the Spook Again light
				case 2:
					video "S", "A", "E", allowAll, 45, 255
';					numbers 5, numberFlash OR 1, 255, 11, 100000			'100k
					numbers "", "", "100,000", "100,000"
					playSFX 0, "Q", "C", 65 + random(5), 250				'Sound + Heather compliment
					AddScore(100000)
				case 3:
					video "S", "A", "E", allowAll, 45, 255
';					numbers 5, numberFlash OR 1, 255, 11, 500000			'500k
					numbers "", "", "500,000", "500,000"
					playSFX 0, "Q", "C", 65 + random(5), 250				'Sound + Heather compliment
					AddScore(500000)
				case 4:
					video "S", "A", "E", allowAll, 45, 255
';					numbers 5, numberFlash OR 1, 255, 11, 1000000			'1 mil
					numbers "", "", "1,000,000", "1,000,000"
					playSFX 0, "Q", "C", 65 + random(5), 250				'Sound + Heather compliment
					AddScore(1000000)
			End Select
			EVP_Total(player) = 0											'Reset counter
			EVP_EBtarget(player)  = EVP_EBtarget(player)  +  EVP_EBsetting							'Increase # you need for EB
			if (EVP_EBtarget(player) > 99) Then
				EVP_EBtarget(player) = 99
			End If
		End If
	End If
End Sub

sub extraBallLight(queueYes)
	if (allowExtraBalls) Then
		extraLit(player)  = extraLit(player)  +  1						'Increase available EB collects
		pulse(15)									'Pulse the light
		DoorSet DoorOpen, 5						'Open the door
		if (queueYes = 1) Then							'Flag that we should prompt EB is lit?
			video "S", "A", "A", allowSmall, 45, 255						'Extra Ball is lit
			playSFX 0, "A", "X", 65 + random(2), 150					'Low priority voice call "Extra Ball is Lit!"	
		End If	
		if (queueYes = 2) Then
			videoSFX "S", "A", "A", allowSmall, 45, 255, 0, "A", "X", 65 + random(2), 255	'"Extra Ball is Lit!"				
		End If
	End If
End Sub

sub extraBallCollect()
	lightningStart(1)
	extraLit(player)  = extraLit(player)  -  1						'Subtract ball lit
	extraBallGet  = extraBallGet  +  1							'Increment master counter
	Select Case (allowExtraBalls)
		case 1:											'Allow Extra Balls?
			video "S", "A", "B", allowSmall, 64, 255
			playSFX 0, "A", "X", 67 + random(2), 255	'EXTRA BALL!
			extraBalls  = extraBalls  +  1							'Player gets another ball!
			spookCheck()								'See what to do with the Spook Again light
		case 2:
			video "S", "A", "E", allowAll, 45, 255
';			numbers 5, numberFlash OR 1, 255, 11, 100000			'100k
			numbers "", "", "100,000", "100,000"
			playSFX 0, "Q", "C", 65 + random(5), 250				'Sound + Heather compliment
			AddScore(100000)
		case 3:
			video "S", "A", "E", allowAll, 45, 255
';			numbers 5, numberFlash OR 1, 255, 11, 500000			'500k
			numbers "", "", "500,000", "500,000"
			playSFX 0, "Q", "C", 65 + random(5), 250				'Sound + Heather compliment
			AddScore(500000)
		case 4:
			video "S", "A", "E", allowAll, 45, 255
';			numbers 5, numberFlash OR 1, 255, 11, 1000000			'1 mil
			numbers "", "", "1,000,000", "1,000,000"
			playSFX 0, "Q", "C", 65 + random(5), 250				'Sound + Heather compliment
			AddScore(1000000)
	End Select
	if (extraLit(player) < 1) Then					'No more collects available?
		light 15, 0							'Turn off collect light
	End If	
	doorLogic()								'See what the door state should be now that EB was collected
End Sub

sub GameOver()
	Dim X, XX, Y, Z, ZZ, abortLoop, tempSort
	for x=0 To 7
		killTimer(x)								'Make sure all timers are dead. You never know.
	Next
	killScoreNumbers()
	playSFX 0, "A", "A", 65 + random(11), 255		'Ending quote
	abortLoop = 1									'How the sort loop knows when to move onto the next player
	tempSort = 0
	pPos(0) = 1										'Default values. When done, 0 = player # with highest score, 3 = player # with lowest score
	pPos(1) = 2
	pPos(2) = 3
	pPos(3) = 4
	if (numPlayers > 1) Then								'If there is more than 1 player...
		player = 0										'Set NO active player (so all scores appear same size during score entry)	
	Else
		player = 1
	End If
	ball = 0											'Disable BALL # from appearing
	Update(0)											'Update A/V with this info
	allLamp(0)											'Turn off all lamps
	for x=0 To 2									'Bubble sort the scores. It also sorts non-playing scores of 0
		if (playerScore(pPos(x + 1)) > playerScore(pPos(x + 0))) Then			
			tempSort = pPos(x + 0)
			pPos(x + 0) = pPos(x + 1)
			pPos(x + 1) = tempSort
		End If		
	Next
	Wall10.TimerEnabled = 1
End Sub

Sub Wall10_Timer()
	me.TimerEnabled = 0
	GameOver0()
End Sub

Dim HSCheck:HSCHeck = 1
Dim HSPlace:HSPlace = 0
Dim GotHS:GotHS = 2
Sub GameOver0()
	Dim X, XX, Y, Z, ZZ, abortLoop, tempSort
	If HSCheck < numPlayers+1 Then
		Debug.Print "Checking HS, cursor POS: " & cursorPOS
		for y=0 To 4																'Check this score against high scores 0 to 4
			Debug.Print "Player: " & cDbl(playerScore(HSCheck)) & " HS check: " & cDbl(highScores(y))
			if (cDbl(playerScore(HSCheck)) >= cDbl(highScores(y))) Then						'Did player beat this high score?	Equalling it will also bump it down a place
				Debug.Print "Score higher than " & y
'				playMusic "N", "E"												'Only play the music if a player got a high score
				musicplayer "bgout_NE.mp3"
				for z=4 To y+1 Step -1											'Shift scores down one space below new high score
					highScores(z) = highScores(z - 1)							
					topPlayers((z * 3) + 0) = topPlayers(((z - 1) * 3) + 0)
					topPlayers((z * 3) + 1) = topPlayers(((z - 1) * 3) + 1)
					topPlayers((z * 3) + 2) = topPlayers(((z - 1) * 3) + 2)
				Next
				for zz = 0 To 4													'Send the newly sorted top 5 scores from RAM to EEPROM
					setHighScore zz, highScores(zz), topPlayers((zz * 3) + 0), topPlayers((zz * 3) + 1), topPlayers((zz * 3) + 2)
				Next
				SaveValue "AMH", "HSPoints" & y, PlayerScore(HSCheck)
				SaveValue "AMH", "HSName" & y, ""
				HSPlace = y
				cursorPos = 0
				initials(0) = 95												'What player has entered (starts as empty spaces)
				initials(1) = 95
				initials(2) = 95
				GotHS = 1
				nameEntry HSCHeck, HSPlace										'Get initials from player, and show which place they got
				Exit For
			Else
				GotHS = 0
			End If
		Next
	End If
	If GotHS = 0 Then
		WaLeftSide1.TimerEnabled = 1
	End If	
End Sub

Sub GameOver1()
	
End Sub

Sub WaLeftSide1_Timer()
	Me.TimerEnabled = 0
	GameOver2()
End Sub

Dim tempcount:tempcount = 0
Sub GameOver2()
	Dim X, XX, Y, Z, ZZ, abortLoop, tempSort
	animatePF 0, 0, 0								'Turn off PF animations
	repeatMusic(0)									'Music will play and then terminate (disable auto looping)	
	if (allowMatch) Then
		Dim Match, MatchFlag, divider
		match = random(10)
		matchFlag = 0
		for x=1 To (numPlayers)														'Break player's scores down into 2 digit numbers for match
			SetScore(x)																'Send that player's score one more time before we "rip it up" for match math
			divider = 1000000000													'Divider starts at 1 billion
			for xx=0 To 7															'Seven places will get us the last 2 digits of a 10 digit score		
				if (playerScore(x) >= divider) Then
					playerScore(x) = playerScore(x) MOD divider
				End If
				divider = Divider / 10						
			Next
			if (playerScore(x) = (match * 10)) Then	'Did we match?	
				matchFlag  = matchFlag  +  1						'Count it up!
				matchGet  = matchGet  +  1						'Increase master counter
			End If
		Next
		'numbers 0, 8, 128, 0, 0					'Send numbers with current EVP value, and it will only display on videos matching this priority				
		video "N", "A", match, allowAll, 75, 255		'Match video of the random number we generated
';		numbers 0, 8, 128, 0, 0								'Show all scores for Match animation
		if (matchFlag) Then										'Does one of the player's scores match?
			credits  = credits  +  matchFlag								'Award a credit for each match!
'			playMusic "Z", "1"								'WIN music
			musicplayer "bgout_Z1.mp3"
		Else
			Debug.Print "failed"
'			playMusic "Z", "0"								'LOSE music
			musicplayer "bgout_Z0.mp3"
		End If
	Else
		video "N", "9", "9", 0, 45, 255					'Game Over Screen!
		stopMusic()
	End If
'	delay(250)											'EP- I'm assuming delay is a built-in function for the propeller?
	showScores = 1									'Now that there has been a game, set the flag to show last scores during attract mode
	startingAttract = lastGameScores									'Start Attract mode with last game's scores. When machine reset runs, it will send the data
	run = 0												'Reset run state for when we cycle back around.
	HSCheck = 1
	GotHS = 2
	inChar = 65
	Wall23.TimerEnabled = 1
End Sub

sub nameEntry(whichPlayer, whichPlace)							'EP- this is where name entry stuff would go
	If cursorPOS = 99 Then
		LoadHighScores()
		cursorPos = 50
		GotHS = 0
		HSCheck = HSCheck + 1
		Wall10.TimerEnabled = 1
		Exit Sub
	End If
	GotHS = 1
	videoPriority(0)											'Set low priority so Match will override Name Entry when we exit here
	If inChar = 65 Then
		DMDScene "", "Z<  " & "Player " & HSCheck & " " & Chr(inChar+1) & Chr(inChar+2) & Chr(inChar+3), 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "    " & Chr(inChar) & "        ", 15, 14, 16665, 14, 255
	ElseIf inChar = 66 Then
		DMDScene "", "< " & Chr(inChar-1) &  " " & "Player " & HSCheck & " " & Chr(inChar+1) & Chr(inChar+2) & Chr(inChar+3), 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "    " & Chr(inChar) & "        ", 15, 14, 16665, 14, 255
	ElseIf inChar = 89 Then
		DMDScene "", Chr(inChar-3) & Chr(inChar-2) & Chr(inChar-1) & " " & "Player " & HSCheck & " " & Chr(inChar+1) & "< ", 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "    " & Chr(inChar) & "        ", 15, 14, 16665, 14, 255
	ElseIf inChar = 90 Then
		DMDScene "", Chr(inChar-3) & Chr(inChar-2) & Chr(inChar-1) & " " & "Player " & HSCheck & " " & "< A", 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "    " & Chr(inChar) & "        ", 15, 14, 16665, 14, 255
	ElseIf inChar = 91 Then
		DMDScene "", "XYZ " & "Player " & HSCheck & " " & " AB", 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "    " & "<" & "        ", 15, 14, 16665, 14, 255
	elseIf inChar = 92 Then
		DMDScene "", "YZ< " & "Player " & HSCheck & " " & "ABC", 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "  " & "Space" & "      ", 15, 14, 16665, 14, 255
	Else
		DMDScene "", Chr(inChar-3) & Chr(inChar-2) & Chr(inChar-1) & " " & "Player " & HSCheck & " " & Chr(inChar+1) & Chr(inChar+2) & Chr(inChar+3), 15, Chr(Initials(0)) & Chr(Initials(1)) & Chr(Initials(2)) & "    " & Chr(inChar) & "        ", 15, 14, 16665, 14, 255
	End If
	modeTimer = Int(375000/cycleAdjuster)						'need to get out of initials screen after this time
	houseKeeping()
	modeTimer = modeTimer - 1								'Start countering down to bailing out of initials
End sub

Sub SendInitials(whichLetter, whichPlace)
	Dim tempinitials
	tempinitials = Chr(initials(0)) & Chr(initials(1)) & Chr(initials(2))
	SaveValue "AMH", "HSName" & whichPlace, tempinitials
End Sub

sub GLIRenable(enableOrNot)
	if (enableOrNot) Then							'MSB prevents start. So we clear it to allow Photo Hunt
		GLIRlit(player) = GLIRlit(player) AND 127
	Else
		GLIRlit(player) = GLIRlit(player) OR 128	'If we want to disable it, set MSB (probably just used for Minion MB
	End If
	showScoopLights()
End Sub

sub ghostColor(RedG, GreenG, BlueG)
	ghostRGB(0) = RedG
	ghostRGB(1) = GreenG
	ghostRGB(2) = BlueG
	doRGB()
End Sub

Sub ghostFlash(whatTime)
	If whatTime < 80 Then whatTime = 80
	Dim bulb
'**	FlGhostG.alpha = 125
'**	FlGhostR.alpha = 125
'**	FlGhostB.alpha = 125
'**	PrGhost.Image = "Ghost2"
	for each bulb in Ghost_RGB
		bulb.Color=RGB(255,255,255)
	next
	ghostFadeTimer = Int(whatTime/CycleAdjuster)
	ghostFadeAmount = Int(whatTime/CycleAdjuster)
End Sub

sub ghostLooking(whereTo)										'Ghost looks at a spot, gets bored, then turns back to center
	if (barProgress(player) = 60 and restartTimer = 0) Then		'Ghost waiting for your embrace, but we're not trying to jump back in for quick restart?
		if (whereTo <> 80 and whereTo <> 100) Then				'Don't put quotes on the sling hits
			playSFX 0, "B", "5", 65 + random(8), 255			'I'm over here baby!
			video "B", "5", "A", allowSmall, 54, 255			'Ghost talking video
			AddScore(5230)										'A few points
			ghostAction = Int(199999/CycleAdjuster)				'Ghost does "sexy, alluring" dance
		End If
		Exit Sub
	End If
	if (ghostLook = 1 or Advance_Enable = 0) Then			
		if (ghostAction = 0) Then								'No ghost action going on?
			ghostMove whereTo, 10								'Ghost looks wherever.
			ghostBored = Int(15000/CycleAdjuster) + Int(random(15000)/CycleAdjuster)		'Set bored timer.
		End If
	End If
End Sub

sub ghostSet(whereTo)											'Moves the ghost and sets that as his new position
'	GhostLocation = -(whereto - 90)								'Update location
'   myservo(GhostServo).write(GhostLocation) 					'Set servo
	GhostMove -(whereto-90), 100
End Sub

Sub GhostMove(whereto, whatspeed)							'EP- BH code is approx. +90 degrees from mine I could adjust the primitive but I don't wanna :-P
	whatspeed = (((GhostFast - GhostSlow)/700) * (700 - whatSpeed)) + GhostSlow		'EP- In the code, the higher the number, the slower it moves, so I have to compensate for that here
	GhostSpeed = whatspeed/4								'EP- This is an approximation of the speed difference
	GhostTarget = -(whereto - 90)							'EP- Also, +rotation for BH is CCW, and it's CW for me, I can't change that
	WaGhost.TimerEnabled = 1
End Sub

sub hellEnable(enableType)									'1 = You can lock balls in the Hellavator and move it 0 = You can't and Hellavator stays down
	if (enableType) Then									'Enable the Hellavator?
		hellLock(player) = 1								'Allow locks / stacking Hell MB
		if (hotProgress(player) <> 3) Then					'Only set these lights if Hotel Mode isn't ready to go (also uses hellavator)		
			if (HellSpeed)	Then							'In motion? Base this off where it's headed, not where it IS
				if (HellTarget = hellDown) Then				'Re-enable elevator call button & lights	
					blink(24)								'Blink the UP button							
					light 25, 7								'DOWN is solid, since elevator is there
					light 30, 0								'Turn OFF "Lock" light
					light 41, 0								'Flasher OFF
				End If
				if (HellTarget = hellUp) Then
					blink(25)								'Blink the DOWN button							
					light 24, 7								'UP is solid, since elevator is there
					pulse(30)								'LOCK is lit!
					blink(41)								'Turn on HELL FLASHER
				End If						
			Else											'Not in motion? Normal check
				if (HellLocation = hellDown) Then			'Re-enable elevator call button & lights	
					blink(24)								'Blink the UP button							
					light 25, 7								'DOWN is solid, since elevator is there
					light 30, 0								'Turn OFF "Lock" light
					light 41, 0								'Flasher OFF
				End If
				if (HellLocation = hellUp) Then
					blink(25)								'Blink the DOWN button							
					light 24, 7								'UP is solid, since elevator is there
					pulse(30)								'LOCK is lit!
					blink(41)								'Turn on HELL FLASHER
				End If			
			End If
		End If
	Else													'Disable it?
		hellLock(player) = 0
		ElevatorSet hellDown, 100 							'Send Hellavator to 1st Floor.
		light 41, 0											'Turn off HELL FLASHER
		light 24, 0											'Turn off both lights
		light 25, 0
		light 30, 0											'Lock is NOT lit			
	End If
End Sub

'FUNCTIONS FOR HOSPITAL MODE 1.................................
sub HospitalAdvance()										'Logic that runs as we advance Hospital Mode 1
	Dim X
	AddScore(advanceScore)
	flashCab 0, 255, 0, 100					'Flash the GHOST BOSS color	
	if (hosProgress(player) > 3) Then									'Has mode already started, or are we waiting for the door to close?
		Exit Sub														'I'm not even sure how this would happen with the ball lock, but who knows?	
	End If
	hosProgress(player)  = hosProgress(player)  +  1										'Normal advance		
	areaProgress(player)  = areaProgress(player)  +  1
	if (hosProgress(player) > 0 and hosProgress(player) < 4) Then				'First 3 advances?
		playSFX 0, "H", hosProgress(player), random(4) + 65, 255			'Play hxA-hxD.wav files
		pulse(hosProgress(player) + 8)											'Pulse next one	
		video "H", hosProgress(player), "A", allowSmall, 75, 200			'Play first 3 videos		
	End If
	if (hosProgress(player) < 4) Then											'Always fill lights and set door
		for x=0 To hosProgress(player)-1										'in case we did a Double Advance
			light x + 8, 7														'Completed lights to SOLID
			pulse(x + 9)														'Pulse the next light
		Next
		DoorSet DoorOpen, 300													'Set door to creak open, 25 cycles per position
	End If
	if (hosProgress(player) = 4) Then											'Mode start?
		Mode(player) = 1														'Set hospital mode ACTIVE for player 1.
		Advance_Enable = 0														'At this point we can't advance any other modes until Ghost is defeated or we loose.
		DoorSet DoorClosed, 1													'DoorSet DoorClosed, 50									'Shut door fast!
	End If
End Sub

sub HospitalStart()								'What happens when we shoot "Doctor Ghost" when lit
	Dim whichClip
	videoModeCheck()
	restartKill 1, 1							'In case we got the Restart
	comboKill()									'So combo lights don't appear after the mode
	storeLamp(player)							'Store the state of the Player's lamps
	allLamp(0)									'Turn off the lamps
	spiritGuideEnable(0)						'No spirit guide during Hospital
	modeTotal = 0								'Reset mode points	
	AddScore(startScore)
	minionEnd(0)								'Disable Minion mode, even if it's in progress
	setGhostModeRGB 0, 255, 0					'Green mode color
	setCabModeFade 0, 255, 0, 200				'Set mode color to GREEN, fade to that color
	popLogic(3)									'Set pops to EVP
	ghostLook = 0
	ghostBored = 0								'Prevents his look action from happening
	LeftTimer = 1								'Set this so it can't re-trigger
	if (countGhosts() = 5) Then					'Is this the last Boss Ghost to beat?
		blink(48)								'Blink that progress light
	End If	
	pulse(17)
	pulse(18)
	pulse(19)
	light 8, 0
	light 9, 0
	light 10, 0
	light 11, 0								'Turn off advance lights
	blink(57)									'Blink the HOSPITAL mode light
	tourReset(43)						'Tour: Left orbit, center shot, right orbit, scoop	
	hosProgress(player) = 6					'Set flag so mode only "starts" once
	killQ()													'Disable any Enqueued videos	
	whichClip = random(3) + 65								'Get the number first so they match ASCII A-C
	video "H", "4", whichClip, allowSmall, 135, 255				'Play hxA-hxD.wav files
	playSFX 0, "H", "4", whichClip, 255						'Play hxA-hxD.wav files
	customScore "H", "7", "D", allowAll OR loopVideo, 30	'Shoot Ghost custom score prompt
'	numbers 8, numberScore OR 2, 0, 0, player	'Show player's score in upper left corner
	numbers PlayerScore(Player), "", "", ""
'	numbers 10, 9, 88, 0, 0					'Ball # upper right
	numbers "", Ball, "", ""
	jackpotMultiplier = 1						'Reset this just in case
	ghostAction = Int(5100/CycleAdjuster)							'Set flag for him to jiggle near door
	TargetSet(TargetUp)						'Put targets UP!
	patientStage = 0							'What stage of Ghost Patient you're at
	patientsSaved = 0							'How many you saved, through Murder!
	DoctorState = 0												'Set ghost to start as Not Distracted
	modeTimer = 0													'Used to animate the door
	'BLINK GHOST LIGHTS SO WE KNOW TO HIT HIM!
'	playMusic "B", "1"											'Boss battle music!
	musicplayer "bgout_B1.mp3"
	activeBalls  = activeBalls  -  1												'Remove a ball from being "Active"
	AutoPlunge(100000)												'Set flag to launch second ball	
	hellEnable(1)
	showProgress 1, player					'Show the Main Progress lights
	DoorSet DoorClosed, 1
	trapDoor = 1								'Flag that ball should be trapped behind door
	hosTrapCheck = 0
	skip = 10
End Sub

sub HospitalLogic()									'Stuff that happens during Doctor Ghost Battle
	if (hosProgress(player) = 4) Then				'Mode just started, door closing?
'		if (DoorSpeed = 0) Then						'Did door stop moving yet? (Is it closed?)
		if WaDoor.TimerEnabled = 0 Then				'EP- Since the movement of the door isn't based on Doorspeed, it's based on the timer
			HospitalStart()							'"Officially" start mode
		End If
	End If
	if (hosProgress(player) > 5 and hosProgress(player) < 9) Then					'Are we trying to save our friend?
		if (DoctorState = 1 and popsTimer = 0) Then									'Ghost distracted?
			DoctorTimer  = DoctorTimer  +  1										'Increment timer
			if (DoctorTimer = (DoctorTarget / 2)) Then								'Only used for Seconds counter
				countSeconds  = countSeconds  -  1
';				numbers 0, numberStay OR 4, 0, 0, countSeconds - 1					'Update the Numbers Timer.
				if (countSeconds > 1 and countSeconds < 7) Then
					playSFX 2, "A", "M", 47 + countSeconds, 1						'Hurry-Up beep
				Else
					playSFX 2, "Y", "Z", "Y", 1										'Beeps
				End If
			End If
			if (DoctorTimer = DoctorTarget) Then									'Time to move? Ghost moves every other second
				DoctorTimer = 0														'Reset timer
				ghostMove GhostLocation - 11, 700									'Move ghost back towards door...
'				ghostMove -(PrGhost.ObjRotZ-90)-10, 700								'EP- I'm not sure why the previous doesn't work
				countSeconds  = countSeconds  -  1									'Subtract!
';				numbers 0, numberStay OR 4, 0, 0, countSeconds - 1					'Update the Numbers Timer.	
				if (countSeconds > 1 and countSeconds < 7) Then
					playSFX 2, "A", "M", 47 + countSeconds, 1						'Hurry-Up beep
				Else
					playSFX 2, "Y", "Z", "Y", 1										'Beeps
				End If
				if (GhostLocation <= (GhostDistracted - 10) AND GhostLocation >= (GhostDistracted - 19)) Then					'Did he move twice?
					playSFX 0, "H", hosProgress(player) + 48, 71 + random(3), 255	'Doctor dictates
					video "H", "6", "Z", allowSmall, 75, 200						'Video of dictating
				End If
				if (GhostLocation <= GhostMiddle AND GhostLocation >= GhostMiddle-9) Then								'Is ghost halfway back?
					playSFX 0, "H", hosProgress(player) + 48, random(3) + 68, 255	'Kaminski pleas for help!
				End If
				if (GhostLocation <= GhostAtDoor+5) Then								'Is ghost back to door?
					playSFX 0, "H", hosProgress(player) + 48, 71 + random(3), 255	'Doctor dictates
					video "H", "6", "Z", allowSmall, 75, 200						'Video of dictating with PROMPT
					killTimer(0)
					DoctorState = 0													'Set Doctor Flag to NOT distracted - must hit Ghost again!
					ghostAction = Int(5500/CycleAdjuster)							'Set flag for him to jiggle near door
					light 8, 0								'Switch lights back
					light 9, 0
					light 10, 0
					light 11, 0	
					pulse(17)
					pulse(18)
					pulse(19)
					light hosProgress(player) + 1, 7			'Use number to indicate progress
				End If					
			End If
		End If
	End If
	if (hosProgress(player) = 9) Then								'Did we just bash the door a 3rd time, freeing our friend?
		HospitalMultiball()
	End If
	if (hosProgress(player) = 10) Then								'During Doctor Ghost multiball!
		Dim X
		modeTimer  = modeTimer  +  1
		if (modeTimer = Int(120000/CycleAdjuster)) Then
			lightningStart(1)		
			x = random(10)
			if (x < 5) Then
				playSFX 0, "H", "C", random(9), 200					'Team Leader commanding ghost to leave and stuff
			Else
				playSFX 0, "L", "G", random(8), 200					'Random lightning		
			End If				
		End If		
		if (modeTimer = Int(150000/CycleAdjuster)) Then
			modeTimer = 0
		End If		
	End If
End Sub

sub HospitalSwitchCheck()											'What happens when you hit the Ghost Targets during the battle 
	if (hosProgress(player) < 9) Then								'Still trying to distract the ghost?
		video "H", "6", "A", allowSmall, 62, 200						'Ghost distracted away from door!
		doctorHits  = doctorHits  +  1
		AddScore(50000 * doctorHits)				'Spam the doctor for more points	
		ghostMove GhostDistracted, 20				'Move the ghost away from door
		if (doctorHits = 1) Then						'First time we've hit him?
			playSFX 0, "H", hosProgress(player) + 48, random(3) + 65, 200		'Play the progress + A B or C clips
		Else
			playSFX 2, "H", "0", 2 + random(4), 190								'Random ghost wails of agony! Lower priority than Ghost Doctor Rambling
		End If
		customScore "H", "7", hosProgress(player) + 65, allowAll OR loopVideo, 70		'Shoot Door custom prompt 3 2 1 to go (files ending G H I
		if (doctorHits = 4) Then						'Repeat the loop to remind player what to do (prompt on hit 1)
			doctorHits = 0
		End If
																	'You can keep distracting the ghost but we DON'T advance until you then hit the door.
		light 16, 0													'Switch lights back
		light 17, 0
		light 18, 0
		light 19, 0
		strobe 8, 7
		ghostFlash(100)		
		ghostAction = 0												'Disable Ghost Jitters
		countSeconds = 21											'Ghost goes from 120 to 10, 110 degrees, 10 degrees per move, 11 moves, move every other second = 22 seconds
';		numbers 0, numberStay OR 4, 0, 0, countSeconds - 1			'Show the Numbers timer
		DoctorState = 1												'Set flag that ghost is distracted
		DoctorTimer = 0												'Reset timer
		DoctorTarget = longSecond * 2								'New target
	End If	
End Sub

sub HospitalMultiball()												'Ghost defeated, beat the crap out of him
	AddScore(winScore)												'5 mil for beating him
	sendJackpot(0)													'Send current jackpot value to Score #0
	spiritGuideEnable(0)											'No spirit guide during MB
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0													'Disable this.
	strobe 8, 7														'Strobe the door for POISON!
	light 57, 7														'HOSPITAL solid = Mode Won!
	blink(16)														'Blink the JACKPOT light
	strobe 17, 3
	killTimer(0)													'Turn off numbers	
	killQ()															'Disable any Enqueued videos	
'	playMusic "G", "S"												'Play annoying Ghost Squad theme!		
	musicplayer "bgout_GS.mp3"
	playSFX 0, "H", "8", 74 + random(3), 255						'Let's kick his ass quotes
	video "H", "8", "J", allowSmall, 84, 255							'"Escape" video
	ghostMove 90, 20	
	TargetTimerSet 50, TargetDown, 10
	'TargetSet(TargetDown)											'Allow Ghost bashing!
	DoorSet DoorOpen, 1												'Open door fast! We'll close it upon losing second ball
	KickLeft 16000, vukPower										'Release captured ball!
	trapDoor = 0													'Flag that ball shouldn't be trapped behind door	
	activeBalls  = activeBalls  +  1								'Increase active balls to 2.
	ballSave()														'Ball save on Multiball
	customScore "B", "1", "D", allowAll OR loopVideo, 36			'Custom Score: Hit ghost for JACKPOTS!
'	numbers 8, numberScore OR 2, 0, 0, player						'Put player score upper left
	numbers PlayerScore(Player), "", "", ""
'	numbers 9, numberScore OR 2, 72, 27, 0							'Use Score #0 to display the Jackpot Value bottom off to right
	numbers "", "", "", EVP_Jackpot
'	numbers 10, 9, 88, 0, 0										'Ball # upper right
	numbers "", Ball, "", ""
	modeTimer = 0													'Reset timer for exorcist quotes
	hosProgress(player) = 90										'Set this so the "End Battle" can start only once. It's at 90 until left VUK kicks, then goes to 10
	ModeWon(player) = ModeWon(player) OR 2							'Set HOSPITAL WON bit for this player.	
	if (countGhosts() = 6) Then										'This the final Ghost Boss? Light BOSSES solid!
		light 48, 7
	End If	
	videoModeCheck()	
	multipleBalls = 1												'When MB starts, you get ballSave amount of time to loose balls and get them back
	ballSave()														'That is, Ball Save only times out, it isn't disabled via the first ball lost		
End Sub

sub HospitalWin()								'We come here when down to 1 ball in multiball
	DOF 134, 2
	Dim X
	if (multiBall) Then							'Was a MB stacked?
		multiBallEnd(1)						'End it, with flag that it's ending along with a mode
	End If	
	multipleBalls = 0
	tourClear()								'Clear the tour lights / values
	loadLamp(player)
	comboKill()
	spiritGuideEnable(1)						'Allow it
	patientStage = 0
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(200/CycleAdjuster)
	ghostFadeAmount = Int(200/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	light 16, 0													'Turn off Ghost lights
	light 17, 0
	light 18, 0
	light 19, 0
	light 8, 0													'Turn off Hospital Advance lights
	light 9, 0
	light 10, 0
	light 11, 0
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	light 16, 0													'Turn off Make Contact
	light 57, 7													'Make Hospital Mode light solid, since it HAS been won
	light 31, 0	
	killTimer(0)													'Turn off numbers
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()
	playSFX 0, "H", "9", 88 + random(3), 255						'Mode Complete dialog	
	killQ()															'Disable any Enqueued videos
	video "H", "9", "X", noExitFlush, 59, 255						'Mode won, prevent numbers
';	numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233	'Load Mode Total Points as Number	
	modeTotal = 0													'Reset mode points	
	videoQ "H", "9", "Y", noEntryFlush OR 3, 0, 233					'Mode Total Video	
'	playMusic "M", "2"												'Normal music
	musicplayer "bgout_M2.mp3"
	Mode(player) = 0												'Set mode active to None
	hosProgress(player) = 100										'Prevents a restart
	ModeWon(player) = ModeWon(player) OR 2							'Set HOSPITAL WON bit for this player.	
	ghostsDefeated(player)  = ghostsDefeated(player)  +  1									'For bonuses
	Advance_Enable = 1												'Allow other modes to be started
	if (countGhosts() = 2 or countGhosts() = 5) Then	'Defeating 2 or 5 ghosts lights EXTRA BALL
		extraBallLight(2)							'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random 2), 255	'"Extra Ball is Lit!"			
	End If	
	demonQualify()													'See if Demon Mode is ready
	checkModePost()
	for x=0 To 5													'Make sure the MB lights are off
		light 26 + x, 0
	Next
	hellEnable(1)		
	showProgress 0, player					'Show the Main Progress lights
End Sub

Function HospitalFail()									'You fail when you lose your second ball before freeing Friend. But we do logic to see if you get a Do-Over, or a Drain.
	Dim X
	multipleBalls = 0
	tourClear()								'Clear the tour lights / values
	loadLamp(player)
	spiritGuideEnable(1)						'Allow Spirit Guide again
	patientStage = 0
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(100/CycleAdjuster)
	ghostFadeAmount = Int(100/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	killTimer(0)
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
'	swDebounce(23) = 25000											
	light 16, 0														'Turn off Ghost lights
	light 17, 0
	light 18, 0
	light 19, 0
	trapDoor = 0													'Flag that ball shouldn't be trapped behind door
	modeTotal = 0													'Reset mode points
	if ((ModeWon(player) AND 2)=2) Then								'Did we win this mode before?
		light 57, 7													'Make Hospital Mode light solid, since it HAS been won
	Else
		light 57, 0													'Haven't won it yet, turn it off
	End If
	ghostMove 90, 20												'Turn ghost back to center	
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	Mode(player) = 0												'Set mode active to None
	Advance_Enable = 1												'Allow other modes to be started
	'checkModePost()
	hellEnable(1)
	if ((modeRestart(player) AND 2) = 2 AND tiltFlag = 0) Then		'Able to restart Hospital?
		modeRestart(player) = modeRestart(player) AND 253			'Clear the restart bit	
		if (hosTrapCheck = 0) Then									'Don't kick the ball if we already did that
			LeftTimer = Int(16000/CycleAdjuster)					'Manually set the kick out.
			LeftPower = vukPower			
		End If
		modeTimer = Int(25000/CycleAdjuster)
		DoorSet DoorOpen, 2										'Open door quickly
		ghostMove 10, 255											'Ghost will slowly turn towards door!
		restartBegin 1, 11, 25000									'Enable a restart!		
		hosProgress(player) = 3									'Allows you to re-start the mode
		strobe 8, 3												'Strobe lights under door		
		light 9, 0		
		light 10, 0
		blink(11)													'Blink GHOST DOCTOR
'		playMusic "H", "2"										'Hurry Up Music!
		musicplayer "bgout_H2.mp3"
		killQ()													'Disable any Enqueued videos	
		video "H", "8", "Y", allowSmall, 121, 255 					'Mode fail! Shoot door to restart!
		playSFX 0, "H", "Z", random(6) + 65, 255						'Mode FAIL dialog		
		showProgress 0, player									'Show the Main Progress lights
		HospitalFail = 1
																	'Flag to prevent a drain!
	Else															'End mode, and let the ball drain
		checkModePost()
		if (tiltFlag = 0) Then
			LeftTimer = Int(1100/CycleAdjuster)						'Manually set the kick out super quick
			LeftPower = vukPower									'A tilt does a ball search, no need to in that condition
		End If
		hosProgress(player) = 0									'Gotta start over
		light 11, 0												'Turn off Doctor Ghost light
		showProgress 0, player
		HospitalFail = 0
	End If
	comboEnable = 1												'OK combo all you want	
	for x=0 To 5												'Make sure the MB lights are off
		light 26 + x, 0
	Next
	showProgress 0, player
End Function
'END FUNCTIONS HOSPITAL MODE 1..................

sub HospitalRestart()											'Allows a quick restart if a Ball Search fucked up the mode
	multipleBalls = 0
	tourClear()													'Clear the tour lights / values
	loadLamp(player)
	spiritGuideEnable(1)										'Allow Spirit Guide again
	patientStage = 0
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(100/cycleAdjuster)
	ghostFadeAmount = Int(100/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	killTimer(0)
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	light 16, 0													'Turn off Ghost lights
	light 17, 0
	light 18, 0
	light 19, 0
	trapDoor = 0													'Flag that ball shouldn't be trapped behind door
	hosTrapCheck = 0												'Clear this!
	modeTotal = 0													'Reset mode points
	Mode(player) = 0												'Set mode active to None
	Advance_Enable = 1												'Allow other modes to be started
	'checkModePost()
	hellEnable(1)
	DoorSet DoorOpen, 2										'Open door quickly
	ghostMove 90, 20											'Turn ghost back to center	
	ghostLook = 0												'Ghost won't look around
	ghostAction = 0											'Disable its animation
	hosProgress(player) = 3									'Allows you to re-start the mode
	strobe 8, 3												'Strobe lights under door		
	light 9, 0		
	light 10, 0
	blink(11)													'Blink GHOST DOCTOR	
	killQ()													'Disable any Enqueued videos	
	video "H", "8", "Y", allowSmall, 121, 255 					'Mode fail! Shoot door to restart!
	playSFX 0, "H", "Z", random(6) + 65, 255					'Mode FAIL dialog		
	showProgress 0, player									'Show the Main Progress lights
'	playMusic "M", "2"										'Normal music
	musicplayer "bgout_M2.mp3"
End Sub

Function hotelPathLogic()
	Dim X, Y
	animatePF 220, 10, 0
	ghostLooking(160)
	comboCheck(3)													'Always check for combo shot!
	if (Mode(player) = 6) Then										'Prison?
		tourGuide 1, 6, 3, 25000, 1									'Check that part of the tour!
		hotelPathLogic = 1
		Exit Function
	End If
	if (Mode(player) = 4) Then										'War fort?
		if (multiBall AND multiballHell) Then						'If MB stacked, first collect Tour. Once collected, it returns a 0 allows Jackpot increase
			if (tourGuide(1, 4, 3, 0, 0)) = 0 Then					'Already did this part of tour?
				multiBallJackpotIncrease()
			End If		
		Else
			x = random(8)
			If X=0 Then Y=79
			If X=1 Then Y=81
			If X=2 Then Y=50
			If X=3 Then Y=47
			If X=4 Then Y=49
			If X=5 Then Y=110
			If X=6 Then Y=77
			If X=7 Then Y=67
			playSFX 0, "W", "5", 65 + x, 210						'Random Army Ghost lines
			if (tourGuide(1, 4, 3, 25000, 0)) = 0 Then
				video "W", "5", 65 + x, allowSmall, Y, 250			'Synced taunt video
			End If													'Check that part of the tour (no WHOOSH sound needed)
		End If
		hotelPathLogic = 1
		Exit Function
	End If
	if (Mode(player) = 1) Then										'Hospital Mode?
		if ((hosProgress(player) > 5 and hosProgress(player) < 9) and (multiBall AND multiballHell)) Then
			multiBallJackpotIncrease()
			hotelPathLogic = 1
			Exit Function
		End If
		if (hosProgress(player) = 10) Then							'Hospital, fighting the Ghost Doctor?
			if (patientStage = 0) Then								'Haven't got the poison yet, and have a MB? Then this advances Jackpot
				if (multiBall AND multiballHell) Then						
					multiBallJackpotIncrease()
					hotelPathLogic = 1
					Exit Function
				Else
					video "H", "7", "A", allowSmall, 70, 240		'Sick ghost in bed		
					playSFX 0, "I", "P", 65 + random(4), 240		'"Kill.... me...."
					strobe 8, 7										'Strobe the door for POISON!	
					hotelPathLogic = 1
					Exit Function
				End If			
			Else
				video "H", "7", "C", allowSmall, 63, 255				'Ghost freed!
				playSFX 0, "I", "P", 83 + random(8), 255			'"Die!" "Thank you kind sir!"
				patientsSaved  = patientsSaved  +  1
				showValue ((patientsSaved * 250000) +  patientStage * 10000), 40, 1				'Flash the points we got
				patientStage = 0									'Reset patient stage
				strobe 8, 7											'Strobe the door for POISON!
				hellEnable(1)										'Until you make the Poison shot, you can go for multiball. Re-enable MB here
				if (multiBall AND multiballHell) Then
					strobe 26, 5									'If MB active, re-strobe Ramp for Jackpot Increase
				Else
					light 26, 0										'Turn off RAMP STROBE			
				End If
				hotelPathLogic = 1
				Exit Function
			End If		
		End If
	End If		
	if (deProgress(player) > 9 and deProgress(player) < 100) Then		'Trying to weaken demon
		DemonCheck(3)
		hotelPathLogic = 1
		Exit Function
	End If		
	if (Advance_Enable and hotProgress(player) < 3) Then				'Able to advance modes, and Hotel not started yet?
		HotelAdvance()	
		hotelPathLogic = 1
		Exit Function
	End If
	if (Mode(player) = 7) Then										'Are we in Ghost Photo Hunt?
		photoCheck(3)
		hotelPathLogic = 1
		Exit Function
	End If
	if (Mode(player) = 5) Then										'Hotel ghost?
		if (hotProgress(player) = 20) Then							'Hotel shot isn't used during control box search	
			AddScore(10000)											'Some points			
			EVP_Jackpot(player)  = EVP_Jackpot(player)  +  25000							'Moar points plz!			
';			numbers 3, numberFlash OR 1, 255, 11, EVP_Jackpot(player)	'Load Jackpot value Points as a number
			numbers "", "", EVP_Jackpot(player), EVP_Jackpot(player)
			video "Q", "J", "C", noEntryFlush OR 3, 45, 255			'Show new Jackpot value
			playSFX 2, "A", "Z", "Z", 255							'Generic shot WHOOSH sound
			strobe 26, 5											'Strobe first 5 lights	to indicate Hotel Path still does something (builds jackpot			
			'video 'L', '5', 'E', allowSmall, 0, 255				'Prompt to shoot flashing camera icons
			hotelPathLogic = 1
			Exit Function
		End If	
		if (hotProgress(player) = 35) Then								'When fighting ghost, only time you can shoot through Hotel shot is if you've hit Hellavator to light Jackpot	
			AddScore(100000)											'Some points
			playSFX 0, "L", "8", 73 + random(8), 255					'Hit the ghost for Jackpot!
			video "L", "G", jackpotMultiplier, allowSmall, 29, 250		'Show Multiplier
			hotelPathLogic = 1
			Exit Function
		End If	
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then	'Theater Ghost?		
		TheaterPlay(0)												'Incorrect shot, ghost will bitch!
		hotelPathLogic = 1
		Exit Function
	End If
	if (multiBall AND multiballHell) Then								'If stacked with Ghost Euthanasia, don't advance value if we need to euthanize ghost							
		multiBallJackpotIncrease()
		hotelPathLogic = 1
		Exit Function
	End If	
	comboVideoFlag = 0												'Nothing active? Reset video combo flag
	AddScore(5000)													'Some points
	video "C", "G", "D", allowSmall, 39, 250						'Regular Combo to the Left <-
	playSFX 2, "A", "Z", "Z", 255									'Whoosh!
	hotelPathLogic = 1
	Exit Function
End Function

'FUNCTIONS FOR HOTEL MODE 5.................................
sub HotelAdvance()													'What happens when we shoot up right ramp and advance Hotel
	Dim X
	AddScore(advanceScore)
	flashCab 0, 255, 0, 100											'Flash the GHOST BOSS color
	hotProgress(player)  = hotProgress(player)  +  1				'Advance progress (will be at least 1)
	areaProgress(player)  = areaProgress(player)  +  1
	if (hotProgress(player) < 4) Then								'First 3 advances?
		playSFX 0, "L", hotProgress(player), random(4) + 65, 255		'First 3 sets of Hotel advance sounds.
		video "L", hotProgress(player), "A", allowSmall, 60, 255		'Adance videos
		pulse(26)													'Have 1 pulsing at minimum
		if (hotProgress(player) < 4) Then							'First 3 advances?
			for x=0 To hotProgress(player)-1						'Fill the lights!
				light x + 26, 7										'Completed lights to SOLID
				pulse(x + 27)										'Pulse the next light
			Next
		End If
	End If
	if (hotProgress(player) = 3) Then								'Did we go here the 3rd time?
		ElevatorSet hellUp, 200										'Move the elevator into 2nd floor position
		light 24, 0													'Turn off CALL ELEVATOR lights
		light 25, 0
		blink(41)													'Blink HELL FLASHER
	End If
End Sub

sub HotelStart1()													'Ball goes into Hellavator, heading down...
	videoModeCheck()
	comboKill()
	storeLamp(player)												'Store the state of the Player's lamps
	allLamp(0)														'Turn off the lamps
	spiritGuideEnable(0)											'No spirit guide
	Advance_Enable = 0												'Mode has started, others can't
	Mode(player) = 5												'Mode has begun, enable its logic	
	minionEnd(0)													'Disable Minion mode, even if it's in progress
	modeTotal = 0													'Reset mode points	
	AddScore(startScore)
'	setFadeRGB 200, 140, 0, 1000									'Fade into a kind of brown colored ghost
	ghostModeRGB(0) = 200											'EP- This is the only place in the code where he calls this sub, so I'm leaving it out
	ghostModeRGB(1) = 140
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(1000/CycleAdjuster)
	ghostFadeAmount = Int((1000/10)/CycleAdjuster)
	setCabModeFade 0, 255, 0, 600									'Turn lighting GREEN (with envy
	popLogic(3)														'Set pops to EVP
	light 29, 7														'Bellboy ghost SOLID, we found him	
	blink(61)														'Blink Hotel Mode light
	hotProgress(player) = 10										'Set flag to "Elevator Dropping"
	playSFX 0, "L", "4", 65 + random(4), 255						'Play l5A-l5D.wav files
	killQ()															'Disable any Enqueued videos
	video "L", "4", "A", allowSmall, 66, 255						'Video of button pushed
	hellLock(player) = 0											'Since we use the Hellavator for Jackpots, can't stack a MB	
	HellBall = 10													'Set flag so it won't retrigger
	ElevatorSet hellStuck, 600										'Send elevator to Stuck position slowly
	showProgress 1, player											'Show the Main Progress lights
	skip = 50														'Set skip event for Elevator move
End Sub

sub HotelStart2()													'Ball gets stuck, gotta find control box!
	Dim X
	hotProgress(player) = 15										'Set flag to "Ball Rolling Towards Scoop"
	playSFX 0, "L", "4", 69 + random(4), 255						'Play l5A-l5D.wav files
	video "L", "4", "B", allowSmall, 120, 255						'Video of button pushed
	ElevatorSet hellDown, 300										'Send elevator to basement to release the ball
	customScore "L", "P", "A", allowAll OR loopVideo, 30			'Shoot Camera Icons! custom score prompt
'	numbers 8, numberScore OR 2, 0, 0, player						'Show player's score in upper left corner
	numbers PlayerScore(player), "", "", ""
'	numbers 10, 9, 88, 0, 0											'Ball # upper right
	numbers "", Ball, "", ""
	light 41, 0														'Turn off HELL FLASHER
	for x=0 To 4
		ControlBox(x) = 0											'Clear Control Box locations	
	Next
	if (tournament = 0) Then										'Unless we're in tourney mode where 3rd shot always wins...
		ControlBox(random(5)) = 255									'Randomly select ONE location to have the control box. (255 flag)
	End If
	DoorSet DoorOpen, 5												'Open the Spooky Door, if it isn't already
	if (countGhosts() = 5) Then										'Is this the last Boss Ghost to beat?
		blink(48)													'Blink that progress light
	End If
	pulse(7)														'Pulse all the Camera Lights, sans Hotel one
	pulse(14)
	pulse(23)
	pulse(39)
	pulse(47)
	'Set STROBING lights to indicate shots. Can't do this until we write the LIGHT SAVE STATE CODE
	strobe 26, 5													'Strobe first 5 lights	to indicate Hotel Path still does something (builds jackpot	
	modeTimer = 0													'Reset mode timer for prompts
	photoWhich = 0													'We use this to count the shots. In tourney mode, 3rd shot always finds control box.
	TargetTimerSet 5000, TargetUp, 100
'	playMusic "B", "1"												'Boss battle music!
	musicplayer "bgout_B1.mp3"
	hellLock(player) = 0											'Disable lock manually
	comboEnable = 0													'Combos during Control Box search would be confusing, so no
	skip = 0														'Reset skip ability
End Sub

sub HotelLogic()													'Logic during control box search / ghost battle
	if (hotProgress(player) = 10) Then								'Waiting for the elevator to get stuck?
		if (HellLocation = hellStuck) Then							'Did the Hellavator make it to the Stuck position?
			HotelStart2()											'Time for Battle! Find control box and banish ghost!
		End If
	End If
	if (hotProgress(player) = 20) Then								'Looking for control box?
		modeTimer  = modeTimer  +  1
		if (modeTimer = Int(150000/CycleAdjuster)) Then				'Random ghost taunt?
			modeTimer = 0											'Reset timer
			playSFX 0, "L", "5", 65 + random(22), 200				'Will not override advance dialog
			video "L", "5", "A", allowSmall, 60, 100				'Will not override video
		End If
	End If
End Sub

sub HotelMultiball()												'What happens when you find the control box
	jackpotMultiplier = 1											'Starts at 1			
	AddScore(winScore)												'You won, so Point Get
	light 7, 0														'Turn off all CAMERA LIGHTS
	light 14, 0
	light 23, 0
	light 39, 0
	light 47, 0
	ghostLook = 1													'Ghost will now look around again.
	light 61, 7														'HOTEL solid = Mode Won!
	blink(17)														'Blink ADVANCE JACKPOT
	blink(18)
	blink(19)
	light 16, 0														'Make sure JACKPOT is off
	strobe 26, 6													'Strobe the Hotel Lights
	pulse(14)														'Pulse the door for Ghost Eviction
	tourReset(43)													'Tour: Left orbit, up center, right orbit, scoop	  OLD Tour: Left orbit, spooky VUK, up center, balcony
	targetReset()													'Reset the target
'	playMusic 'G', 'S'												'Play annoying Ghost Squad theme!
	musicplayer "bgout_GS.mp3"
	playSFX 0, "L", "7", 65 + random(3), 255						'Yeah! We fuckin' did it!
	killQ()															'Disable any Enqueued videos
	video "L", "7", "A", allowSmall, 91, 255						'Pull lever video
	videoQ "L", "8", "D", allowSmall, 30, 200						'"Ramp lights Jackpot"	
	sendJackpot(0)													'Send jackpot value to score #0
	customScore "L", "P", "B", allowAll OR loopVideo, 60			'Prompt for Ramp and Target Multiplier
'	numbers 8, numberScore OR 2, 0, 0, player						'Put player score upper left
	numbers PlayerScore(player), "", "", ""
'	numbers 9, numberScore OR 2, 72, 27, 0							'Use Score #0 to display the Jackpot Value bottom off to right
	numbers "", "", "", hellJackpot
'	numbers 10, 9, 88, 0, 0											'Ball # upper right
	numbers "", Ball, "", ""
	ghostMove 90, 15
	if (extraLit(player) = 0) Then									'An EB could be lit, and not collected during Control Box search.
		DoorSet DoorClosed, 200										'Else, closes the door for GHOST EVICTION!		
	End If
	TargetTimerSet 10, TargetUp, 10									'Put targets UP
	ElevatorSet hellUp, 100											'Move elevator UP so you can shoot it to Light Jackpot
	blink(41)														'HELL FLASHER!
	hotProgress(player) = 30										'Set this so the "End Battle" can start only once.
	ModeWon(player) = ModeWon(player) OR 32							'Set HOTEL WON bit for this player.	
	if (countGhosts() = 6) Then										'This the final Ghost Boss? Light BOSSES solid!
		light 48, 7
	End If
	convictState = 1												'Use same variable as Prison Free Ghost for GHOST EVICTION!
	convictsSaved = 0												'Reset How many you've evicted
	AutoPlunge(autoPlungeFast)										'Set flag to launch second ball
	multipleBalls = 1												'When MB starts, you get ballSave amount of time to loose balls and get them back
	ballSave()														'That is, Ball Save only times out, it isn't disabled via the first ball lost														
	comboEnable = 1													'OK combo all you want
End Sub

sub HotelLightJackpot()
	hotProgress(player) = 35										'Set JACKPOT READY!
	playSFX 0, "L", "8", 73 + random(8), 255						'Hit the ghost for Jackpot!
	killQ()
	'CHANGE VIDEO TO DISPLAY WHAT JACKPOT IS LIT
	video "L", "G", jackpotMultiplier, allowSmall, 29, 250			'Show Multiplier
	'videoQ 'L', '8', 'B', allowSmall, 0, 240						'Ghost cowers!
	customScore "B", "1", "D", allowAll OR loopVideo, 36			'Custom Score: Hit ghost for JACKPOT!
	sendJackpot(0)													'Send updated jackpot value to score #0
	light 17, 0														'First turn them all off
	light 18, 0
	light 19, 0
	'Blink the Ghost Targets that we hit already
	if (gTargets(0) = 1) Then
		blink(17)
	End If
	if (gTargets(1) = 1) Then
		blink(18)
	End If
	if (gTargets(2) = 1) Then
		blink(19)
	End If
	pulse(16)														'Pulse MAKE CONTACT
	light 26, 0														'Turn OFF Hotel Stobe
	TargetSet(TargetDown)											'Put targets down
End Sub

sub HotelJackpot()
	blink(17)														'Blink ADVANCE JACKPOT
	blink(18)
	blink(19)
	light 16, 0														'Make sure JACKPOT is off
	strobe 26, 6													'Strobe the Hotel Lights
	targetReset()													'Reset the target values so we can re-multiply Jackpot
	ElevatorSet hellUp, 100											'Move elevator down
	blink(41)
	killQ()
	ghostFlash(50)
	playSFX 0, "L", "8", 81 + random(4), 255						'Jackpot sounds!
	video "L", "J", jackpotMultiplier, allowSmall, 45, 255			'Jackpot animation
	showValue EVP_Jackpot(player) * jackpotMultiplier, 40, 1		'Show what jackpot value was
	customScore "L", "P", "B", allowAll OR loopVideo, 60			'Prompt for Ramp and Target Multiplier	
	hotProgress(player) = 30										'Reset flag, we need to re-enable Jackpot
	TargetTimerSet 8000, TargetUp, 5								'Put targets back up
	ghostAction = Int(20000/CycleAdjuster)							'Whack routine
	jackpotMultiplier = 1
	sendJackpot(0)													'Send updated jackpot value (no multiplier now) to score #0
End Sub

sub HotelWin()														'When down to 1 ball, mode is won!
	DOF 134, 2
	multipleBalls = 0
	tourClear()														'Clear the tour lights / values	
	loadLamp(player)												'Load the original lamp state back in
	comboKill()
	light 61, 7														'HOTEL solid = Mode Won!
	AddScore(5000000)												'5 mil for beating him
	convictState = 0
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	ghostFadeTimer = Int(200/CycleAdjuster)
	ghostFadeAmount = Int(200/CycleAdjuster)
	killNumbers()
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()
	light 16, 0														'Turn off Ghost Lights
	light 17, 0
	light 18, 0
	light 19, 0
	light 26, 0														'Turn off Advance Lights
	light 27, 0
	light 28, 0
	light 29, 0
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0	
	if (videoMode(player) = 0) Then
		TargetTimerSet 5000, TargetUp, 100							'Put targets back up, but not so fast ball is caught
	End If
	playSFX 0, "L", "9", 65 + random(4), 255						'Mode Complete dialog	
	killQ()															'Disable any Enqueued videos	
	video "L", "9", "A", noExitFlush, 63, 255 						'Play Death Video
'	numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233	'Load Mode Total Points
	numbers "", "", ModeTotal, ModeTotal
	modeTotal = 0													'Reset mode points		
	videoQ "L", "9", "B", noEntryFlush OR 3, 45, 233				'Mode Total:	
'	playMusic "M", "2"												'Normal music
	musicplayer "bgout_M2.mp3"
	Mode(player) = 0												'Set mode active to None
	hotProgress(player) = 100										'Can't be restarted
	ModeWon(player) = ModeWon(player) OR 32							'Set HOTEL WON bit for this player.	
	ghostsDefeated(player)  = ghostsDefeated(player)  +  1			'For bonuses
	Advance_Enable = 1												'Other modes can start now
	if (countGhosts() = 2 or countGhosts() = 5) Then				'Defeating 2 or 5 ghosts lights EXTRA BALL
		extraBallLight(2)							'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random 2), 255	'"Extra Ball is Lit!"			
	End If	
	demonQualify()									'See if Demon Mode is ready
	checkModePost()
	hellEnable(1)
	showProgress 0, player					'Show the Main Progress lights
	spiritGuideEnable(1)
End Sub

sub HotelFail()
	multipleBalls = 0
	tourClear()								'Clear the tour lights / values	
	loadLamp(player)								'Load the original lamp state back in
	comboKill()
	convictState = 0
	spiritGuideEnable(1)
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(100/CycleAdjuster)
	ghostFadeAmount = Int(100/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	killNumbers()
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	light 7, 0	'Turn off all CAMERA LIGHTS
	light 14, 0
	light 23, 0
	light 39, 0
	light 47, 0
	light 16, 0														'Turn off Ghost Lights
	light 17, 0
	light 18, 0
	light 19, 0
	if ((ModeWon(player) AND 32) = 32) Then							'Did we win this mode before?
		light 61, 7													'Make Hospital Mode light solid, since it HAS been won
	Else
		light 61, 0													'Haven't won it yet, turn it off
	End If
	ElevatorSet hellDown, 100 										'Make sure Hellavator is down
	light 25, 7														'Current state is SOLID
	blink(24)														'Other state BLINKS
	light 30, 0														'Lock is NOT lit
	modeTotal = 0													'Reset mode points
	Mode(player) = 0												'Set mode active to None	
	Advance_Enable = 1												'Other modes can start now
	if ((modeRestart(player) AND 32) = 32) Then						'Able to restart Hotel?
		modeRestart(player) = ModeRestart(player) AND 223			'Clear the restart bit	
		hotProgress(player) = 3										'Reset this to 2. Shoot the right ramp again will raise elevator and let you try again.
	Else
		hotProgress(player) = 0										'Reset this to 2. Shoot the right ramp again will raise elevator and let you try again.
	End If
	showProgress 0, player											'Show the Main Progress lights	
	checkModePost()
	hellEnable(1)
End Sub
'END HOTEL MODE 5.................................

sub BoxCheck(whichSpot)												'Code for looking for control box. Maybe update using Photo Hunt code? (it's much better)
	if (ControlBox(whichSpot) = 255) Then							'Control box found? (this will never actually occur in Tournament mode)
		ControlBox(whichSpot) = 1									'Set "Checked Here" flag
		HotelMultiball()
		Exit Sub													'Don't do other checks (prevents "miss" dialog when you actually get it)
	End If
	if (ControlBox(whichSpot) = 1) Then								'Did we already check here?
		playSFX 0, "L", "6", 85 + random(5), 255					'"You already looked there!"
		video "L", "6", 85 + random(6), allowSmall, 45, 200			'Video of search		
		'videoQ 'L', '5', 'E', allowSmall, 0, 100					'Prompt to find Camera shot
	End If	
	if (ControlBox(whichSpot) = 0) Then								'First time we've checked here?
		photoWhich  = photoWhich  +  1
		ControlBox(whichSpot) = 1									'Set "Checked Here" flag
		'videoQ 'L', '5', 'E', allowSmall, 0, 100					'Prompt to find Camera shot
		Select Case whichSpot										'Turn off the lights when we hit them
			case 0:
				light 7, 0
			case 1:
				light 14, 0
			case 2:
				light 23, 0
			case 3:
				light 39, 0
			case 4:
				light 47, 0
		End Select
		if (tournament and photoWhich = 3) Then						'In tournament mode, the 3rd unique Camera Shot you hit always = mode win
			HotelMultiball()
		Else
			playSFX 0, "L", "6", 65 + random(13), 255				'Play the "Its not here" sound l6A - l6E
			video "L", "6", 65 + random(14), allowSmall, 45, 200	'Video of search
		End If
	End If
End Sub

sub KickLeft(kickTime, lStrength)
	kickTime = Int(kickTime/CycleAdjuster)
	if (hosProgress(player) > 3 and hosProgress(player) < 9) Then	'Helps prevent ball ejects...
		Exit Sub													'Dring Doctor battle
	End If
	badExit = 1														'Prevents re-triggering. If ball doesn't go down left habitrail, this flag will kick ball if it ends up back in scoop
	LeftTimer = kickTime
	LeftPower = lStrength
End Sub

sub leftOrbitLogic()
	Dim X, Y
	animatePF 190, 10, 0
	if (hellMB and minion(player) < 100) Then
		tourGuide 3, 8, 0, 50000, 1										'Check for GHOST CATCH, and give default 50k if we've already hit that spot
		Exit Sub
	End If	
	if (Mode(player) = 4) Then											'War fort active?
		x = random(8)
		If X = 0 Then Y = 79
		If X = 1 Then Y = 81
		If X = 2 Then Y = 50
		If X = 3 Then Y = 47
		If X = 4 Then Y = 49
		If X = 5 Then Y = 110
		If X = 6 Then Y = 77
		If X = 7 Then Y = 67
		playSFX 0, "W", "5", 65 + x, 210								'Random Army Ghost lines
		if tourGuide(3, 4, 0, 25000, 0) = 0 Then
			video "W", "5", 65 + x, allowSmall, Y, 250					'Synced taunt video
		End If															'Check that part of the tour (no WHOOSH sound needed)		
		Exit Sub
	End If
	if (barProgress(player) > 69 and barProgress(player) < 100) Then	'Haunted Bar active?
		tourGuide 3, 3, 0, 25000, 1										'Check that part of the tour!
		Exit Sub
	End If
	if (Mode(player) = 1) Then											'Hospital active?
		tourGuide 3, 1, 0, 25000, 1										'Check that part of the tour!
		Exit Sub
	End If
	if (hotProgress(player) > 29 and hotProgress(player) < 40) Then		'Fighting the Hotel Ghost? (can't do tour during the Control Box search)
		tourGuide 3, 5, 0, 25000, 1										'Check that part of the tour!
		Exit Sub
	End If
	if (Mode(player) = 6) Then											'Prison mode active?
		tourGuide 3, 6, 0, 25000, 1										'Check that part of the tour!
		Exit Sub
	End If
	if (Advance_Enable and priProgress(player) < 4) Then				'Advancing Prison 1 2 3, or making 4th orbit shot to start?
		PrisonAdvance()													'Advance Prison (backstory))
		Exit Sub
	End If
	if (hotProgress(player) = 20)	Then								'Searching for the Control Box?
		BoxCheck(0)														'Check / flag box for this location
		Exit Sub
	End If
	if (Mode(player) = 7) Then											'Are we in Ghost Photo Hunt?
		photoCheck(0)
		Exit Sub
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then		'Theater Ghost?
		TheaterPlay(0)													'Incorrect shot, ghost will bitch!
		Exit Sub
	End If
	if (deProgress(player) > 9 and deProgress(player) < 100) Then		'Fighting the Demon?
		DemonCheck(0)
		Exit Sub
	End If		
	if (minionMB > 9) Then												'Minion MB Jackpot Increase?
		minionJackpotIncrease()
		MagnetSet(100)
		lightningStart(Int(50000/CycleAdjuster))
		Exit Sub
	End If
	comboVideoFlag = 0													'Nothing active? Reset video combo flag
	AddScore(5000)														'Some points
	video "C", "G", "E", allowSmall, 39, 250							'Regular Combo to the Right ->	
	playSFX 2, "A", "Z", "Z", 255										'Whoosh!
	Exit Sub
End Sub

Function leftVUKlogic()
	animatePF 200, 10, 0
	'Check if ball is supposed to be held behind door. If so, don't execute combos
	'BALL HOLD GOES HERE IF WE END UP MAKING THAT
	if (hosProgress(player) > 5 and hosProgress(player) < 9) Then		'Should ball stay locked?
		leftVUKLogic = 0
		Exit Function													'Do nothing
	End If
	if (deProgress(player) > 1 and deProgress(player) < 9) Then
		leftVUKLogic = 0												'Prevents ball from being kicked out
		Exit Function
	End If
	'OK, normal shot, continue:
	if (badExit) Then													'Did ball not successfully exit the VUK and roll down the habitrail?	
		KickLeft 7000, vukPower											'The default
		leftVUKLogic = 0
		Exit Function													'Do nothing
	End If
	comboCheck(1)
	if (extraLit(player))	Then										'Extra ball lit?
		extraBallCollect()												'Award it!
		KickLeft 31000, vukPower										'Kick it out!
		leftVUKLogic = 1
		Exit Function
	End If
	if (Advance_Enable and theProgress(player) = 3) Then				'Ready to start Theater mode?
		TheaterStart()													'If THEATER and DOCTOR are both lit, THEATER starts first.
		leftVUKLogic = 0
		Exit Function													'If THEATER is WON, DOCTOR can be started			
	End If																'If THEATER fails (time out or drain) THEATER RE-LITES and will start if you shoot there again
	if (Mode(player) = 1 and hosProgress(player) = 10) Then				'Hospital battle?
		AddScore(100000)												'Poison Points!
		video "H", "7", "B", allowSmall, 60, 255						'Poison Grab!		
		playSFX 0, "I", "P", 69 + random(8), 255
		if (patientStage < 5) Then
			patientStage  = patientStage  +  1							'You can get up to 5 extra poisons for extra points. Not sure why. Who cares?
		End If
		hellEnable(0)													'Disable Hell Lock (also puts Elevator to DOWN). Can't enable MB until you make right ramp shot to euthanize ghost patient
		light 8, 0														'Turn OFF door strobe
		strobe 26, 5													'Turn ON hotel path strobe
		KickLeft 7000, vukPower											'The default
		leftVUKLogic = 1
		Exit Function
	End If
	if (Mode(player) = 6 and convictState = 2) Then						'We opened the door, now time to free a ghost?
		video "P", "8", "Z", allowSmall, 29, 255						'Ghost freed!
		playSFX 0, "P", "Z", 65 + random(4), 255
		convictsSaved  = convictsSaved  +  1
		showValue convictsSaved * 100000, 40, 1							'Flash what you scored for saving this convict
		convictState = 1												'Go back to Door Closed state
		DoorSet DoorClosed, 5											'Open door quickly!
		light 8, 0														'Turn off strobing
		pulse(14)														'Blink Camera shot
		modeTimer = 0													'Reset this so prompt won't happen for a bit
		KickLeft 7000, vukPower											'The default		
		leftVUKLogic = 1
		Exit Function
	End If
	if ((hotProgress(player) > 29 and hotProgress(player) < 40) and convictState = 2) Then	'Fighting the Hotel Ghost? (can't do tour during the Control Box search)
		'Ghost Eviction!
		convictsSaved  = convictsSaved  +  1
		video "L", "E", "2", allowSmall, 36, 255						'Ghost Evicted!
		playSFX 0, "L", "E", 65 + random(9), 255						'Random boot + ghost sound FX	
		if (convictsSaved < 10) Then									'Haven't evicted them all yet?
			showValue convictsSaved * 100000, 40, 1						'Flash what you scored for saving this convict		
			convictState = 1											'Go back to Door Closed state
			DoorSet DoorClosed, 5										'Close door
			light 8, 0													'Turn off strobing
			pulse(14)													'Pulse Camera shot			
		Else															'Kick out 10 to win?
			videoQ "L", "E", "9", 2, 30, 255							'All ghosts evicted!
			showValue convictsSaved * 250000, 40, 1						'Flash what you scored for evicting this ghost	
			convictState = 255											'Set this state to asub further triggers
			DoorSet DoorClosed, 5										'Close door
			light 8, 0													'Turn off strobing
			light 14, 0													'Turn off camera light		
		End If
		KickLeft 7000, vukPower											'The default
		leftVUKLogic = 1
		Exit Function
	End If
	if (barProgress(player) > 69 and barProgress(player) < 100) Then										'Haunted Bar?
		tourGuide 2, 3, 1, 25000, 1 								'Check that part of the tour!
		KickLeft 7000, vukPower										'The default
		leftVUKLogic = 1
		Exit Function
	End If
	if (hellMB and minion(player) < 100) Then
		tourGuide 2, 8, 1, 50000, 1								'Check for GHOST CATCH, and give default 50k if we've already hit that spot
		KickLeft 7000, vukPower										'The default
		leftVUKLogic = 1
		Exit Function
	End If
	if (goldHits = 10) Then											'Collecting gold?
		killQ()														'Disable any Enqueued videos
		video "W", "G", "F", allowSmall, 50, 255					'Got some gold!
		playSFX 0, "W", "G", 77 + random(8), 255					'Ka-ching, Kaminski happy, Ghost mad!
		AddScore(500000)											'One meeeeleon points!
		goldTotal  = goldTotal  +  500000							'Keep track of how much gold we got X multiplier
		KickLeft 7000, vukPower										'Kick it out fairly quickly
		leftVUKLogic = 1
		Exit Function
	End If
	if (deProgress(player) = 1 and Advance_Enable) Then				'Ready to start Demon Battle, and can start a mode?
		DemonLock1()
		leftVUKLogic = 1
		Exit Function												'Don't want the ball to be kicked out (like Doctor Ghost mode)
	End If
	if (deProgress(player) > 9 and deProgress(player) < 100) Then	'Trying to weaken demon
		DemonCheck(1)
		if (activeBalls > 1) Then
			KickLeft(7000 + ( activeBalls - 1) * 6300), vukPower	'Give player a slight break... the more balls active the longer it is			
		Else
			KickLeft 7000, vukPower									'The default			
		End If
		leftVUKLogic = 1
		Exit Function
	End If
	if (Advance_Enable and (hosProgress(player) > 0 and hosProgress(player) < 4)) Then	'IS this this 2nd or 3rd time we've hit this?
		HospitalAdvance()											'Advance Hospital
		KickLeft 7000, vukPower										'If both DOCTOR and THEATER are lit, DOCTOR GHOST MODE starts first.			
		leftVUKLogic = 1
		Exit Function
	End If
	if (hotProgress(player) = 20)	Then							'Searching for the Control Box?
		BoxCheck(1)													'Check / flag box for this location
		KickLeft 11000, vukPower									'Roll the ball back down
		leftVUKLogic = 1
		Exit Function
	End If
	if (hotProgress(player) = 30 or hotProgress(player) = 35) Then	'Hotel ghost battle?
		KickLeft 11000, vukPower									'Kick it out
		leftVUKLogic = 1
		Exit Function
	End If
	if (Mode(player) = 7) Then										'Are we in Ghost Photo Hunt?
		photoCheck(1)
		KickLeft 11000, vukPower
		leftVUKLogic = 1
		Exit Function
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then	'Theater Ghost?
		if (theProgress(player) = 11) Then							'Waiting for Shot 2, in which case this shot is CORRECT?
			TheaterPlay(1)											'Advance the play!
			leftVUKLogic = 0
			Exit Function											'No combo
		Else			
			TheaterPlay(0)											'Incorrect shot, ghost will bitch!
			KickLeft 7000, vukPower									'Kick it out quick
			leftVUKLogic = 1
		Exit Function
		End If
	End If
	comboVideoFlag = 0												'Nothing active? Reset video combo flag	
	AddScore(5000)													'Some points
	KickLeft 7000, vukPower											'The default	
	'Nothing going on default prompt
	video "C", "G", "E", allowSmall, 39, 250						'Regular Combo to the Right ->	
	playSFX 2, "A", "Z", "Z", 255									'Whoosh!
	leftVUKLogic = 1
	Exit Function													'Can combo
End Function

sub lightningFX(lightStage)
	lightningGo = 1
	'Creates a lightning effect using a number from 0-10000
	'Send it a lobbed off modeTimer value to add lightning to speech calls
	if (lightStage < Int(27000/CycleAdjuster)) Then
		Select Case (lightStage)
			case Int(100/CycleAdjuster):
				if (random(10) < 5) Then
					cabLeft 0, 0, 0				
					BackGlassOff(1)
				Else	
					cabRight 0, 0, 0	
					BackGlassOff(3)
				End If				
				doRGB()			
			case Int(3000/CycleAdjuster):
				GIpf(192)		
			case Int(3500/CycleAdjuster):
				lightningPWM = 0
				cabLeft 0, 0, 0
				cabRight 0, 0, 0
				BackGlassOff(2)
				doRGB()		
			case Int(4000/CycleAdjuster):
				GIpf(128)		
			case Int(5000/CycleAdjuster):
				GIpf(0)		
			case Int(19000/CycleAdjuster):
				cabLeft 0, 0, 20
				SetBackGlass 1, "blue"
				cabRight 0, 0, 0
				BackGlassOff(3)
				doRGB()	
				GIpf(224)
				light 40, 1
				light 41, 1
				light 42, 1
				backglass 0, 1			
			case Int(21000/CycleAdjuster):
				cabLeft 0, 0, 20
				SetBackGlass 1, "blue"
				cabRight 0, 0, 0
				BackGlassOff(3)
				doRGB()
				GIpf(96)
				light 40, 1
				light 41, 1
				light 42, 0	
				backglass 1, 0		
			case Int(22000/CycleAdjuster):
				cabLeft 0, 0, 0
				BackGlassOff(1)
				cabRight 0, 0, 10
				SetBackGlass 2, "blue"
				doRGB()	
				GIpf(32)
				light 40, 1
				light 41, 0
				light 42, 0
				backglass 1, 0		
			case Int(24000/CycleAdjuster):
				cabLeft 0, 0, 10
				SetBackGlass 1, "blue"
				cabRight 0, 0, 0
				BackGlassOff(3)
				doRGB()	
				GIpf(0)			
				light 40, 0
				light 41, 0
				light 42, 0	
				backglass 0, 1			
			case Int(25000/CycleAdjuster):
				cabLeft 0, 0, 0
				BackGlassOff(1)
				cabRight 0, 0, 5
				SetBackGlass 2, "blue"
				doRGB()
				backglass 1, 0		
			case Int(25200/CycleAdjuster):
				cabLeft 0, 0, 5
				SetBackGlass 1, "blue"
				cabRight 0, 0, 0
				BackGlassOff(3)
				doRGB()
				backglass 0, 1		
			case Int(26000/CycleAdjuster):
				lightningEnd(50)
		End Select		
		if (lightStage > Int(6000/CycleAdjuster) and lightStage < Int(19000/CycleAdjuster)) Then					'Turn off both RGB's and flash the inserts all BRIGHT
			lightningPWM  = lightningPWM  +  1
			'Serial.println lightningPWM, DEC
			if (lightningPWM = Int(400/cycleAdjuster)) Then	
				cabLeft 200, 200, 255
				SetBackGlass 1, "white"
				cabRight 0, 0, 0
				BackGlassOff(3)
				doRGB()			
				GIpf(160)	
				light 40, 0
				'light 41, 1
				light 42, 0
			End If
			if (lightningPWM > Int(800/cycleAdjuster)) Then	
				lightningPWM = 0
				cabLeft 0, 0, 0
				BackGlassOff(1)
				cabRight 200, 200, 255
				SetBackGlass 2, "white"
				doRGB()			
				GIpf(64)			
				light 40, 1
				'light 41, 0
				light 42, 1							
			End If
		End If
	End If
	if (lightStage > Int(49999/cycleadjuster) and lightStage < Int(99999/cycleadjuster)) Then
		Select Case (lightStage)
			case Int(50005/cycleAdjuster+5):
				cabColor 0, 0, 0, 0, 0, 0
				BackGlassOff(2)
				GIpf(224)
				doRGB()						
			case Int(50500/cycleAdjuster):
				cabColor 0, 0, 64, 0, 0, 64
				SetBackGlass 2, "blue"
				SetBackGlass 1, "blue"
				GIpf(0)
				doRGB()						
			case Int(51000/cycleAdjuster):
				cabColor 0, 0, 128, 0, 0, 128
				SetBackGlass 2, "blue"
				SetBackGlass 1, "blue"
				GIpf(224)
				doRGB()						
			case Int(51500/cycleAdjuster):
				cabColor 0, 0, 0, 0, 0, 0
				BackGlassOff(2)
				GIpf(0)
				doRGB()						
			case Int(52000/cycleAdjuster):
				cabColor 0, 0, 64, 0, 0, 64
				SetBackGlass 2, "blue"
				SetBackGlass 1, "blue"
				GIpf(224)
				doRGB()						
			case Int(52500/cycleAdjuster):
				cabColor 0, 0, 128, 0, 0, 128
				SetBackGlass 2, "blue"
				SetBackGlass 1, "blue"
				GIpf(0)
				doRGB()						
			case Int(53000/cycleAdjuster):
				cabColor 0, 0, 0, 0, 0, 0
				BackGlassOff(2)
				GIpf(224)
				doRGB()						
			case Int(53500/cycleAdjuster):
				cabColor 0, 0, 64, 0, 0, 64
				SetBackGlass 2, "blue"
				SetBackGlass 1, "blue"
				GIpf(0)
				doRGB()						
			case Int(55000/cycleAdjuster):
				cabColor 255, 255, 255, 255, 255, 255
				SetBackGlass 2, "white"
				SetBackGlass 1, "white"
				setCabColor 0, 0, 0, 20				
			case Int(57000/cycleAdjuster):
				lightningEnd(10)						
		End Select		
	End If
if (lightStage > Int(99999/cycleadjuster) and lightStage < Int(120000/CycleAdjuster)) Then
		lightningPWM  = lightningPWM  +  1
		if (lightningPWM = Int(300/cycleAdjuster)) Then	
			cabLeft 255, 0, 0
			SetBackGlass 1, "red"
			cabRight 0, 0, 0
			BackGlassOff(3)
			doRGB()			
			GIpf(160)
		End If
		if (lightningPWM > Int(600/CycleAdjuster)) Then	
			lightningPWM = 0
			cabLeft 0, 0, 0
			BackGlassOff(1)
			cabRight 255, 0, 0
			SetBackGlass 2, "red"
			doRGB()			
			GIpf(64)									
		End If
		if (lightStage = Int(119999/cycleadjuster)) Then			'Fade to black complete? Fade back up to mode color
			lightningEnd(25)			
		End If			
	End If
End Sub

sub lightningEnd(resumeSpeed)
	lightningTimer = 0								'Finish cycle
	backglass 1, 1
	lightningGo = 0								'Effect is done!
	flashCab 0, 0, 0, resumeSpeed					'Fade back into normal color from Black
	GIpf(224)
End Sub

sub lightningStart(theValue)
	if (lightningTimer = 0) Then
		lightningTimer = theValue
	End If
End Sub

sub lightningKill()
	lightningTimer = 0															'Finish cycle
	backglass 1, 1
	lightningGo = 0																'Effect is done!
	GIpf(224)
End Sub

sub loadBall()
	launchCounter = 0
	if (Sw57 = 0) Then 							'No ball in shooter lane?
		AutoPlunge(25005)
	Else
		'Serial.print("BALL ALREADY LOADED: ")
		plungeTimer = Int(24000/CycleAdjuster)		'Manually set plunge timer to skip ball loading part
	End If
End Sub

sub logic()																	'This doesn't run if we're in a Ball Drain.
	if (loopCatch = ballCaught) Then										'Trying to catch a ball under ghost? Check these conditions:
		if (barProgress(player) = 60) Then									'Ball caught by ghost?
			loopCatch = 0													'Clear this
			BarTrap()														'Proceed
		End If
		if (deProgress(player) = 2) Then									'Second ball caught?
			loopCatch = 0													'Clear this
			DemonLock2()													'Proceed
		End If	
		if (minionMB = 20) Then												'Minion Multiball, ready to catch ball?
			loopCatch = 0
			minionMBtrap()	
		End If
		if (deProgress(player) = 20) Then									'Final shot to demon, and ball was caught?
			loopCatch = 0
			DemonWin()														'You won the game!
		End If
		if (videoMode(player) = 1) Then	
			if (barProgress(player) <> 60 and Advance_Enable = 1 and minion(player) <> 10) Then
				loopCatch = 0
';				runVideoMode()												'EP- disabling for now until I can figure out how to do a video mode
			End If
		End If
	End If
	if (minion(player) = 100 and modeTimer > Int(99999/cycleadjuster) and popsTimer = 0) Then		'Minion MB, and a player has trapped a ball? (not the one trapped on mode start)
		modeTimer = modeTimer + 1
		if (modeTimer > (Int(Int(100000/CycleAdjuster)/cycleAdjuster) + longSecond)) Then
			modeTimer = Int(Int(100000/CycleAdjuster)/CycleAdjuster)
			countSeconds = countSeconds - 1									'Subtract!
'			numbers 0, numberStay OR 4, 0, 0, countSeconds - 1)				'Update the Numbers Timer.	EP- I don't know what this is supposed to be updating, maybe I'll figure it out later
			if (countSeconds > 1 and countSeconds < 7) Then
				playSFX 2, "A", "M", 47 + countSeconds, 1					'Hurry-Up countdown
			Else
				playSFX 2, "Y", "Z", "Z", 1									'Beeps
			End If
			if (countSeconds = 0) Then										'Time's up! Kill timer number, release ball (no jackpot)
				minionMBjackpot(1)
			End If
		End If
	End If
	if (hellMB and modeTimer < Int(80000/CycleAdjuster)) Then				'Modetimer stays under 99999 in case Minion is stacked with Hell MB
		modeTimer = modeTimer + 1
		if (modeTimer > Int(69999/cycleadjuster)) Then						'A large range, to ensure the lights get activated properly
			modeTimer = Int(80000/CycleAdjuster)							'Should stop it from progressing
			lightSpeed = 1
			flashCab 0, 0, 0, 25
			if (Mode(player) = 0) Then										'Not in a mode?
				showProgress 1, player										'Show the progress, Active Mode style
			End If
			minionLights()	
			popLogic(0)														'Figure out what the pops should be doing		
			'BLINK THE MINION LIGHTS
			tourReset(58)											'Tour: Left orbit, door VUK, up middle, right orbit (excludes Hotel and Scoop)
			blink(24)														'Call button light status		
			light 25, 7														'Current state is HIT TO GO UP	
			blink(41)														'Blink the hellavator flasher
			strobe 26, 5													'Strobe all lights on that shot except Camera (since it's used for combos)
			blink(49)														'Blink the Multiball Progress light				
		Else
			Select Case (modeTimer)
				case Int(8840/CycleAdjuster):
					cabColor 35, 0, 35, 35, 0, 35
					setcabColor 0, 0, 0, 50
					allLamp(2)
					Exit Sub
				case Int(10840/CycleAdjuster):
					allLamp(0)
					GIpf(96)
					Exit Sub
				case Int(23870/CycleAdjuster):
					cabColor 70, 0, 70, 70, 0, 70
					setcabColor 0, 0, 0, 50
					allLamp(3)
					Exit Sub
				case Int(25870/CycleAdjuster):
					allLamp(0)
					GIpf(64)
					modeTimer = 28000
					Exit Sub
				case Int(38700/CycleAdjuster):
					cabColor 105, 0, 105, 105, 0, 105
					setcabColor 0, 0, 0, 50
					allLamp(4)
					Exit Sub
				case Int(40700/CycleAdjuster):
					allLamp(0)
					GIpf(0)
					modeTimer = 43000
					Exit Sub
				case Int(46400/CycleAdjuster):
					cabColor 140, 0, 140, 140, 0, 140
					setcabColor 0, 0, 0, 50
					allLamp(5)
					Exit Sub
				case Int(48400/CycleAdjuster):
					allLamp(0)
					GIpf(0)					
					Exit Sub				
				case Int(53780/CycleAdjuster):
					cabColor 175, 0, 175, 175, 0, 175
					setcabColor 0, 0, 0, 50
					allLamp(7)
					Exit Sub
				case Int(55780/CycleAdjuster):
					allLamp(0)
					GIpf(0)					
					Exit Sub				
				case Int(57420/CycleAdjuster):
					cabColor 210, 0, 210, 210, 0, 210
					setcabColor 0, 0, 0, 50
					allLamp(5)				
					Exit Sub			
				case Int(59420/CycleAdjuster):
					cabColor 255, 0, 255, 255, 0, 255
					doRGB()
					lightSpeed = 5
					strobe 3, 5				'Strobe EVERYTHING!
					strobe 8, 7
					strobe 20, 4
					strobe 26, 6
					strobe 36, 4
					strobe 43, 5
					strobe 56, 7				
					Exit Sub
				case Int(60000/CycleAdjuster):
					cabColor 0, 0, 0, 0, 0, 0
					doRGB()
					GIpf(128)					
					Exit Sub
				case Int(60500/CycleAdjuster):
					cabColor 255, 0, 255, 0, 0, 0
					doRGB()
					GIpf(64)					
					Exit Sub
				case Int(61000/CycleAdjuster):
					cabColor 0, 0, 0, 0, 0, 0	
					doRGB()
					GIpf(32)					
					Exit Sub		
				case Int(61500/CycleAdjuster):
					cabColor 0, 0, 0, 255, 0, 255
					doRGB()	
					GIpf(128)					
					Exit Sub		
				case Int(62000/CycleAdjuster):
					cabColor 0, 0, 0, 0, 0, 0
					doRGB()
					GIpf(64)					
					Exit Sub
				case Int(62500/CycleAdjuster):
					cabColor 255, 0, 255, 255, 0, 255
					doRGB()
					GIpf(32)					
					lightSpeed = 10
					Exit Sub
				case Int(63000/CycleAdjuster):
					cabColor 0, 0, 0, 0, 0, 0
					doRGB()	
					GIpf(128)					
					Exit Sub
				case Int(64500/CycleAdjuster):
					cabColor 255, 0, 255, 255, 0, 255
					doRGB()
					GIpf(64)					
					Exit Sub	
				case Int(65000/CycleAdjuster):
					cabColor 0, 0, 0, 0, 0, 0
					doRGB()	
					GIpf(32)					
					Exit Sub
				case Int(65500/CycleAdjuster):
					cabColor 255, 0, 255, 255, 0, 255
					doRGB()
					GIpf(128)					
					Exit Sub
				case Int(66000/CycleAdjuster):
					cabColor 0, 0, 0, 0, 0, 0
					doRGB()	
					GIpf(192)					
					Exit Sub
				case Int(66500/CycleAdjuster):
					cabColor 255, 0, 255, 255, 0, 255
					doRGB()
					GIpf(224)
					allLamp(0)				
					Exit Sub											
			End Select
		End If	
	End If
	if (Mode(player) = 7 or Mode(player) = 99) Then				'In GHOST PHOTO HUNT, or FINAL FLASH?
		photoLogic()
	End If
	if (multiBall) AND (multiballLoading) Then					'Multiball ready, loading balls onto play?
		multiTimer = multiTimer - 1
		if (multiTimer = 1) Then								'Just about done?
			AutoPlunge(autoPlungeFast)							'Auto launch a ball.
			ballSave()											'Enable ball save
			multiTimer = autoPlungeFast + 100 					'Spit out another ball pretty quickly
			multiCount = multiCount - 1							'Decrement count
			if (multiCount = 0) Then							'Did we kick out all balls requested?
				multiBall =(multiBall AND 254) 					'EP- 254 is NOT multiballLoading		'Clearing loading bit
				multiBall = multiBall OR multiballLoaded		'Change multiball condition to "All Balls Ejected"
				multiTimer = 0									'Cancel timer
			End If
		End If
	End If
	if (wiki(player) = 255 and tech(player) = 255 and psychic(player) = 255) Then			'All 3 team members SPELLED?	
		pulse(0)				'Pulse lights again
		pulse(1)
		pulse(51)
		wiki(player) = 0
		tech(player) = 0
		psychic(player) = 0
		if (videoModeEnable) Then
			if (Advance_Enable and minion(player) < 10) Then	'Modes can be advanced, and a Minion isn't active?
				videoQ "S", "V", "A", allowSmall, 45, 250		'Video Mode Ready!
				videoMode(player) = 1							'Ready to collect
				loopCatch = catchBall							'Flag that we want to catch the ball in the loop	
				TargetTimerSet 10, TargetDown, 50				'Put targets down
				blink(17)
				blink(18)
				blink(19)
			Else
				videoQ "S", "V", "B", allowSmall, 45, 250		'Video Mode Ready After Mode Ends
				videoMode(player) = 10
			End If		
		Else
			AddScore(500000)									'If no VM, just give points
		End If
	End If
	if (Mode(player) = 1) Then									'Hospital Mode active?
		HospitalLogic()		
	End If
	if (Mode(player) = 3 or barProgress(player) = 60)	Then	'Bar mode active?
		BarLogic()
	End If
	if (Mode(player) = 4) Then									'Fighting the War Fort ghost?
		WarLogic()
	End If
	if (Mode(player) = 5) Then									'Hotel Mode active?
		HotelLogic()		
	End If
	if (Mode(player) = 6) Then									'Prison Mode active?
		PrisonLogic()
	End If
	if (ghostAction and ghostBored = 0) Then					'Flag to have the ghost be doing something?
		doGhostActions()
	End If
	if (ghostBored and ghostTimer = 0) Then						'Ghost turns back to center, unless there's a Ghost Move Timer going on
		ghostBored = ghostBored - 1
		if (ghostBored = Int(10000/CycleAdjuster)) Then			'Time up?
			ghostMove GhostLocation + 10, 5						'Ghost turns to the front.
		End If
		if (ghostBored = Int(7000/CycleAdjuster)) Then								'Time up?
			ghostMove GhostLocation - 10, 5						'Ghost turns to the front.
		End If
		if (ghostBored = Int(3000/CycleAdjuster)) Then								'Time up?
			ghostMove GhostLocation + 10, 5						'Ghost turns to the front.
		End If
		if (ghostBored = 1) Then								'Time up?
			ghostMove 90, 100									'Ghost turns to the front.
			ghostBored = 0
		End If
	End If
	if (minion(player) = 12) Then								'Waiting for targets?
		if (TargetLocation >= TargetUp) Then					'Did targets make it back up?
			minion(player) = 1									'NOW allow mode to start again
		End If
	End If
	if (goldHits = 10 and popsTimer = 0) Then					'Are we stealing gold during the War Fort mode?
		WarGoldLogic()	
	End If
if (restartTimer) Then
		if (hosProgress(player) = 3 and modeTimer) Then			'Giving the ball back? Make sure players know flipper works!
			modeTimer = modeTimer - 1
			if (modeTimer >= Int(11000/CycleAdjuster) and modeTimer <= Int(11100/CycleAdjuster)) Then
				LeftFlipper.RotateToEnd
				PlaySound SoundFX("FlipperUp"),0,0.5,-0.1,0.25
			End If
			if (modeTimer >= Int(10500/CycleAdjuster) and modeTimer <= Int(10600/CycleAdjuster)) Then
				LeftFlipper.RotateToStart
				PlaySound SoundFX("FlipperDown"),0,0.5,-0.1,0.25
			End If
			if (modeTimer >= Int(8000/CycleAdjuster) and modeTimer <= Int(8100/CycleAdjuster)) Then
				LeftFlipper.RotateToEnd
				PlaySound SoundFX("FlipperUp"),0,0.5,-0.1,0.25
			End If
			if (modeTimer >= Int(4000/CycleAdjuster) and modeTimer <= Int(4100/CycleAdjuster)) Then
				LeftFlipper.RotateToStart
				PlaySound SoundFX("FlipperDown"),0,0.5,-0.1,0.25
			End If		
		End If
	End If	
End Sub

sub MachineReset()
	Dim X
'	Serial.println("System Reset")
	cabColor 32, 32, 32, 32, 32, 32					'Dim lighting
	setGhostModeRGB 0, 0, 0							'Set ghost to off
	ghostColor 0, 0, 0
	allLamp(0)										'Turn off lamps
'	myservo(Targets).write(TargetDown)				'Put targets down
	TargetSet TargetDown
'	myservo(HellServo).write(hellDown) 				'Hellavator down
	ElevatorSet hellDown, 600
'	myservo(DoorServo).write(DoorOpen)	 			'Open Door
	DoorSet DoorOpen, 600
'	myservo(GhostServo).write(90) 					'Center Ghost
	GhostMove 90, 600
'	for x=0 to 4									'Send the high scores to display (in case they changed, or at start of game)
'		sendHighScores(x)
'	End If
	lightSpeed = 1									'Speed at which the lights change
'	Update(startingAttract)							'Set attract mode to ON (also sets Freeplay, number credits)
	AttractLights = 1								'EP- Since Update doesn't do anything, have to set the variable manually
End Sub

sub MagnetSet(setMagTimer)
'	Coil(Magnet, 255)									'Magnet on for 100ms to catch ball
	DOF 132, 2
	mMagnaSave.MagnetOn = 1
'**	mMagnaSave.GrabCenter = 1
	magFlag = magFlagTime								'This is how many MS to pulse it 100 times a second. If it exceeds 10, magnet will be "solid on"
	MagnetTimer = 50*Int(setMagTimer/CycleAdjuster)		'Set total time EP- multiplied by 50, may need to adjust this more
'**	If MagnetTimer < 10 Then MagnetTimer = 10
	MagnetCount = 0										'Reset PWM counter
End Sub

'FUNCTIONS FOR MINION GHOST............................
sub minionStart()
	AddScore(startScore)
	setGhostModeRGB 128, 128, 128							'Set ghost to white
	setCabModeFade 0, 0, 255, 200							'Light the cabinet BLUE
	minion(player) = 10										'Set flag that we've started
	playSFXQ 0, "M", "D", random(10), 255					'Something Something Specter!
	if (minionHitProgress(player)) Then						'Have we already attacked this class Minion?
		minionHits = minionHitProgress(player)				'Set how many hits we have left
	Else													'If not, old school just get next value for that particular class minion  3 hits, 4 hits, etc
		minionHits = minionTarget(player)					'Count DOWN instead of UP to make it easier to pick video
	End If
	minionHitProgress(player) = 0							'Clear the flag no matter what
	if (hellMB = 0) Then
		musicplayer "bgout_MI.mp3"								'Minion music, unless a full Hellavator MB has started
		modeTotal = 0										'This will be only mode active, so reset mode total
		'No HMB active, so OK to show custom Minion Score
		customScore "M", "M", "Y", allowAll OR loopVideo, 120		'SHOOT GHOST!
'		numbers 8, numberScore OR 2, 0, 0, player			'Put player score upper left
		numbers PlayerScore(player), "", "", ""
'		numbers 9, 2, 122, 0, minionHits					'Hits to Go upper right
		numbers "", minionHits, "", ""
	End If
	'playSFX 1, 'M', 'C', '0', 255							'Ghost Minion Found Sound
	if (minionsBeat(player) > 8) Then
		playSFX 0, "M", "C", "9", 255						'You can keep beating them, but they only go up to Class 9
		video "M", "F", "9", allowSmall, 29, 250			'Ghost level cap at 9
	Else
		playSFX 0, "M", "C", 1 + minionsBeat(player), 255	'It's a class 1-9
		video "M", "F", 1 + minionsBeat(player), allowSmall, 29, 250		'Show which level ghost we're fighting
	End If
	strobe 17, 3											'Strobe the Ghost Targets
	if (minionHits <= minionDamage) Then												'Almost defeat? Will next hit kill it?				
		if (minionsBeat(player) = minionMB1 or minionsBeat(player) = minionMB2) Then	'Multiball Minion?
			if (hellMB) Then
				videoQ "M", "9", "1", 2, 38, 100			'Hit Ghost to STACK multiballs
			Else
				videoQ "M", "9", "0", 2, 38, 100			'Hit ghost for Multiball!	
			End If
		Else
			videoQ "M", "9", 64 + 1, 2, 30, 100				'1 hit to go!
		End If		
	Else
		videoQ "M", "9", 64 + minionHits, 2, 30, 100		'More than a single hit to beat minion? Show how many
	End If
	if ((minionsBeat(player) = minionMB1 or minionsBeat(player) = minionMB2) and hellMB = 0) Then	'Can this Minion Battle start a Minion MB, and Hell MB isn't active?
		hellEnable(0)										'Disable Hell Locks
	Else
		hellEnable(1)										'Else, enable them
	End If
	TargetTimerSet 10, TargetDown, 50						'Put targets back up, but not so fast ball is caught	
	ghostAction = Int(425005/CycleAdjuster)+5				'Ghost guarding	
	GLIRenable(0)											'Fighting a minion, you can't GLIR
End Sub

sub minionHitLogic()
	Dim mX, x
	minionHits  = minionHits  -  minionDamage
	lightningStart(Int(50000/CycleAdjuster))
	killQ()	'Prevents incorrect hit # from appearing
	if (minionHits > 0) Then								'Haven't won yet?
		ghostFlash(100)
		DOF 133, 2
		mX = 65 + random(10)								'Get a random ASCII letter A-J
		playSFX 1, "N", "8", mX, 255						'Sound to match animation
		video "M", "8", mX, allowSmall, 46, 255				'Ghost hit animation
		animatePF 104, 15, 0								'Minion hit, stuff flies off
		if (hellMB = 0) Then
'			numbers 9, 2, 122, 0, minionHits				'Updated Hits to Go upper right					
			numbers "", minionHits, "", ""
		End If		
		if (minionHits <= minionDamage) Then				'Almost defeat? Will next hit kill it?				
			if (minionsBeat(player) = minionMB1 or minionsBeat(player) = minionMB2) Then	'Multiball Minion?
				playSFX 0, "M", "D", 70 + random(3), 255	'Hit the ghost for Multiball! (has a gap so it plays after Whack Sound. Pre-dates playSFXQ command
				if (hellMB) Then
					videoQ "M", "9", "1", 2, 38, 100		'Hit Ghost to STACK multiballs
				Else
					videoQ  "M", "9", "0", 2, 38, 100		'Hit ghost for Multiball!	
				End If
			Else											'Normal minion
				videoQ "M", "9", 64 + minionHits, 2, 30, 100		'How many hits to go
				playSFXQ 1, "M", "D", 65 + random(5), 255	'"Let's finish him off!" same channel as Whack Sound, will play after it's done
			End If
		Else
			videoQ "M", "9", 64 + minionHits, 2, 30, 100		'How many hits to go				
		End If
		ghostAction = Int(468005/CycleAdjuster)+5			'Minion hit, leading into guarding motion	
		AddScore(minionTarget(player) * 50000)				'Add score
		if (minionsBeat(player) > 3) Then					'The more difficult minions? Magnet Fun!
			x = minionsBeat(player) - 3						'Reduce range	
			if (x > 9) Then									'Limit it from 1 to 9
				x = 9
			End If
'			Coil Magnet, 50 + (x * 20)						'Magnet pulse! Stronger the more minions you fight
			MagnetSet(x*100)
		End If
	Else
		ghostAction = Int(468005/CycleAdjuster)+5			'Minion hit, leading into guarding motion	
		if (minionsBeat(player) = minionMB1 or minionsBeat(player) = minionMB2) Then	'Multiball Minion?
			minionMB = 1									'Flag saying Multiball can start						
			if (minionsBeat(player) = minionMB2) Then			'Also, starting the second Minion MB also lights EXTRA BALL
				extraBallLight(2)							'Light extra ball, no prompt we'll do there	
			End If		
		End If				
		if (minionMB = 1) Then								'Multiball flag set?
			minionMultiballStart()
		Else
			minionWin()										'Normal Minion End
		End If
	End If
End Sub

sub minionLights()						'Set Minion Lights to whatever they should be
	if (minion(player) = 0)	Then		'Can't advance minions?
		Exit Sub						'Do nothing
	End If
	if (minion(player) = 10) Then		'Fighting a Ghost Minion?
		strobe 17, 3				'Strobe his lights
		light 16, 0				'Make sure JACKPOT is off
		Exit Sub	
	End If
	pulse(17)						'Pulse all 3 by default (they're at least lit)
	pulse(18)
	pulse(19)
	if (minionsBeat(player) < 3) Then		'First 3 minions, where you can just hit any targets 3 times?
		pulse(17)						'Pulse all 3 by default (they're at least lit)
		pulse(18)
		pulse(19)
		if (minionHits = 2) Then			'Make lights solid to count how many we've hit
			light 19, 7
		End If
		if (minionHits = 1) Then
			light 18, 7
			light 19, 7
		End If		
	Else								'Otherwise it's a level 4+, meaning targets have to be hit individually
	
		light 17, 7						'Start with them on (lit then pulse whichever ones we haven't hit it
		light 18, 7
		light 19, 7
		if ((targetBits AND 4)=4) Then
			pulse(17)
		End If
		if ((targetBits ANd 2)=2) Then
			pulse(18)
		End If
		if ((targetBits AND 1)=1) Then
			pulse(19)
		End If		
	End If
End Sub

sub minionWin()
	DOF 134, 2
	'ghostAction = 0
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFlash(300)												'Flash minion, fade to black
	ghostMove 90, 100
	blink(17)													'Blink targets during kill animation
	blink(18)													'They'll get changed to PULSE after ball release
	blink(19)
	light 16, 0													'Turn off MAKE CONTACT
	MagnetSet(350)												'Catch ball
	minion(player) = 11											'Set flag for mode ending, to put targets up AFTER magnet release
	ghostsDefeated(player)  = ghostsDefeated(player)  +  1		'Keep track for bonuses
	minionsBeat(player)  = minionsBeat(player)  +  1			'Keep track for Multiball
	if (hellMB) Then
		setCabModeFade 255, 0, 255, 50							'Turn color back to Magenta
	End If
	if (minionsBeat(player) > 254) Then							'Got a kill screen coming up!
		minionsBeat(player) = 254
	End If
	AddScore(minionsBeat(player) * 100000)
	killQ()														'Disable any Enqueued videos
	playSFX 0, "M", "E", 65 + random(8), 255					'Sound for light-sucking, different "Minion Defeated" quotes
	video "M", "9", 88 + random(1), noExitFlush, 57, 255		'Ghost sucked into light!  M9X or M9Y, left or right flip
'	numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233		'Flash the total points scored in mode
	numbers "", "", modeTotal, modeTotal
	videoQ "M", "9", "Z", noEntryFlush OR 3, 45, 233			'Minion Mode Total:
	minionHits = 3												'3 hits to find another minion
	minionTarget(player)  = minionTarget(player)  +  1			'Increase the hits it takes	
	if (minionTarget(player) > 9) Then							'At limit?
		minionTarget(player) = 3								'Reset it
	End If
	GLIRenable(1)									'Re-enable GLIR
	animatePF 44, 30, 0							'Minion kill animation! (one shot
	if (hellMB = 0) Then
		killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
		killCustomScore()					
		modeTotal = 0													'Reset mode points				
	End If
End Sub

sub minionEnd(endType)
	ghostAction = 0
	light 17, 0																	'Turn off lights as default
	light 18, 0
	light 19, 0
	if (minionMB) Then
		light 7, 0																'Turn off Jackpot Lights
		light 39, 0
		minionMB = 0
	End If
	ghostAction = 0
	lightningKill()
	if (multiBall) Then
		cabModeRGB(0) = 255														'Manually set Cabinet RGB mode
		cabModeRGB(1) = 0
		cabModeRGB(2) = 255
		flashCab 255, 255, 255, 50												'Go back to Magenta color
	Else
		setCabModeFade defaultR, defaultG, defaultB, 200						'Reset cabinet color	
	End If
	targetReset()																'Reset target flags
	if (endType = 0 and minion(player) = 10) Then								'We are disabling Minions because a different mode started, but we were battling one when mode started?
		minionHitProgress(player) = minionHits									'Store the progress of how much we damaged him, so it's retrieved when next battle starts
	End If
	'This gets reset because you'll still have to hit targets to bring back the minion (though his "power" will be lower)
	if (endType = 3) Then
		if (minion(player) > 9 and minion(player) < 100) Then					'Were we fighting a standard minion when the ball drained?
			minionHitProgress(player) = minionHits								'Store the progress of how much we damaged him, so it's retrieved when next battle starts
		Else
			minionHitProgress(player) = 0										'Make sure this is clear
			minionHits = 3
		End If
		ghostModeRGB(0) = 0														'Fade out the minion
		ghostModeRGB(1) = 0
		ghostModeRGB(2) = 0
		ghostFadeTimer = Int(100/CycleAdjuster)
		ghostFadeAmount = Int(100/CycleAdjuster)
		TargetTimerSet 10000, TargetUp, 100										'Put the targets back up
		minion(player) = 1														'Set flag minion fight can be restarted once the player gets the ball back		
		pulse(17)																'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)
		killScoreNumbers()														'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
		killCustomScore()
		Exit Sub	
	End If
	minionHits = 3																'First 3 minions, this counts hits. Minions 3+, used to play incrementing target sound		
	if (endType = 1) Then														'Flag that mode ended with WIN + Reset targets?	
		minion(player) = 1														'Set flag minion fight can be restarted once the targets are back up
		if (videoMode(player) = 10 and hellMB = 0) Then							'If Hell MB not active, and Video Mode paused, re-enable it
			videoMode(player) = 1
			loopCatch = catchBall												'Flag that we want to catch the ball in the loop
		Else
			TargetTimerSet 9000, TargetUp, 10									'Put the targets back up		
		End If
		pulse(17)																'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)
		if (Mode(player) = 0) Then												'No main modes active, and 
			if (multiBall = 0) Then												'Hellavator Multiball not active?
'				playMusic "M", "2"												'Revert to normal music	
				musicplayer "bgout_M2.mp3"												'EP- PlayMusic is the method used to actually play music, so I can't define my own; I'll have to replace every instance
			End If	
		End If	
		if (Mode(player) = 7) Then
'			playMusic "H", "2"													'Photo hunt music		
			musicplayer "bgout_H2.mp3"
		End If
		Exit Sub
	End If
	if (endType = 2) Then														'Flag that mode ended with WIN? + Do not reset targets yet..
		minion(player) = 1														'Set flag minion fight can be restarted once the targets are back up
		pulse(17)																'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)		
		if (Mode(player) = 0) Then
'			playMusic "M", "2"													'Normal music		
			musicplayer "bgout_M2.mp3"
		End If
		if (Mode(player) = 7) Then
'			playMusic "H", "2"													'Photo hunt music
			musicplayer "bgout_H2.mp3"
		End If
		Exit Sub
	End If
	minion(player) = 0															'Set flag to disable Minion Battle
End Sub

sub minionMultiballStart()
	AddScore(150000)
	comboKill()									'So combo lights don't appear after the mode
	if (hellMB = 0) Then						'Hell MB isn't in progress?
'		playMusic 'M', 'M'						'Only need to switch to MB music if not stacked with Hell MB		
		musicplayer "bgout_MB.mp3"					'EP- This is exactly the same as MM.mp3... I don't know why there's two
		storeLamp(player)						'Store the state of the Player's lamps. If HellMB active, this has already been done
		allLamp(0)								'Turn off the lamps so we can repaint them		
		modeTotal = 0							'If Hell MB was active it already did this, so we don't want to erase that value unless Hell MB didn't start	
	End If
	spiritGuideEnable(0)						'No spirit guide during Hospital
	dirtyPoolMode(0)							'We want to trap balls!
	'PAINT LAMPS HERE! (double check)
	strobe 3, 5									'Strobe left orbit
	strobe 36, 4								'Strobe right orbit
	pulse(16)									'Pulse JACKPOT
	light 17, 0									'Turn off lights
	light 18, 0
	light 19, 0	
	blink(2)									'Blink MINION MASTER progress light
	ghostLook = 0								'Ghost doesn't look around
	ghostAction = Int(110000/CycleAdjuster)		'Ghost guarding ball
	minion(player) = 100						'Set flag that we're in Minion Multiball Mode
	minionMB = 10								'Flag that says a Ball is trapped under the ghost!
	Advance_Enable = 0							'Disable mode advancement
	setCabModeFade 0, 0, 255, 50				'Bright blue!
	minionJackpot = 100000						'Starting Jackpot value
	if (hellMB = 0) Then
		manualScore 5, minionJackpot				'Store jackpot in Score #5
		customScore "M", "M", "W", allowAll OR loopVideo, 60	'Ghost Lites, Orbits Build Jackpot
'		numbers 8, numberScore OR 2, 0, 0, player	'Put player score upper left
		numbers PlayerScore(player), "", "", ""
'		numbers 9, 9, 88, 0, 0					'Ball # upper right
		numbers "", Ball, "", ""
'		numbers 10, numberScore OR 2, 68, 27, 5	'Use Score #5 to display the Minion Jackpot Value	
';		EP- Not sure what to put here
	End If
	killQ()									'Disable any Enqueued videos	
	video "M", "M", "1", allowSmall, 87, 255	'Minion MB Start!
	playSFX 0, "M", "M", "D", 255				'Ghost Minion Multiball!
	MagnetSet(100)								'Hold the ball.
	TargetSet(TargetUp)						'Trap it using the targets
	trapTargets = 1							'Ball should be trapped behind targets
	DoorSet DoorOpen, 25						'Make sure the door is open		
	'NEEDS IF STILL LOADING CONDITION!
	if (multiBall) Then												'If a MB is active, it has to be Hellavator MB from Mode 0
		if (multiBall AND multiballLoading) Then						'It can be in 2 states - active  2 balls or more), or still loading its balls (unlikely BUT POSSIBLE
			multiCount = multiCount  +  1								'If it's still loading balls, then 3 will be active. We can only load 1 more.
			multiBall = multiBall OR multiballMinion					'Set Minion MB bit
		Else														'Much more likely that HellMB is already underway
			multiBall = multiBall OR (multiballMinion OR multiballLoading)		'Set Minion MB bit, and Ball Loading bit since we need more balls
			multiTimer = 10										'Next ball will be kicked out pretty quickly
			multiCount = (4 - activeBalls)					'Figure out how many balls we can add. 2 balls active = add 2. 3 balls active = add 1
			if (multiCount <> countBalls()) Then				'If this number doesn't match # of balls in the trough...
				multiCount = countBalls()	 				'then set the value to how many balls are actually in the trough
			End If		
		End If
		tourClear()											'Turn off the Tour Ghost Catch lights		
		hellEnable(1)											'Enable this so Hell Jackpots can still be collected
	Else
		multiBall = multiBall OR (multiballMinion OR multiballLoading)		'Set Minion MB bits, and Ball Loading bit
		multiTimer = 10										'Next ball will be kicked out pretty quickly
		multiCount = 2									'We'll add 2 balls
		if (countBalls() < multiCount) Then				'If, somehow, there is less than 2 balls in trough...
			multiCount = countBalls()	 				'then set the value to how many balls are actually in the trough
		End If	
		hellEnable(0)											'Can't stack the Hell MB on Minion MB (only the other way around)
	End If
	popLogic(0)												'Figure out what the pops should be doing
	showProgress 1, player									'Show the Main Progress lights (not each mode's advancement
	modeTimer = Int(99999/CycleAdjuster)							'Prevents timer from starting until second ball trapped
End Sub

sub minionMBtrap()
	ghostLook = 0								'Ghost doesn't look around
	ghostAction = Int(110000/cycleAdjuster)		'Ghost guarding ball
	setCabModeFade 0, 0, 128, 50				'Medium Blue
	video "M", "M", "7", 0, 27, 255				'Ghost catches ball (don't show numbers yet...	
	modeTimer = Int(100000/cycleAdjuster)		'Setup timer
	countSeconds = 13							'10 seconds to collect jackpot
';	numbers 0, numberStay OR 4, 0, 0, countSeconds - 1				'Update the Numbers Timer.	
	videoQ "M", "M", "8", allowSmall OR noEntryFlush, 29, 255	'Can you collect Jackpot in time? Arrows to numbers
	if (hellMB = 0) Then
		customScore "M", "M", "X", allowAll OR loopVideo, 60	'Ghost Lites, Orbits Build Jackpot
	End If
	playSFX 0, "M", "M", 65 + random(2), 255	'Ghost catch & chuckle
	minionMB = 10								'Flag that says a Ball is trapped under the ghost!
	MagnetSet(150)								'Hold the ball.
	TargetSet(TargetUp)						'Trap it using the targets
	trapTargets = 1
	pulse(16)									'Pulse JACKPOT
	light 17, 0								'Turn off lights
	light 18, 0
	light 19, 0	
	'videoQ 'M', 'M', '2', allowSmall, 0, 255)	'Collect Jackpot Countdown! (arrows point at timers
End Sub

sub minionMBjackpot(timeRelease)
	killNumbers()								'Kill any numbers that are flashing
	killTimer(0)								'Kill the timer (either we got jackpot or it timed out doesn't matter)
	modeTimer = Int(99999/CycleAdjuster)		'Prevents timer from showing until next target trapped	
	ghostLook = 1								'Ghost CAN look around
	ghostAction = 0							'Ghost guarding ball
	cabColor 0, 0, 0, 0, 0, 0					'Set cab DARK...
	setCabModeFade 0, 0, 255, 50				'..and flash it back to BRIGHT BLUE
	if (hellMB = 0) Then
		customScore "M", "M", "W", allowAll OR loopVideo, 60	'Ghost Lites, Orbits Build Jackpot
	End If
	if (timeRelease = 0) Then
		video "M", "M", "3", allowSmall, 20, 255		'Minion Jackpot! The really fancy animation
		playSFX 0, "M", "M", 73 + random(3), 255	'Jackpot!	
		showValue (minionJackpot +  countSeconds * 50000), 40, 1			'You get jackpot value + seconds remaining bonus		
	Else										'Jackpot timed out - automatic ball release no points
		playSFX 2, "M", "Z", "Z", 255				'Sizzle sound with laugh
		video "M", "M", "9", allowSmall, 45, 255	'Minion Jackpot! The really fancy animation	
	End If
	minionMB = 20								'Flag that says a Ball has been freed!
	loopCatch = catchBall						'Set flag that we want to catch the ball
	TargetSet(TargetDown)						'Drop targets quickly
	trapTargets = 0
	light 16, 0								'Turn OFF jackpot
	pulse(17)									'Strobe target lights
	pulse(18)
	pulse(19)	
End Sub

sub minionJackpotIncrease()
	strobe 3, 5									'Strobe left orbit
	strobe 36, 4									'Strobe right orbit
	minionJackpot  = minionJackpot  +  100000						'Increase Jackpot
	manualScore 5, minionJackpot				'Store jackpot in Score #5
	playSFX 0, "M", "M", 5 + random(2), 255		'Jackpot increase sound, Male or Female scream	
	playSFXQ 0, "M", "M", 70 + random(3), 255	'Team leader random compliment
	if (hellMB) Then									'If stacked, video that explicitly says "Minion Jackpot" As if anyone would be watching DMD
'		numbers 7, numberFlash OR 1, 255, 11, minionJackpot	'Load Jackpot value Points as a number
		numbers "", "", minionJackpot, minionJackpot
		video "M", "M", "6", noEntryFlush OR 3, 30, 255	'Show new Jackpot value		
	Else
		video "M", "M", "5", allowSmall, 26, 255	'New Jackpot Value display
		showValue minionJackpot, 40, 0				'Show new value after video (but don't add it to score		
	End If
End Sub
'FUNCTIONS FOR MINION GHOST............................

sub modeAction()													'If MODETIMER set, check this logic
	if (deProgress(player) = 50) Then								'Ending credits?
		switchDead = 0												'Prevent ball search during credits
		modeTimer = modeTimer - 1
		if (modeTimer = Int(50000/CycleAdjuster)) Then				'Fade out music
			fadeMusic 1, 0
		End If	
		if (modeTimer = Int(10000/CycleAdjuster)) Then				'Release ball
			TargetSet(TargetDown)	
			setGhostModeRGB 0, 0, 0									'Turn off Ghost Color
		End If	
		if (modeTimer = 0) Then										'Restart player's game!
			stopMusic()
'			musicVolume(0) = 35										'Set back to normal
'			musicVolume(1) = 35
'			volumeSFX(3, musicVolume(0), musicVolume(1))	
			TargetTimerSet 1000, TargetUp, 1						'Put targets back up right away!			
			restartPlayer(player)
			light 63, 7												'DEMON BATTLE solid
		End If
	End If
	if (deProgress(player) = 9) Then								'Waiting for ball to clear targets?
		modeTimer = modeTimer - 1
		if (modeTimer = 1) Then		
			modeTimer = DoctorTimer									'Default time before target moves again			
			TargetTimerSet 10, TargetUp, 1							'Put targets back up right away!
			deProgress(player) = 10									'MB officially started
		End If				
	End If
	if (deProgress(player) = 10) Then								'Trying to WEAKEN the demon?
		modeTimer = modeTimer - 1
		if (modeTimer = 1) Then		
			modeTimer = DoctorTimer									'Default time before target moves again		
			playSFX 2, "A", "Z", "Z", 255							'MOVEMENT SOUND		
			DemonMove()
		End If		
	End If
	if (hosProgress(player) > 5 and hosProgress(player) < 9) Then
		modeTimer = modeTimer - 1
		if (modeTimer = 1) Then		
			DoorSet DoorClosed, 5									'Close door back up		
		End If
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100 and popsTimer = 0) Then									'Doing the THEATER GHOST PLAY?
		modeTimer = modeTimer - 1
		if (modeTimer = 1) Then
			modeTimer = longSecond									'Reset timer
			countSeconds = countSeconds - 1							'Reduce seconds left
			if (countSeconds = 0) Then								'Out of time?
				TheaterFail(0)										'Time's up, Fail mode, allow animation and speech		
			Else
'				numbers 0, numberStay | 4, 0, 0, countSeconds - 1)				'Update the Numbers Timer EP- Can't figure out what these do
				shotValue = shotValue - 10000 
'				numbers(9, 2, 70, 27, shotValue)					'Shot Value
				numbers "", shotValue, "", ""
				if (countSeconds > 1 and countSeconds < 7) Then	'Count down 5 4 3 2 1
					playSFX 2, "A", "M", 47 + countSeconds, 1
				Else
					playSFX 2, "Y", "Z", "Z", 1						'Beeps
				End If				
			End If
		End If
	End If
End Sub

Sub MoveDoor()				'EP- This is empty because it's handled by the WaDoor_timer sub
End Sub

Sub MoveEleveator()			'EP- This is empty because it's handled by the TiElevator_timer sub
End Sub

Sub MoveTarget()			'EP- This is empty because it's handled by the WaGhostTarget_timer sub
End Sub

sub multiBallStart(notRandomAward)
	restartKill 0, 0
	if (Mode(player) = 0) Then						'Not in a mode?
		Advance_Enable = 0							'Can't advance during multiball
		hellMB = 1									'Set flag that Hellavator MB has started
		catchValue = 1								'Cycle 1 of the Ghost Catch
		volumeSFX 3, 80, 80						'Temp higher volume music	
		modeTimer = 2000							'Let's do some wicked smart lighting!
		DoorSet DoorOpen, 500						'Open the door so we can shoot through it for Ghost Catch!	 (only if no Main Modes active
		ghostSet(140)
		ghostMove 90, 400
		if (videoMode(player) = 1) Then				'Video Mode was ready to start?
			videoMode(player) = 10					'Gonna have to wait
			loopCatch = 0
			TargetTimerSet 1000, TargetUp, 50		'Put targets back up manually
			minionEnd(2)							'Allow minions
		End If		
		if (minion(player) < 10) Then					'Minion or other mode isn't active?
			modeTotal = 0							'No other modes active, so reset mode total. If Minion is stacked with MB, the totals combine
		End If
		if (notRandomAward) Then
'			playMusic 'M', 'P'					'Also, only change music if we're not in a mode		
			musicplayer "bgout_MP.mp3"
		End If
		comboKill()
		storeLamp(player)							'Store the state of the Player's lamps
		allLamp(0)									'Turn off the lamps so we can repaint them		
		'PAINT LAMPS HERE!!!!!!!!!!
		blink(49)									'Blink the MB light during mode
		modeTotal = 0								'Since no mode is active, we can store a value for Hellavator mode
		cabModeRGB(0) = 255						'Manually set Cabinet RGB mode
		cabModeRGB(1) = 0
		cabModeRGB(2) = 255
		cabColor 0, 0, 0, 0, 0, 0					'Goto black quickly...
		doRGB()
	Else
		hellMB = 0
		blink(41)									'Blink the hellavator
		strobe 26, 5								'Strobe all lights on that shot except Camera (since it's used for combos
		blink(49)									'Blink the Multiball Progress light
		if (notRandomAward) Then
			playSFX 1, "Q", "A", "6", 255 			'Bells, beeps and ghost noises - non music version	
		End If
	End If
	multiBall = multiballLoading OR multiballHell	'Set Multiball loading flag (bit 0) and the flag that says Hell MB is active
	if (notRandomAward) Then
		multiTimer = Int(60000/CycleAdjuster)		'Set timer.
		playSFX 0, "Q", "A", "3", 255				'MULTIBALL!!! (syncs with music FX		
	Else
		multiTimer = Int(8000/CycleAdjuster)		'If Spirit Guide award, give next ball right away and don't say MULTIBALL!!!
	End If
	multiCount = 2									'We'll kick out 2 balls, for 3 total.
	if (countBalls() < multiCount) Then				'If, somehow, there is less than 2 balls in trough...
		multiCount = countBalls()	 				'then set the value to how many balls are actually in the trough
	End If
	hitsToLight(player)  = hitsToLight(player)  +  1						'Increase # of call button presses you'll need next time, max 4
	if (hitsToLight(player) > 4) Then					'Max out at 4 hits to light lock
		hitsToLight(player) = 4
	End If
	'hellEnable(0)									'Put elevator down, can't lock anymore
	if (hellMB) Then
		videoQ "Q", "A", "5", 0, 129, 200			'Ramp builds, Hellavator Collects, Flashing shots catch ghosts	
		manualScore 6, hellJackpot(player)		'Current Hell Jackpot Value
		customScore "Q", "B", "A", allowAll OR loopVideo, 90		'Custom Score: Hit ghost for JACKPOTS!
'		numbers 8, numberScore OR 2, 0, 0, player					'Put player score upper left
		numbers PlayerScore(player), "", "", ""
'		numbers 9, 9, 88, 0, 0										'Ball # upper right
		numbers "", Ball, "", ""
'		numbers 10, numberScore OR 2, 84, 27, 6						'Use Score #0 to display the Jackpot Value bottom off to right
		numbers "", "", "", hellJackpot(player)
	Else
		videoQ "Q", "A", "4", 0, 86, 200			'Ramp builds, Hellavator Collects (What we show when MB is stacked with mode - no ghost catching
	End If
End Sub

sub multiBallJackpotIncrease()
	AddScore(10710)											'Some points just for making the shot
	hellJackpot(player)  = hellJackpot(player)  +  250000								'Add 250k to player's jackpot value	
	manualScore 6, hellJackpot(player)						'Current Hell Jackpot Value
	killQ()
	'numbers 3, numberFlash OR 1, 255, 11, hellJackpot(player)	'Load Jackpot value Points as a number
	'video 'Q', 'J', 'C', noEntryFlush OR B00000011, 0, 255		'Show new Jackpot value
	video "Q", "J", "C", allowAll, 45, 255						'Show new Jackpot value
'	numbers 7, numberFlash OR 1, 255, 11, hellJackpot(player)	'Load Jackpot value Points as a number
';	numbers "", "", hellJackpot(player), hellJackpot(player)
	playSFX 2, "Q", "J", "C", 255								'Whooshing sound
	flashCab 128, 0, 128, 50
	strobe 26, 5												'Strobe first 5 lights	
End Sub

sub multiBallEnd( modeStacked)
	Dim endState
	loopCatch = 0												'Don't watch to catch balls!
	killQ()
	killNumbers()
	multiBall = multiBall AND 240								'We just want the MSB's that store MB states (mask off the Loaded flags)
	endState = 0
	'Multiball can end 5 ways:
	'1 = Hell MB, nothing else active
	'2 = Hell MB, during a mode (usually ends when mode does)
	'3 = Minion MB
	'4 = Hell MB stacked with Minion MB
	'5 = Hell MB, with a Minion active
	if (multiBall = multiballHell) Then									'It's either a plain HMB, or one stacked on another mode
		if (modeStacked or Mode(player)) Then
			endState = 2
		Else
			if (minion(player) > 9 and minion(player) < 100) Then
				endState = 5
			Else
				endState = 1			
			End If
		End If
	End If
	if (multiBall = multiballMinion) Then									'It was a Minion MB?
		endState = 3
	End If
	if (multiBall = (multiballMinion OR multiballHell)) Then				'Minion MB stacked onto Hell MB?
		endState = 4
	End If
	'Serial.print("ENDSTATE: ")
	'Serial.println(endState)
	'A/V functions only here (since we suppress those if TILT)
	if (tiltFlag = 0) Then													'If ended normally (not a tilt) restart lights and music as needed	
		if (endState = 1) Then
			video "Q", "Z", "Z", allowAll, 45, 255							'Multiball Mode Total:		
'			numbers 1, numberFlash OR 1, 255, 11, modeTotal				'Show Hell MB Mode Total Points
			numbers "", "", modeTotal, modeTotal
'			playMusic 'M', '2'											'Play the normal music	
			musicplayer "bgout_M2.mp3"
			setCabModeFade defaultR, defaultG, defaultB, 100			'Reset cabinet color (obviously don't want to do that if mode active
			killScoreNumbers()											'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
			killCustomScore()				
		End If
		if (endState = 4 or endState = 3) Then
			AddScore(minionsBeat(player) * 250000)								'If stacked, we end it as Minion MB End		
			playSFX 0, "M", "I", "W", 255										'Shortened version of Ghost Into Light	
			video "M", "9", 88 + random(1), noExitFlush, 57, 255				'Ghost sucked into light!  M9X or M9Y, left or right flip	
'			numbers 1, numberFlash OR 1, 255, 11, modeTotal					'Flash the total points scored in mode
';			numbers "", "", modeTotal, modeTotal
			videoQ "M", "M", "4" , noEntryFlush OR allowAll, 45, 255			'Minion MB Total:
			ghostsDefeated(player)  = ghostsDefeated(player)  +  1										'Keep track for bonuses
			minionsBeat(player)  = minionsBeat(player)  +  1											'Keep track for Multiball		
			minionTarget(player)  = minionTarget(player)  +  1											'Increase the hits it takes
			if (minionTarget(player) > 9) Then					'At limit?
				minionTarget(player) = 3					'Reset it
			End If			
'			playMusic 'M', '2'												'...But if in Minion MB, change music as that mode ends along with multiball
			musicplayer "bgout_M2.mp3"
			setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color (obviously don't want to do that if mode active
			killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
			killCustomScore()				
		End If	
		if (endState = 5) Then												'If normal, non-MB minion battle still active, resume Minion Music
			video "Q", "Z", "Z", allowLarge OR allowSmall, 45, 255			'Multiball Mode Total:		
'			numbers 1, numberFlash OR 1, 255, 11, modeTotal				'Show Hell MB Mode Total Points
			numbers "", "", modeTotal, modeTotal
'			playMusic 'M', 'I'											'Play the minion music
			musicplayer "bgout_MI.mp3"
			setCabModeFade 0, 0, 255, 50									'Light the cabinet BLUE quickly
			killScoreNumbers()												'Reset score numbers (so they don't bleed over into new display)
			customScore "M", "M", "Y", allowAll OR loopVideo, 120			'Change to MINION custom score
'			numbers 8, numberScore OR 2, 0, 0, player						'Put player score upper left
			numbers PlayerScore(player), "", "", ""
'			numbers 9, 2, 122, 0, minionHits								'Hits to Go upper right
			numbers "", minionHits, "", ""
		End If
	End If
	'OK now do the logic part of the modes ending
	if (Mode(player) = 0 and modeStacked = 0) Then				'Not in a mode or just coming out of one?
		Advance_Enable = 1										'Re-enable advance
		tourClear() 											'Clear the Ghost Catch lights
		targetReset()											'Reset Ghost target flags		
		ghostAction = 0										'Clear ghost movements (obviously don't want to do that if mode active)
		multipleBalls = 0
		loadLamp(player)										'Not sure if we need this?		
		showProgress 0, player								'Show all the Main Progress lights
		if (minion(player) < 10) Then								'If Minion isn't active, go ahead and reset mode points
			modeTotal = 0
		End If			
	End If
	if (multiBall AND multiballHell) Then							'Hell MB was active? Do stuff to end it.
		Dim x
		lockCount(player) = 0									'Reset these so we can start normal MB over again
		callHits = 0
		hellMB = 0												'It's over!
		light 49, 7											'Regular Multiball complete!
		if (Advance_Enable = 0) Then								'No modes active that might have Tour Shots?
			if (videoMode(player) and minion(player) <> 10) Then	'Video mode ready, and not fighting a minion?
';				videoModeLite()
			End If	
			for x=0 To 4											'Kill the camera lights
				light 26 + x, 0
			Next
		End If
	End If
	if (multiBall AND multiballMinion) Then							'Minion MB was active?
		minion(player) = 1										'Since we must have been in Mode 0 on entry, re-enable Minion
		pulse(17)												'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)		
		killTimer(0)											'Kill the Jackpot timer
		ghostModeRGB(0) = 0
		ghostModeRGB(1) = 0
		ghostModeRGB(2) = 0
		ghostFlash(300)										'Flash minion, fade to black
		trapTargets = 0										'No matter what, balls no longer trapped
		if (videoMode(player)) Then									'Video mode ready? Leave targets down at end
			videoModeLite()
		Else
			if (TargetLocation <> TargetDown or minionMB = 10) Then	'Targets up, or headed that way? A ball might be trapped behind them	
				TargetSet(TargetDown)								'Put them down...
				TargetTimerSet 10000, TargetUp, 10				'and after a second, put the back up
			Else
				TargetTimerSet 1000, TargetUp, 10					'Else, put them up immediately
			End If		
		End If
		light 7, 0											'Turn off jackpot lights
		light 39, 0
		light 16, 0											'Turn off MAKE CONTACT		
		light 2, 7											'Light MINION MASTER solid. No matter what you do, you've won the mode!
		minionMB = 0											'Clear the mode
		spiritGuideEnable(1)									'Allow Spirit Guide again
		minionHits = 3											'Set # of hits to 3 (for target sounds)
		if (barProgress(player) <> 70 and deProgress(player) <> 4 and minion(player) <> 10) Then							'Unless your friend trapped by a ghost, or Demon Advance...
			dirtyPoolMode(1)										'Don't want to trap balls anymore	
		End If
	Else	
		if (minion(player) <> 10) Then
			targetLogic(0)										'Not minion mode? See where the targets should be automatically (unless in a mode)
		End If
	End If
	'Stuff that happens no matter what
	multiBall = 0
	multiTimer = 0
	ElevatorSet hellDown, 100 								'Send Hellavator to 1st Floor.
	light 41, 0												'Hellavator flasher off
	light 25, 7												'Current state is SOLID
	blink(24)													'Other state BLINKS
	light 30, 0												'Lock is NOT lit
	light 31, 0												'Make sure camera isn't blinking either
	minionLights()								'See what they should be set at
	'checkModePost()							'We're going to do this manually
	doorLogic()								'Figure out what to do with the door
	checkRoll()								'See if we enabled GLIR Ghost Photo Hunt during that mode
	elevatorLogic()							'Did the mode move the elevator? Re-enable it and lock lights
	popLogic(0)								'Figure out what mode the Pops should be in
	GLIRenable(1)								'Re-enable GLIR (will also paint Scoop Lights for us)
 	if (Advance_Enable = 0) Then									'If in a mode...
		hellEnable(0)											'DISABLE more MB - Can only start MB once per mode (if mode allows)
	Else
		hellEnable(1)											'If not in a mode, eligible again
		demonQualify()		
	End If	
End Sub

'FUNCTIONS FOR PHOTO HUNT MODE 7............................
sub photoStart()								'When you shoot scoop with Photo Hunt lit
	Dim X
	videoModeCheck()
	lightSpeed = 2								'Fast light speed
	restartKill 0, 0
	AddScore(500000)
	comboKill()
	storeLamp(player)							'Store the state of the Player's lamps
	allLamp(0)									'Turn off the lamps
	spiritGuideEnable(0)						'No spirit guide during Photo Hunt
	'MINION LIGHTS???
	pulse(17)									'Pulse the Ghost Loop Lights
	pulse(18)
	pulse(19)
	modeTotal = 0								'Reset mode points	
	if (NOT(minionMB) and minion(player) < 10) Then	'Not in a Minion Mode?
		setGhostModeRGB 0, 0, 0				'Set Ghost to black. But this way we can make him flash
	End If
	TargetTimerSet 100, TargetDown, 350		'Put targets down, ghost loop adds time
	setCabModeFade 25, 0, 0, 300				'Kind of dim red "darkroom" lighting
	photosToGo = photosNeeded(player)			'See how many photos we need.
	playSFX 0, "F", "2", 62 + photosToGo, 255	'Mode start dialog, based off photos needed
	killQ()									'Disable any Enqueued videos
	video "F", "2", "A", allowSmall, 83, 255
	GLIR(player) = 255							'Set flag that we are IN photo hunt mode
	Mode(player) = 7							'Ghost photo hunt mode!
	Advance_Enable = 0							'Can't advancd until we win or lose (or drain)
	photoTimer = Int(70000/cycleAdjuster)		'Set high so timer doesn't start for an extra second
	countSeconds = photoSecondsStart(player)	'Time left to hit shot
';	numbers 0, numberStay OR 4, 0, 0, countSeconds - 1		'Update the Numbers Timer. We do "-1" so it'll display a zero.
	DoorSet DoorOpen, 5						'Open the Spooky Door, if it isn't already
	hellEnable(0)								'Can't lock balls
	showProgress 1, player					'Show the Main Progress lights	(do this first so the BLINK PROGRESS will work
	blink(50)									'Blink the PHOTO ACE progress light
	customScore "F", "2", "Z", allowAll OR loopVideo, 84		'Custom Score: Strobing Shots for Photos!
'	numbers 8, numberScore OR 2, 0, 0, player				'Player's score
	numbers playerScore(player), "", "", ""
	photoValue = (countSeconds * 10000) + (100000 * (photosNeeded(player) - 2))
'	numbers 9, 2, 70, 27, photoValue						'Photo Value
	numbers "", "", "", photoValue
'	numbers 10, 2, 122, 0, photosToGo						'How many photos are left
	numbers "", photoValue, "", ""
'	playMusic 'H', '2'							'Hurry-up music
	musicplayer "bgout_H2.mp3"
	ScoopTime = Int(55000/CycleAdjuster)		'Kick out the ball	
	for x=0 To 5
		photoLocation(x) = 0					'Clear Control Box locations	
		light photoLights(x), 0				'Turn off the 6 camera positions	
	Next
	photoCurrent = random(5)						'Select a camera, but not the one on the scoop (since we just came from there
	if (extraLit(player) and photoCurrent = 1) Then	'If Extra Ball lit and first photo is same shot, make first photo left orbit
		photoCurrent = 0
	End If
	photoWhich = 0										'Used for tourney path
	if (tournament) Then
		photoCurrent = photoPath(photoWhich)			'Pre-determined first shot if in Tournament Mode
	End If
	photoLocation(photoCurrent) = 255					'Which one has the camera
	strobe (photoLights(photoCurrent) - photoStrobe(photoCurrent)), photoStrobe(photoCurrent) + 1						'Strobe as many under it as we can
End Sub

sub photoLogic()										'What goes on during Photo Hunt Mode
	photoTimer = photoTimer  -  1
	Select Case photoTimer
		case Int(149999/CycleAdjuster):
			allLamp(7)
		case Int(149500/CycleAdjuster):
			allLamp(0)
		case Int(149000/CycleAdjuster):
			allLamp(7)
		case Int(148500/CycleAdjuster):
			allLamp(0)
		case Int(148200/CycleAdjuster):
			allLamp(7)
		case Int(147900/CycleAdjuster):
			allLamp(6)
		case Int(147600/CycleAdjuster):
			allLamp(5)
		case Int(147300/CycleAdjuster):
			allLamp(4)
		case Int(147000/CycleAdjuster):
			allLamp(3)
		case Int(146700/CycleAdjuster):
			allLamp(2)
		case Int(146400/CycleAdjuster):
			allLamp(0)
		case Int(146000/CycleAdjuster):
			if (photosToGo) Then							'Not done yet?
				loadLamp(tempLamp)						'Restore previous lights from temp memory
				strobe (photoLights(photoCurrent) - photoStrobe(photoCurrent)), photoStrobe(photoCurrent) + 1						'Strobe as many under it as we can					
				photoTimer = longSecond * 2			'A grace period of a few seconds before timer starts to decement again
			Else
				photoWin()
			End If
		case 1:
			if (Mode(player) = 7) Then					'Still in Photo hunt mode?
				strobe (photoLights(photoCurrent) - photoStrobe(photoCurrent)), photoStrobe(photoCurrent) + 1	'Make sure STROBE is on!
				photoTimer = longSecond				'Reset timer
				countSeconds  = countSeconds  -  1						'Reduce seconds left
				photoValue  = photoValue  -  10000					'Lose 10k points per second
'				numbers 9, 2, 70, 27, photoValue		'Update Photo Value
				numbers "", "", "", photoValue
				if (countSeconds = 0) Then				'Out of time?
					photoFail(0)						'Fail blog!		
				Else
';					numbers 0, numberStay OR 4, 0, 0, countSeconds - 1	'Update the Numbers Timer
				if (countSeconds > 1 and countSeconds < 7) Then	'Count down 5 4 3 2 1
						playSFX 2, "A", "M", 47 + countSeconds, 1
					Else
						playSFX 2, "Y", "Z", "Z", 1				'Beeps
					End If				
				End If
			End If
	End Select
End Sub

sub photoCheck(whichSpot)									'Checking if your shot has the Ghost Photo
	if (photoLocation(whichSpot) = 255) Then				'A ghost photo found?
		photoLocation(whichSpot) = 0						'Clear that location! (8-22-14 fix)
		flashCab 255, 255, 200, 20							'Flash of white, then back to red mode color
		photoTimer = Int(150000/CycleAdjuster)				'Reset this, a little higher to trigger LIGHT SHOW
		AddScore((countSeconds * 10000) + (100000 * (photosNeeded(player) - 2)))		'10 grand per second remaining + ()100k * # Times You've Started Photo Hunt)
		countSeconds = photoSecondsStart(player)		'Time left to hit shot
		photoValue = (countSeconds * 10000) + (100000 * (photosNeeded(player) - 2))	'Re-calculate next photo value	
'		numbers 9, 2, 70, 27, photoValue												'Update display Photo Value
		numbers "", "", "", photoValue
';		numbers 0, numberStay OR 4, 0, 0, countSeconds - 1			'Update the Numbers Timer so we see the new number	right away 		
		photosTaken(player)  = photosTaken(player)  +  1						'Total number for bonus
		photosToGo  = photosToGo  -  1								'Reduce this
'		numbers 10, 2, 122, 0, photosToGo				'Update how many photos are left
		numbers "", photosToGo, "", ""
		ghostFlash(50)									'Flash the ghost. In photo mode only, fades to black. If minion, goes back to medium white
		light (photoLights(whichSpot) - photoStrobe(whichSpot)), 0	'Turn off the Strobe
		if (photosToGo > 0) Then									'Didn't win yet?
			playSFX 0, "F", "3", 65 + random(8), 255			'Good, catch another!
			video "F", "3", 65 + random(9), allowSmall, 47, 255		'Show ghost photo
			videoQ "F", "9", 64 + photosToGo, allowSmall, 30, 255		'Follow-up video saying how many we have to go
			photoCurrent = whichSpot								'Set X to be the current shot, so we don't pick 2 in a row
			Do while (photoCurrent = whichSpot)						'If random camera is same as last, loop continues
				photoCurrent = random(5)							'Pick from any of the first 5 shots (not the scoop)	
				if (extraLit(player) and photoCurrent = 1) Then		'If Extra Ball is lit and we choose the door VUK, choose something else
					photoCurrent = whichSpot
				End If
			Loop
			photoWhich  = photoWhich  +  1									'Used for tourney path
			if (tournament) Then
				photoCurrent = photoPath(photoWhich)			'Pre-determined next shot if in Tournament Mode
			End If
			photoLocation(photoCurrent) = 255						'Which new location has the camera
			strobe (photoLights(photoCurrent) - photoStrobe(photoCurrent)), photoStrobe(photoCurrent) + 1	'Strobe the shot!	
			if (photoCurrent = 3) Then									'Make SURE this one sticks!
				strobe 26, 6			
			End If
			if (photosToGo = 1) Then
				lightSpeed = 3									'If last shot, make the lights pulse even faster!
			End If
		Else															'Ending stuff. Mode TRULY ends after the flash finishes
			comboKill()
			'AWARD BONUS OF TOTAL PHOTOS * SOMETHING
			DOF 111, 2
			killTimer(0)												'Turn off numbers	
			AddScore(1000000 * photosNeeded(player))					'1 million for each photo you got! Nice win bonus!
			killQ()
			playSFX 0, "F", "4", 65 + random(4), 255					'Win dialog!
			video "F", "3", 65 + random(9), noExitFlush, 47, 255		'Show final photo
'			numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233			'Load Mode Total Points
			numbers "", "", modeTotal, modeTotal
			videoQ "F", "9", "Y", noEntryFlush OR 3, 57, 233	'Photo Hunt Mode Total:
		End If
		storeLamp(tempLamp)								'Store the lights in temp slot 5 since we're about to do an animation
	Else													'Not a photo shot?
		playSFX 0, "F", "3", 73 + random(8), 255			'Taunt player
		video "F", "2", "C", allowSmall, 74, 200			'Empty frame + prompt	
		AddScore(5000)										'A few points
	End If
End Sub

sub photoWin()										'What happens when you collect X photos in time
	DOF 134, 2
	modeTotal = 0									'Reset mode total
	loadLamp(player)								'Load the original lamp state back in
	light 50, 7												'Light PHOTO ACE progress solid!
	if (photosNeeded(player) < 9) Then								'Under the max?
		photosNeeded(player)  = photosNeeded(player)  +  1								'Increase # of photos required
		photoSecondsStart(player)  = photoSecondsStart(player)  -  1							'Decrease 1 second off the timer per photo
	End If
	photoEnd(1)												'Win condition, 1 = request new music
	if (photosNeeded(player) = 6) Then							'Light EXTRA BALL on 3rd successful Photo Hunt
		extraBallLight(2)										'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random 2), 255	'"Extra Ball is Lit!"			
	End If
	if (photosNeeded(player) = 4) Then							'First photo hunt success? Check if Demon Mode is ready
		demonQualify()
	End If
End Sub

sub photoFail( reasonFail)									'What happens when you DON'T
	loadLamp(player)
	comboKill()
	if (photosNeeded(player) = 3) Then				'Haven't won a Photo Hunt yet?
		light 50, 0								'Make sure progress light is OFF
	End If
	light 43, 0
	killTimer(0)						'Turn off numbers	
	if (reasonFail = 0) Then						'Fail via drain we pass a 1, and thus, don't do the video or speech
		killQ()								'Disable any Enqueued videos	
		playSFX 0, "F", "5", 65 + random(8), 10	'Fail dialog
		video "F", "9", "Z", noExitFlush, 57, 255	'Show final photo
'		numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233			'Load Mode Total Points
		numbers "", "", modeTotal, modeTotal
		videoQ "F", "9", "X", noEntryFlush OR 3, 57, 233		'Photo Hunt Mode Total:
		photoEnd(1)							'Send a 1 meaning START new music
	Else
		photoEnd(0)							'We ARE in a drain. Send a 0 meaning DON'T start new music
	End If
End Sub

sub photoEnd( musicChange)								'What happens after WIN or LOSE, regardless, to close out the mode. Other modes need this!
	Dim X
	lightSpeed = 1									'Normal light speed
	GLIR(player) = GLIRneeded(player)				'How many times you'll have to spell GLIR to restart
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	for x=0 To 5
		photoLocation(x) = 0					'Clear Control Box locations	
		light photoLights(x), 0				'Turn off the 6 camera positions	
	Next
	rollOvers(player) = 0						'Clear bits
	light 43, 0								'Turn off GLIR MODE LIGHT
	if (NOT(minionMB) and minion(player) < 10) Then	'Not in a Minion Mode?
		light 16, 0							'Turn off the Make Contact light that may have been left on the cached Light Data	
	End If
	if (musicChange) Then
		if (minionMB = 0 and minion(player) < 10) Then		'Not in Minion Multiball, or fighting a Minion?
'			playMusic 'M', '2'							'Normal music
			musicplayer "bgout_M2.mp3"
		End If
		if (minionMB > 9) Then									'Minion MB?
'			playMusic 'M', 'I'							'Placeholder for Minion MB theme
			musicplayer "bgout_MI.mp3"
		End If
		if (minionMB < 10 and minion(player) > 9) Then			'Just fighting a minion?
'			playMusic 'M', 'I'
			musicplayer "bgout_MI.mp3"
		End If
	End If
	photoTimer = 0							'Kill this timer
	Mode(player) = 0						'Set mode to ZERO
	Advance_Enable = 1						'Can advance	
	modeTotal = 0								'Reset mode points	
	checkModePost()	
	hellEnable(1)
	spiritGuideEnable(1)						'Re-enable spirit guide
	showProgress 0, player					'Show the Main Progress lights
	photosToGo = 0								'This is often used to check if mode active, so set it to ZERO
End Sub
'FUNCTIONS FOR PHOTO HUNT MODE 7............................

sub popCheck()
	if (skillShot) Then			
		if (skillShot = 1) Then						'Did we hit the Skill shot?
			skillShotSuccess 1, 0					'Success!
			DOF 117, 2
		Else
			skillShotSuccess 0, 0					'Failure, so just disable it
		End If
		'return										'The pop hit for skill shot can also advance other stuff 8-22-14 fix
	End If
	'Wasn't a skill shot, so continue as per normal
	if (Advance_Enable = 0) Then					'In a mode of some sort?
		evpPops()									'Do EVP pops
	Else
		if (popMode(player) = 1) Then				'Advancing Fort?		
			if (fortProgress(player) < 50) Then			
				WarAdvance()
			End If											
		End If	
		if (popMode(player) = 2) Then				'Advancing Bar?
			if (barProgress(player) < 50) Then			
				BarAdvance()
			End If		
		End If				
		if (popMode(player) = 3) Then				'Done advancing Bar and Fort?
			evpPops()								'Do EVP pops		
		End If
	End If
End Sub

sub popLogic(popType)
	if (popType = 0) Then													'Need to Figure out what Pop Mode we should be in?
		if (barProgress(player) = 50 or fortProgress(player) = 50) Then		'Eligible to start Bar mode or War Fort mode?
			popType = 3														'Then pops should do EVPs
			light 20, 0														'Do the lights, and return so nothing else can trigger
			light 21, 0
			light 22, 0
			pulse(20)
			Exit Sub
		End If
		if (fortProgress(player) > 99) Then	'Has War Fort already been won?
				popType = 2				'Then Advance Bar	
		End If
		if (barProgress(player) > 79) Then	'Has Bar Ghost already been won?
				popType = 1				'Then Advance Fort	
		End If
		if (fortProgress(player) > 99 and barProgress(player) > 79) Then	'Both have been won?
			popType = 3					'EVP's from now on
		End If
		if (fortProgress(player) < 100 and barProgress(player) < 80) Then	'Neither have been won?						
			if (barProgress(player) > fortProgress(player)) Then
				popType = 2				'If Bar is progressed further, light it
			Else
				popType = 1				'Else they're both 0, or Fort is further
			End If		
		End If
		if (Advance_Enable = 0) Then			'If in a mode of some kind, always Jackpot Advance / EVP
			popType = 3
		End If
	End If
	if (popType = 1) Then						'Advancing Fort?
		light 20, 0
		light 21, 0
		light 22, 0
		pulse(21)
	End If
	if (popType = 2) Then						'Advancing Bar?
		light 20, 0
		light 21, 0
		light 22, 0
		pulse(22)
	End If
	if (popType = 3) Then						'EVP pops?			
		light 20, 0
		light 21, 0
		light 22, 0
		pulse(20)
	End If
	popMode(player) = popType	
End Sub

sub popToggle()
	if (leftVolume > rightVolume) Then
		leftVolume = 10
		rightVolume = 100
	Else
		leftVolume = 100
		rightVolume = 10
	End If
End Sub

'FUNCTIONS FOR PRISON MODE 6............................
sub PrisonAdvance()							'The first 3 advances
	AddScore(advanceScore)
	flashCab 0, 255, 0, 100					'Flash the GHOST BOSS color
	priProgress(player) = priProgress(player)  +  1
	areaProgress(player) = areaProgress(player)  +  1
	if (priProgress(player) < 3) Then										'First 2 advances?
		light (2 + priProgress(player)), 7								'Current number solid.
		pulse(3 + priProgress(player))									'Pulse the next
		playSFX 0, "P", priProgress(player)+ 48, random(4) + 65, 255	'Play 1 of 4 audio clips
		video "P", priProgress(player) + 48, "A", allowSmall, 60, 254	'Run video
	End If
	if (priProgress(player) = 3) Then										'Third advance?
		light 3, 7													'Make sure the first 3 are solid...
		light 4, 7
		light 5, 7
		blink(6)														'Blink "Prison Lock"
		playSFX 0, "P", "3", random(3) + 65, 255						'Play 1 of 3 audio clips	
		video "P", "3", "A", allowSmall, 90, 254						'Run video	
	End If	
	if (priProgress(player) = 4) Then										'Fourth orbit shot to start mode?
		priProgress(player) = 6										'Set flag so that when it hits basement Prison Mode will start (left orbit won't do anything now)
	End If
	'priProgress(player) = 4 is the 4th shot up the orbit. This then goes to ORBS or Basement to Lock the first ball
	'We do this to prevent the 3rd orbit shot from also locking the first ball
End Sub

sub PrisonAdvance2()							'Locking the balls
	killQ()									'Combos fuck this up sometimes, so just in case...
	AddScore(advanceScore)
	'flashCab 0, 255, 0, 200					'Flash the GHOST BOSS color
	priProgress(player)  = priProgress(player)  +  1											'Advance progress. First time here this will be 6. Will get incremented to 7 to start mode 8-22-14 fix
	if (priProgress(player) < 7) Then										'First 2 balls?		8-22-14 update, this will never occur now	
		light (priProgress(player) - 2), 7								'Make light solid.
		video "P", 44 + priProgress(player), "B", allowSmall, 124, 255	'Video of Ball Locked
		playSFX 0, "P", "4", 60 + priProgress(player), 255			'Ah, I'm trapped! Next person get down there!	
	End If
	if (priProgress(player) = 7) Then										'Locked 3rd Ball? (actually just the 4th shot)
		PrisonStart()	
	End If
End Sub

sub PrisonStart()							'Prison Ghost Battle
	Dim x
	videoModeCheck()
	modeTotal = 0								'Reset mode points	
	AddScore(startScore)
	comboKill()
	storeLamp(player)							'Store the state of the Player's lamps	
	allLamp(0)									'Turn off the lamps
	spiritGuideEnable(1)
	popLogic(3)								'Set pops to EVP
	minionEnd(0)								'Disable Minion mode, even if it's in progress
	setGhostModeRGB 255, 131, 0				'Orange ghost!
	setCabModeFade 0, 255, 0, 200				'Turn lighting GREEN (with envy
	Advance_Enable = 0							'Mode started, disable advancement until we are done
	Mode(player) = 6							'Set theater mode ACTIVE for player
	teamSaved = 0								'Reset how many members saved
	jackpotMultiplier = 1						'Reset this just in case
	blink(62)									'Blink the PRISON mode light.
	tourReset(46)						'Tour: Left orbit, center shot, hotel path, right orbit
'	playMusic 'B', '1'						'Boss battle music!
	musicplayer "bgout_B1.mp3"
	x = random(3)							'Video clip must match audio
	killQ()									'Disable any Enqueued videos
	video "P", "4", 68 + x, allowSmall, 219, 255	
	playSFX 0, "P", "4", 68 + x, 255			'Mode start dialog
	'videoQ 'P', '5', 'G', loopVideo OR allowSmall, 0, 200			'The ghost behind all 3 targets!
	hellEnable(0)								'Disable the Hellavator Call & Lock
	customScore "P", "5", "G", allowAll OR loopVideo, 177		'Shoot score with targets in front
';	numbers 8, numberScore OR 2, 0, 0, player	'Show player's score in upper left corner
';	numbers 9, 9, 88, 0, 0					'Ball # upper right
';	numbers 10, 2, 2, 27, 3					'Show balls left to add
';	numbers 11, 2, 116, 27, 1					'Jackpot multiplier	
	convictState = 1							'State of Prison Ghost (Need to open door)
	convictsSaved = 0							'Reset How many you've saved
	'DoorSet DoorClosed, 5						'Close the door for this state
	pulse(14)									'Pulse the door shot
	if (countGhosts() = 5) Then						'Is this the last Boss Ghost to beat?
		blink(48)									'Blink that progress light
	End If	
	TargetTimerSet 10000, TargetUp, 100		'Put the targets back up
	pulse(17)									'Ghost targets strobe for MINION BATTLE!
	pulse(18)
	pulse(19)	
	targetReset()	
	priProgress(player) = 9					'Set flag to delay scoop when the ball gets there
	hellEnable(0)								'Can't do multiball since this is a 4 ball mode anyway	
	showProgress 1, player					'Show the Main Progress lights
	doorLogic()
End Sub

sub PrisonLogic()
	if (priProgress(player) > 9 and priProgress(player) < 20) Then							'Trying to free your friends?
		modeTimer  = modeTimer  +  1
		if (modeTimer > Int(100000/CycleAdjuster)) Then														'Prisoner prompt?					
			if (convictState = 1) Then								'Haven't opened the door yet?
				playSFX 1, "P", "X", 65 + random(4), 255			'Prompt to do that
				video "P", "8", "X", 3, 38, 254			
			Else
				'NEW VIDEO HERE:
				video "P", "8", "V", allowSmall, 45, 255			'Door is open Prompt to SHOOT VUK
				playSFX 1, "P", "V", 65 + random(4), 255	
			End If	
			modeTimer = 0
		End If
	End If
	if (priProgress(player) = 20) Then													'Prison multiball?
		modeTimer  = modeTimer  +  1
		if (modeTimer = Int(65000/cycleAdjuster)) Then														'Prisoner prompt?					
			if (convictState = 1) Then								'Haven't opened the door yet?
				playSFX 1, "P", "X", 65 + random(4), 255			'Prompt to do that
				video "P", "8", "X", 3, 38, 254			
			Else
				video "P", "8", "V", allowSmall, 45, 255			'Door is open Prompt to SHOOT VUK
				playSFX 1, "P", "V", 65 + random(4), 255	
			End If	
		End If
		if (modeTimer = Int(130000/CycleAdjuster)) Then							'Team member prompt?
			playSFX 0, "P", 65 + random(4), random(10), 255		
			modeTimer = 0
		End If
	End If
End Sub

sub PrisonDrainCheck( whenDrain)
	if (whenDrain) Then											'1 = Drain after all balls free
		if (activeBalls = 3) Then									'Did we lose first member?
			playSFX 0, "P", "9", 65 + random(3), 255			'Heather calls it quits
			video "P", "9", "X", allowSmall, 30, 255			'She leaves
			videoQ "P", "7", "F", allowSmall, 39, 255			'Prompt how many balls are left
		End If
		if (activeBalls = 2) Then									'Did we lose second member?
			playSFX 0, "P", "9", 68 + random(3), 255			'Misty calls it quits
			video "P", "9", "Y", allowSmall, 30, 255			'She leaves
			videoQ "P", "7", "E", allowSmall, 39, 255			'Prompt how many balls are left
		End If		
		if (activeBalls = 1) Then									'Did we lose third member?
			PrisonWin()
		Else
			jackpotMultiplier = activeBalls					'Update jackpot multipler
			sendJackpot(0)										'Send jackpot value to score #0
		End If
	Else												'0 = Drain before all balls free
		if (priProgress(player) = 11) Then				'Did we lose first member?
			playSFX 0, "P", "9", 65 + random(3), 255	'Heather calls it quits
			video "P", "9", "X", allowSmall, 30, 255			'She leaves
		End If
		if (priProgress(player) = 12) Then				'Did we lose second member?
			playSFX 0, "P", "9", 68 + random(3), 255	'Misty calls it quits
			video "P", "9", "Y", allowSmall, 39, 255			'She leaves
		End If
';		numbers 11, 2, 116, 27, activeBalls	'Update Jackpot multiplier - 1		
	End If
End Sub

sub PrisonRelease()
	priProgress(player)  = priProgress(player)  +  1	
	modeTimer = 0
	if (priProgress(player) = 11) Then				'First player freed?	
		AddScore(startScore * 1)
		if (ScoopTime and spiritGuide(player)) Then	'If both are YES, the ball is in the scoop doing a Spirit Guide, so ENQUEUE release video
			videoSFX "P", "7", "A", 2, 71, 200, 0, "P", "5", 88 + random(3), 200
		Else										'Normal
			playSFX 0, "P", "5", 88 + random(3), 255	'Heather is free!			
			video "P", "7", "A", allowSmall, 71, 255
			videoQ "P", "7", 67 + activeBalls + 1, allowSmall, 39, 200
		End If
		teamSaved  = teamSaved  +  1
		AutoPlunge(autoPlungeFast)							'Autolaunch ball!
		pulse(17)									'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)	
		targetReset()
		customScore "P", "5", 64 + (targetBits AND 7), allowAll OR loopVideo, 177		'Shoot score with targets in front
';		numbers 10, 2, 2, 27, 2					'Show balls left to add
';		numbers 11, 2, 116, 27, activeBalls + 1)	'Jackpot multiplier (add one since the ball won't be loaded yet		
	End If
	if (priProgress(player) = 12) Then				'Second player freed?
		AddScore(startScore * 2)
		if (ScoopTime and spiritGuide(player)) Then	'If both are YES, the ball is in the scoop doing a Spirit Guide, so ENQUEUE release video
			videoSFX "P", "7", "B", 2, 71, 200, 0, "P", "6", 88 + random(3), 200
		Else										'Normal
			playSFX 0, "P", "6", 88 + random(3), 255	'Misty is free!
			video "P", "7", "B", allowSmall, 71, 255
			videoQ "P", "7", 67 + activeBalls + 1, allowSmall, 39, 255							
		End If
		teamSaved  = teamSaved  +  1
		AutoPlunge(autoPlungeFast)							'Autolaunch ball!
		pulse(17)									'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)	
		targetReset()
		customScore "P", "5", 64 + (targetBits AND 7), allowAll OR loopVideo, 177		'Shoot score with targets in front
';		numbers 10, 2, 2, 27, 1					'Show balls left to add
';		numbers 11, 2, 116, 27, activeBalls + 1)		'Jackpot multiplier (add one since the ball won't be loaded yet	
	End If	
	if (priProgress(player) = 13) Then				'Third player freed? Start JACKPOT MODE!
		AddScore(startScore * 3)
		if (ScoopTime and spiritGuide(player)) Then	'If both are YES, the ball is in the scoop doing a Spirit Guide, so ENQUEUE release video
			videoSFX "P", "7", "C", 2, 100, 200, 0, "P", "7", 88 + random(3), 200
		Else										'Normal
			playSFX 0, "P", "7", 88 + random(3), 255	'Kaminski is free!
			video "P", "7", "C", allowSmall, 100, 255
			videoQ "P", "7", 67 + activeBalls + 1, allowSmall, 39, 255
		End If	
		teamSaved  = teamSaved  +  1
		priProgress(player) = 20					'Flag that we're now bashing the hell out of the ghost.
		AutoPlunge(autoPlungeFast)					'Autolaunch ball!
		TargetTimerSet 100, TargetDown, 1		'Put the targets down quickly so we can get JACPOTS		
		light 62, 7								'Light mode SOLID = Win
'		playMusic 'G', 'S'						'Amazing Ghost Squad theme!
		musicplayer "bgout_GS.mp3"
		'All balls released, so now we ease up a bit and allow a Ball Save time
		multipleBalls = 1							'When MB starts, you get ballSave amount of time to loose balls and get them back
		ballSave()									'That is, Ball Save only times out, it isn't disabled via the first ball lost		
		killScoreNumbers()							'Disable any custom score numbers so we can rebuild them
		jackpotMultiplier = activeBalls + 1			'Update jackpot value
		sendJackpot(0)								'Send jackpot value to score #0
		customScore "B", "1", "D", allowAll OR loopVideo, 36				'Custom Score: Hit ghost for JACKPOTS!
';		numbers 8, numberScore OR 2, 0, 0, player						'Put player score upper left
';		numbers 9, numberScore OR 2, 72, 27, 0							'Use Score #0 to display the Jackpot Value bottom off to right
';		numbers 10, 9, 88, 0, 0										'Ball # upper right
'		ModeWon(player) OR= 1 << 6				'Set PRISON WON bit for this player.	
		ModeWon(player) = ModeWon(player) OR 64
		if (countGhosts() = 6) Then										'This the final Ghost Boss? Light BOSSES solid!
			light 48, 7
		End If
	End If	
End Sub

sub PrisonJackpot()
	MagnetSet(50)											'Catch ball briefly	
	video "P", "9", 65 + random(2), allowSmall, 45, 255		'One of two Jackpot Bash videos (left or right
	playSFX 0, "P", "8", 65 + random(8), 255				'Jackpot Sound!
	ghostFlash(50)
	ghostAction = Int(20000/CycleAdjuster)					'Whack routine	
	showValue (EVP_Jackpot(player) * activeBalls), 40, 1
End Sub

sub PrisonWin()
	DOF 134, 2
	multipleBalls = 0
	tourClear()								'Clear the tour lights / values
	AddScore(winScore)
	loadLamp(player)
	comboKill()
	convictState = 0
	light 3, 0							'Turn off mode counter lights
	light 4, 0
	light 5, 0
	light 6, 0
	light 16, 0							'Turn off Ghost Lights
	light 17, 0
	light 18, 0
	light 62, 7							'Old Prison solid = Mode Won!
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()		
	ghostMove 90, 20
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(200/CycleAdjuster)
	ghostFadeAmount = Int(200/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	light 62, 7							'Light mode SOLID = Win
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	Advance_Enable = 1						'Allow other modes to be started
	TargetTimerSet 5000, TargetUp, 100	'Put targets back up, but not so fast ball is caught
	killQ()													'Disable any Enqueued videos
	playSFX 0, "P", "9", 88 + random(3), 255					'Ghost dies, We fuckin' did it!
	video "P", "9", "Z", noExitFlush, 101, 255 					'Play Death Video	
';	numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233			'Load Mode Total Points
	modeTotal = 0							'Reset mode points		
	videoQ "P", "9", "V", noEntryFlush OR 3, 45, 233	'Prison Mode Total:	
'	playMusic 'M', '2'							'Normal music
	musicplayer "bgout_M2.mp3"
	Mode(player) = 0						'Set mode active to None
	priProgress(player) = 100				'Reset this for no real reason.
	if (countGhosts() = 2 or countGhosts() = 5) Then	'Defeating 2 or 5 ghosts lights EXTRA BALL
		extraBallLight(2)							'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random 2), 255	'"Extra Ball is Lit!"			
	End If	
	demonQualify()									'See if Demon Mode is ready
	checkModePost()
	hellEnable(1)
	showProgress 0, player					'Show the Main Progress lights
End Sub

sub PrisonFail()
	multipleBalls = 0
	tourClear()								'Clear the tour lights / values
	loadLamp(player)						'Bring in the old lights
	comboKill()
	convictState = 0
	light 2, 0							'Turn off mode counter lights
	light 3, 0
	light 4, 0
	light 5, 0
	light 16, 0							'Turn off Ghost Lights
	light 17, 0
	light 18, 0
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(200/CycleAdjuster)
	ghostFadeAmount = Int(200/cycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	if ((ModeWon(player) AND 64)=64) Then								'Did we win this mode before?
		light 62, 7												'Make Prison Mode light solid, since it HAS been won
	Else
		light 62, 0												'Haven't won it yet, turn it off
	End If		
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	ghostMove 90, 20
	Advance_Enable = 1						'Allow other modes to be started
	modeTotal = 0							'Reset mode points	
	'playMusic 'M', '2'					'Normal music
	Mode(player) = 0						'Set mode active to None
	if ((modeRestart(player) AND 64)=64) Then							'Able to restart Prison?
		modeRestart(player) = modeRestart(player) AND 191		'Clear the restart bit	
		priProgress(player) = 3									'When you come back, Prison lock will already be lit
	Else
		priProgress(player) = 0									'Blew it a second time? Gotta start all over!
	End If
	checkModePost()
	hellEnable(1)
	showProgress 0, player					'Show the Main Progress lights
End Sub

sub restartPlayer(whichPlayer)
	Dim X
	animatePF 0, 0, 0	
	x = whichPlayer
	popMode(x) = random(2) + 1					'Starts either 1 = Fort or 2 = Bar (3 = EVP but never starts in that mode)
	if (popMode(x) = 1) Then					'Start out Advancing Fort?
		light 20, 0
		light 21, 0
		light 22, 0
		pulse(21)
	End If
	if (popMode(x) = 2) Then					'Start Out Advancing Bar?
		light 20, 0
		light 21, 0
		light 22, 0
		pulse(22)	
	End If
	Mode(x) = 0
	SetScore(x)									'Set player's score on DMD
	ModeWon(x) = 0								'Clear the bits of what modes they've won
	modeRestart(x) = 126						'At the start each player gets 1 re-start chance per mode
	tourComplete(x) = 0							'Which tours player has completed	
	hosProgress(x) = 0
	theProgress(x) = 0
	barProgress(x) = spotProgress				'Can be changed in settings
	fortProgress(x) = spotProgress				'Can be changed in settings
	hotProgress(x) = 0
	priProgress(x) = 0
	deProgress(x) = 100							'You completed Demon Mode!
	lockCount(x) = 0							'Reset # of locked Hellavator balls
	spiritProgress(x) = 0						'If tourney mode, players get awards in sequence	
	spiritGuide(x) = 1							'Spirit guide is always lit to start for each player	
	EVP_Jackpot(x) = 1000000					'Reset to 1 million
	'Don't Reset EVP's!
	photosTaken(x) = 0							'Total photos a player got.
	areaProgress(x) = 0							'How many mode-advancing shots each player has made
	ghostsDefeated(x) = 0
	photosNeeded(x) = 3
	photoSecondsStart(x) = 21					'How many seconds to get a ghost photo
	GLIR(x) = 1									'At the start of the game, spell GLIR once to light Photo hunt, then twice, 3 times, ect.
	GLIRneeded(x) = 1							'How many GLIR spells player needs to light PHOTO HUNT
	GLIRlit(x) = 0								'Zero PHOTO HUNTS lit to start
	minion(x) = 1								'Each player starts with Minion Fight enabled
	minionTarget(x) = 3							'3 hits for first Minion Battle
	minionHits = 3								'Reset this
	minionsBeat(x) = 0							'How many minions you've beaten.
	minionHitProgress(x) = 0					'No minion damage progress yet
	TargetSet(TargetUp)							'Put targets UP so we can enagage Minion Battle!
	'Don't reset ORB or Extra Balls!
	wiki(x) = 0
	tech(x) = 0
	psychic(x) = 0	
	rollOvers(x) = 0
	hellLock(x) = 1								'Always start with Hell lock enabled		
	hellJackpot(x) = 1000000					'Starting MB jackpot value
	hitsToLight(x) = 1							'How many times you have to press "Call" before hellavator moves / lights for lock	(starts with just 1)						
	callHits = 0								'How many times you've hit Call this ball (resets per player)	
	minionEnd(1)								'The default is to enable the Minion Battle	
	doorLogic()									'Figure out what to do with the door
	elevatorLogic()								'Can lock balls, Hellavator is Lit		
	scoreMultiplier = 1							'This will almost always be 1
	trapDoor = 0								'Flags if the ball should be trapped or not
	trapTargets = 0
	slingCount = 0	
	spiritGuideEnable(1)						'Mode 0, it can always be lit		
	bonusMultiplier = 1							'Reset multiplier (it's per ball so don't need unique variable per player)		
	modeTimer = 0	
	HellBall = 0
	tiltCounter = 0								'Reset to zero
	sweetJumpBonus = 0							'Reset score (hitting it adds value)
	sweetJump = 0								'Reset video/SFX counter
	Advance_Enable = 1							'Game starts with all modes eligible for advancement.
	AutoEnable = 255 							' 255 /enables everything
	'The ball under the ghost is Active Ball #1. Release it and there is no need to change the value
	setCabMode defaultR, defaultG, defaultB		'Set the cab mode to default color
	comboKill()
	dirtyPoolMode(1)							'Check for Dirty Pool Balls
	Update(0)									'Update with the current info
	comboEnable = 1								'OK combo all you want
	GLIRenable(1)								
'	playMusic 'M', '2'							'Normal music
	musicplayer "bgout_M2.mp3"
	showProgress 0, player						'Reset progress lights
	storeLamp(x)								'Store the status of every lamp in that player's memory
End Sub

sub restartBegin(whichMode, startingSeconds, startingTimer)
	restartMode = whichMode						'If we fail, which mode progress do we reset?
	restartSeconds = startingSeconds			'How many seconds you get
	restartTimer = Int(startingTimer/CycleAdjuster)				'Start this above 25k to give player time to get the ball back
';	numbers 0, numberStay OR 4, 0, 0, restartSeconds - 1	'Update the Numbers Timer.	
End Sub

sub restartKill(whichKill, whatReason)			'Fail to get a restart? Reset whatever mode we were trying to restart.
	killTimer(0)											'Turn off timer
	restartTimer = 0										'Disable the timer
	restartSeconds = 0										'Clear the seconds
	if (whichKill = restartMode and whatReason = 1) Then		'Did we restart the same mode we just failed?
		restartTimer = 0									'Disable the timer
		restartSeconds = 0									'Clear the seconds	
		restartMode = 0									'Clear the mode and return
		Exit Sub
	End If
	if (whichKill = 0 and restartMode) Then					'Did we get here via a drain, and there was a restart mode active?
		Select Case (restartMode)							'Kill whatever restart mode was active (since the call to here wasn't explicit)
			case 1: 										'Hospital Restart Fail
				hosProgress(player) = 0
				ghostMove 90, 20
				checkModePost()
				showProgress 0, player
			case 2: 										'Theater Fail
				theProgress(player) = 0
				ghostMove 90, 20
				checkModePost()
				showProgress 0, player
			case 3: 										'Bar fail
				barProgress(player) = 0
				ghostMove 90, 20
				dirtyPoolMode(1)							'Check for Dirty Pool!
				loopCatch = 0								'Disable loop catch
				checkModePost()
				showProgress 0, player
		End Select
		showProgress 0, player
		restartMode = 0		
		Exit Sub
	End If
	'OK, player must have timed out the restart and game is still active
	video "B", "0", "X", allowSmall, 26, 200		'Restart time out video
	if (Advance_Enable) Then
'		playMusic 'M', '2'							'Go back to normal music if nothing is going on
		musicplayer "bgout_M2.mp3"
	End If
	Select Case (whichKill)
		case 1: 										'Hospital Restart Fail
			hosProgress(player) = 0
			ghostMove 90, 20
			checkModePost()
			showProgress 0, player
		case 2: 										'Theater Fail
			theProgress(player) = 0
			ghostMove 90, 20
			checkModePost()
			showProgress 0, player
		case 3: 										'Bar fail
			barProgress(player) = 0
			ghostMove 90, 20
			dirtyPoolMode(1)							'Check for Dirty Pool!
			loopCatch = 0								'Disable loop catch
			checkModePost()
			showProgress 0, player
	End Select
	showProgress 0, player
	restartMode = 0
End Sub

sub rollLeft()																'Lane change LEFT
	rollOvers(player) = (ShiftLeft(rollOvers(player), 1) AND 255) OR (ShiftRight(rollOvers(player), 7))
'	orb(player) = (orb(player) << 1) | (orb(player) >> 5)					'Top 2 MSB's of ORB are unused. Rotate lower 6.
	orb(player) = (ShiftLeft(orb(player), 1) AND 63) OR ShiftRight(orb(player), 5)
	laneChange()
	'checkRoll()
	'checkOrb(0)
End Sub

sub rollRight()																'Lane change RIGHT
	rollOvers(player) = (ShiftRight(rollOvers(player), 1)) OR (ShiftLeft(rollOvers(player), 7) AND 255) 'Rotate bit right
'	orb(player) = (orb(player) >> 1) | (orb(player) << 5)					'Top 2 MSB's of ORB are unused. Rotate lower 6
	orb(player) = ShiftRight(orb(player), 1) OR (ShiftLeft(orb(player), 5) AND 63)
	laneChange()
	'checkRoll()
	'checkOrb(0)
End Sub

sub scoopDo()														'What to do when the ball is shot into the scoop (a lot can happen!)
	if (barProgress(player) = 80) Then								'In ghost whore multiball?
		if (kegsStolen < 10) Then
			video "B", "0", "W", allowSmall, 45, 255				'Kaminski with beer
			playSFX 0, "B", "K", 65 + random(8), 255				'Kaminski comments about free beer
			kegsStolen = kegsStolen + 1
			showValue kegsStolen * 100000, 40, 1
		Else
			video "B", "0", "U", allowSmall, 38, 255				'"No Kegs Left!"
			playSFX 0, "B", "K", 73 + random(4), 255
		End If
	End If
	if (Mode(player) = 1) Then										'If Touring the Hospital, complete % of tour and kick out.
		tourGuide 0, 1, 5, 505010, 1								'Give more points than normal (scoop is harder shot)
		Exit Sub
	End If
	if (hotProgress(player) > 29 and hotProgress(player) < 40) Then	'Fighting the Hotel Ghost? (can't do tour during the Control Box search)
		tourGuide 0, 5, 5, 505010, 1								'Give more points than normal (scoop is harder shot)
		Exit Sub
	End If
	if (Advance_Enable and fortProgress(player) = 50) Then			'Eligible to start War Fort?			
		WarStart()			
		Exit Sub
	End If
	if (Advance_Enable and barProgress(player) = 50) Then			'Eligible to start Bar?			
		BarStart()			
		Exit Sub													'Return so other modes can't start
	End If
	if (hotProgress(player) = 20)	Then							'Searching for the Control Box?
		BoxCheck(4)													'Check / flag box for this location
		ScoopTime = 20000											'Kick out the ball
		Exit Sub													'Return so other modes can't start
	End If
	if (GLIRlit(player) = 1 and Advance_Enable = 1) Then			'Flag for Photo Hunt Start? Must equal 1, so if MSB set, will prevent a start
		GLIRlit(player) = 0											'Decrement how many we have
		photoStart()												'Start that mode!
		Exit Sub													'Return so other modes can't start
	End If
	if (deProgress(player) > 9 and deProgress(player) < 100) Then	'Trying to weaken demon
		DemonCheck(5)
		Exit Sub
	End If	
	if (Mode(player) = 7) Then										'Are we in Ghost Photo Hunt?
		photoCheck(5)
		Exit Sub													'Return so other modes can't start
	End If
	if (theProgress(player) > 9 and theProgress(player) < 100) Then	'Theater Ghost?
		if (theProgress(player) > 9 and theProgress(player) < 100) Then		'Theater Ghost?
			if (theProgress(player) = 12) Then						'Waiting for Shot 3, in which case this shot is CORRECT?
				TheaterPlay(1)										'Advance the play!
				Exit Sub
			Else			
				TheaterPlay(0)										'Incorrect shot, ghost will bitch!
				Exit Sub
			End If
		End If
	End If			
	'If none of those things are active, we do Spirit Guide (if lit)
	if (spiritGuide(player) = 255) Then								'Spirit guide not lit?
		video "S", "G", "Y", allowSmall, 42, 250					'Spell Team Members prompt
		Exit Sub													'Just give the ball back
	End If
	if (spiritGuide(player) = 1) Then								'Player has a Spirit Guide, but can we collect it?
		if (spiritGuideActive = 1) Then								'Spirit guide is LIT and available at the moment
			spiritGuideStart()										'Do routine
			Exit Sub
		Else
			video "S", "G", "X", allowSmall, 30, 250				'Available after mode ends
			Exit Sub		
		End If
	End If
End Sub

sub sendJackpot(whichNumber)										'Adds any multipliers to jackpot and sends that current # to the display
	'Most jackpots use a simple multipler. Check if it's a fancy one first.
	if (barProgress(player) = 80) Then								'Fighting ghost whore? Then it's special. So special.
		if (whoreJackpot < 10) Then									'Play the normal-ish ones for first 10 hits
			manualScore 0, EVP_Jackpot(player) + ((whoreJackpot + 1) * 100000)	'Update the value for score display		
		Else
			manualScore 0, EVP_Jackpot(player) + ((whoreJackpot + 1) * 200000)	'Update the value for score display
		End If	
		Exit Sub
	End If
	if (jackpotMultiplier = 0) Then									'Did we forget to set it?
		manualScore 0, EVP_Jackpot(player)							'Send normal value
	Else
		manualScore 0, EVP_Jackpot(player) * jackpotMultiplier		'Send multiplied value
	End If
End Sub

sub showProgress(modeStatus, whichPlayer)
	'modeStatus:
	'0000 - Not in a mode, show overall mode progress, path progress, ORB, GLIR
	'0001 - In a mode, show overall mode progress, ORB and GLIR  basically, just no paths
	if (HellSpeed)	Then										'In motion? Base this off where it's headed, not where it IS
		if (HellTarget = hellDown) Then
			light 41, 0											'Turn OFF Hell Flasher
		End If
		if (HellTarget = hellUp) Then
			blink(41)											'Turn ON Hell Flasher	
		End If						
	Else														'Not in motion? Normal check
		if (HellLocation = hellDown) Then						'State of Hellavator may have changed during mode so update its flasher to match its position. Notice how I used "its" properly both times?			
			light 41, 0											'Turn OFF Hell Flasher				
		End If		
		if (HellLocation = hellUp) Then		
			blink(41)											'Turn ON Hell Flasher		
		End If		
	End If	
	if (modeStatus = 0 and Advance_Enable = 1) Then				'Show all the mode path progress  1 2 3 indicators, etc
		if (priProgress(player) < 100) Then						'Able to advance or restart Prison?
			if (priProgress(player) = 0) Then					'Always fill lights
				pulse(3)
				light 4, 0
				light 5, 0
				light 6, 0		
			End If
			if (priProgress(player) = 1) Then					'Always fill lights
				light 3, 7
				pulse(4)
				light 5, 0
				light 6, 0		
			End If	
			if (priProgress(player) = 2) Then					'Always fill lights
				light 3, 7
				light 4, 7
				pulse(5)
				light 6, 0		
			End If			
			if (priProgress(player) = 3) Then					'Third advance?
				pulse(3)										'Pulse the 3 lights
				pulse(4)										'As player locks balls, lights go from Pulse to Solid
				pulse(5)
				blink(6)										'Blink "Prison Lock"
			End If	
			if (priProgress(player) > 2 and priProgress(player) < 8) Then		'Locking Balls?
				light 3, 7										'First 3 solid
				light 4, 7
				light 5, 7
				blink(6)										'Blink "Prison Lock"		
			End If		
		Else
			light 3, 0
			light 4, 0
			light 5, 0
			light 6, 0			
		End If
		if (hosProgress(player) < 90) Then						'Able to advance hospital
			if (hosProgress(player) = 0) Then
				pulse(8)
				light 9, 0
				light 10, 0
				light 11, 0		
			End If
			if (hosProgress(player) = 1) Then
				light 8, 7
				pulse(9)
				light 10, 0
				light 11, 0		
			End If
			if (hosProgress(player) = 2) Then
				light 8, 7
				light 9, 7
				pulse(10)
				light 11, 0		
			End If		
			if (hosProgress(player) = 3) Then
				light 8, 7
				light 9, 7
				light 10, 7
				pulse(11)		
			End If	
		Else													'Can't restart it!
			light 8, 0
			light 9, 0
			light 10, 0
			light 11, 0		
		End If
		if (hotProgress(player) < 100) Then						'Able to advance hotel?
			if (hotProgress(player) = 0) Then
				pulse(26)
				light 27, 0
				light 28, 0
				light 29, 0		
			End If
			if (hotProgress(player) = 1) Then
				light 26, 7
				pulse(27)
				light 28, 0
				light 29, 0		
			End If
			if (hotProgress(player) = 2) Then
				light 26, 7
				light 27, 7
				pulse(28)
				light 29, 0		
			End If		
			if (hotProgress(player) = 3) Then
				light 26, 7
				light 27, 7
				light 28, 7
				pulse(29)		
			End If	
		Else
			light 26, 0
			light 27, 0
			light 28, 0
			light 29, 0
		End If
		if (theProgress(player) < 100) Then						'Able to advance Theater?
			if (theProgress(player) = 0) Then
				pulse(36)
				light 37, 0
				light 38, 0
				light 12, 0		
			End If
			if (theProgress(player) = 1) Then
				light 36, 7
				pulse(37)
				light 38, 0
				light 12, 0		
			End If
			if (theProgress(player) = 2) Then
				light 36, 7
				light 37, 7
				pulse(38)
				light 12, 0		
			End If		
			if (theProgress(player) = 3) Then					'Ready to start?
				light 36, 7
				light 37, 7
				light 38, 7
				pulse(12)
				light 11, 0										'If doctor AND theater both ready, Theater gets priority
			End If	
		Else													'Can't start or re-start, all lights OFF
			light 36, 0											'Turn them all OFF
			light 37, 0
			light 38, 0
			light 12, 0	
		End If
		if (minionMB = 10) Then									'Is that going on as well?
			light 16, 0											'Turn OFF make contact
			pulse(17)											'Strobe target lights
			pulse(18)
			pulse(19)
			pulse(39)
		End If	
		if (minionMB = 20) Then									'Is that going on as well?
			pulse(16)											'Pulse MAKE CONTACT
			light 17, 0											'Turn off lights
			light 18, 0
			light 19, 0	
			pulse(7)
		End If			
	End If
	laneChange()												'Update ORB and GLIR
	'updateRollovers()											'Update ORB and GLIR
	if ((ModeWon(whichPlayer) AND 2)=2) Then					'Hospital?
		light 57, 7
	End If
	if ((ModeWon(whichPlayer) AND 4)=4) Then					'Theater?
		light 58, 7
	End If
	if ((ModeWon(whichPlayer) ANd 8)=8) Then					'Haunted bar?
		light 60, 7
	End If
	if ((ModeWon(whichPlayer) AND 16)=16) Then					'War fort?
		light 59, 7
	End If
	if ((ModeWon(whichPlayer) AND 32)=32) Then					'Hotel?
		light 61, 7
	End If
	if ((ModeWon(whichPlayer) AND 64)=64) Then					'Prison?
		light 62, 7
	End If		
	if (deProgress(whichPlayer) = 100) Then						'Already beat it once?
		light 63, 7												'Light is solid!
	End If
	if (wiki(player) < 255) Then
		pulse(0)
	Else
		light 0, 7
	End If
	if (tech(player) < 255) Then
		pulse(1)
	Else
		light 1, 7
	End If	
	if (psychic(player) < 255) Then
		pulse(51)
	Else
		if (scoringTimer) Then									'Double scoring active so the light blinks	
			blink(51)	
		Else
			light 51, 7											'Completed, so it's solid			
		End If	
	End If
	showScoopLights()
	if (extraLit(player)) Then									'Extra ball lit?
		pulse(15)												'Pulse the light			
	Else
		light 15, 0												'If not, that sucker should be OFF
	End If
	'Overall Progress Towards Demon Mode
	if (photosNeeded(player) > 3) Then							'Did you complete at least 1 photo hunt?
		light 50, 7	
	End If	
	if (hitsToLight(player) > 1) Then							'Completed a Hellavator Multiball?
		light 49, 7
	End If
	if (ModeWon(whichPlayer) = 126) Then						'Beat all Ghost Bosses?
		light 48, 7												'Light is solid!
	End If	
	if (minionsBeat(player) > minionMB1) Then					'Beat 3 or more minions?
		light 2, 7
	End If
End Sub

sub showScoopLights()
	if (theProgress(player) = 12) Then							'Waiting for third shot for Theater?
		TheaterStrobe()											'Turn the strobe back on, return
		Exit Sub
	End If
	'Show what the scoop can do.
	'Dimly light lights = available but not Top Priority
	light 43, 0
	light 44, 0
	light 45, 0
	light 46, 0
	light 47, 0
	Dim guideBright:guideBright = 7											'By default, these can be Bright (Pulsing)
	Dim glirBright:glirBright = 7											'If they get modifed, they will light differently
																'This code doesn't control priority, just makes the lights represent priority
	'CONDITIONS WHERE CAMERA LIGHT MIGHT BE ON
	if (hotProgress(player) = 20)	Then						'Searching for the Control Box?
		if (ControlBox(4) = 1) Then								'Did we already check here?
			light 47, 0											'Camera is OFF
		End If
		if (ControlBox(4) = 0 or ControlBox(4) = 255) Then		'Haven't checked there yet?
			pulse(47)											'Camera is PULSING
		End If
	End If
	if (tourLights(5) = 1) Then									'If a TOUR LIGHT is set here, make sure it resumes blinking
		blink(photoLights(5))
	End If
	if (barProgress(player) = 80) Then							'Ghost whore multiball?
		pulse(47)												'Pulse Scoop Camera for beer stealing
	End If
	if (Advance_Enable and fortProgress(player) = 50) Then		'Eligible to start War Fort?			
		guideBright = 1											
		glirBright = 1		
		pulse(44)
	End If
	if (Advance_Enable and barProgress(player) = 50) Then		'Eligible to start Bar?			
		guideBright = 1
		glirBright = 1		
		pulse(45)
	End If
	light 43, 0													'Default is GLIR off
	
	if (GLIRlit(player) = 129) Then								'GLIR is lit but has been disabled for this mode (MSB set)
		light 43, 1												'Light it dimly lest we forget about it
	End If
	if (GLIRlit(player) = 1) Then								'OK GLIR is lit and no specific limitation set on it (usually via Minion Mode)
		if (Advance_Enable) Then								'Modes can be started?
			pulse(43)											'Pulse that sucker						
			guideBright = 1										'... and indicate Spirit Guide is low priority (if it happens to be lit also)
		Else													'GLIR is dim
			light 43, 1
		End If	
	End If
	if (spiritGuide(player) = 1) Then							'Is it lit / has been earned (EARN THIS!!!)
		if (spiritGuideActive) Then								'Can it currently be collected?
			if (guideBright = 7) Then							'GLIR doesn't have priority?
				pulse(46)										'Pulse SPIRIT GUIDE
			Else
				light 46, guideBright							'Earned, but not what scoop will award at this time so dim
			End If			
		Else
			light 46, 1											'We earned it, but can't collect at this time, so dim
		End If	
	Else
		light 46, 0												'Not even lit, so it's off
	End If
End Sub

sub skillShotNew(show1st)													'Call this to randomly pick a Skill Shot and enable it (either on Player 1 Ball 1 or next player start)
	skillShot = random(3)													'Pick 1, 2 or 3
	skillShot = skillShot + 	1											'Pops = 1, ORBS = 2, Basement = 3
	'videoPriority(0)														'Zero out video priority
	if (numPlayers = 1) Then												'In single player games, do not indicate Player #	
		customScore "K", "0", 64 + skillShot, allowSmall OR loopVideo, 120		'Custom Score for skill shot
		'video('K', '0', 64 + skillShot, loopVideo | allowSmall, 0, 1)
		if (ball > 1) Then
'			playSFX 0, 'S', '0' + skillShot, 'A' + random(3), 255)			'Psychic skill shot prompt
			PlaySFX 0, "S", skillShot, 65 + random(3), 255
		End If
	Else																	'Multiplayer, show which player is up and has the skill shot
		customScore "K", player, 64 + skillShot, allowSmall OR loopVideo, 120		'Custom Score for skill shot
		'video('K', '0', 64 + skillShot, loopVideo | allowSmall, 0, 1)
'		numbers(10, 2, 44, 27, numPlayers)									'Number of players, just after Player X current indicator
';		numbers "", "", "   "&numPlayers, ""
		if (ball > 1) Then
			playSFX 0, "S", skillShot, 65 + random(3), 255					'Psychic skill shot prompt				
		End If
	End If	
	numbers PlayerScore(player), "", "", ""									'Put player score upper left, using Double Zeros
	numbers "", Ball, "", ""												'Ball # upper right	
End Sub

sub skillShotSuccess(didSucceed, showMiss)					'What happens when you make the skill shot
	Dim skillValue
	'This happens no matter what
	ballSave()												'Start the ballsaver at this point and check what to do with Spook Again light
	'killQ()												'Disable any queued videos
	'killNumbers()
	killScoreNumbers()										'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()		
	skillValue = 0
	if (didSucceed) Then
		video "K", "0", "0", 0, 55, 255						'Top priority success video!
		playSFX 1, "A", "E", random(8), 255					'Success Dialog! (temp)
		Select Case (skillShot)								'Gonna be 1, 2 or 3
			case 1:
				skillValue = 250000							'Pops a bit harder
			case 2:
				skillValue = 125000							'Orbs easiest
			case 3:
				skillValue = 500000							'Behind hellavator hardest
		End Select
		showValue skillValue * ball, 40, 1					'Show the value after video		
		AddScore skillValue * ball							'500k pops, 1 mil ORB, 1.5 mil Basement X Ball # (so worth more on Ball 3)			
	Else
		if (showMiss) Then
			video "K", "0", "1", 0, 22, showMiss			'Put in a SKILL SHOT MISSED graphic here (very low priority)
			playSFX 0, "Q", "Z", "Z", 200					'Else, negative sound!	
			'playSFX(0, 'S', 'H', '0' + random(8), 255)		'Give player shit
		Else
			DMDScore()										'EP- Added to clear up the DMD queue
		End If
	End If
	skillShot = 0											'Disable skill shot
	modeTimer = 0											'Reset the timer
End Sub

sub skippable()									'If a skippable event is active (SKIP > 0) this function decides what to jump to if player chooses to speed things up
	Select Case skip							'Choose what to do!
		case 10:								'Hospital Ghost Stuff (probably just the beginning)
			if (plungeTimer > Int(25010/CycleAdjuster)) Then			'Waiting for second ball?
				plungeTimer = Int(25010/CycleAdjuster) + 1				'Set it to near-immediate load state
			End If
		case 20:								'Theater Ghost (beginning and every subsequent shot!)
			if (LeftTimer > Int(6010/CycleAdjuster)) Then				'Beginning of theater? (Eject from behind door)
				LeftTimer = Int(6010/CycleAdjuster)
				modeTimer = longSecond * 2		'Remove grace period on timer
			End If
		case 21:								'Theater Ghost second shot, waiting for elevator to go down
			if (HellLocation > hellDown) Then		'Did the Hellavator not make it to the stuck position yet?
				ElevatorSet hellDown, 5		'Speed up the hellavator!
				modeTimer = longSecond * 2
			End If
		case 22:								'Theater Ghost third shot waiting for left VUK?
			if (LeftTimer > Int(6010/CycleAdjuster)) Then
				LeftTimer = Int(6010/CycleAdjuster)
				modeTimer = longSecond * 2		'Remove grace period on timer
			End If
		case 23:								'Theater Ghost (beginning and every subsequent shot!)
			if (ScoopTime > Int(9010/CycleAdjuster)) Then				'Waiting for ball to eject scoop?
				ScoopTime = Int(9010/CycleAdjuster)				'KIck it out now!
				TargetTimerSet 10, TargetDown, 50	'Put targets down quickly
				modeTimer = longSecond			'Remove grace period
			End If
		case 30:								'Bar ghost "waiting for embrace"
			if (ScoopTime > Int(9010/CycleAdjuster)) Then
				ScoopTime = Int(9010/CycleAdjuster)
				TargetTimerSet 10, TargetDown, 30	'Put targets down quickly
			End If
		case 35:								'Bar ghost "waiting for embrace"
			if (plungeTimer > Int(25010/CycleAdjuster)) Then			'Waiting for second ball?
				plungeTimer = Int(25010/CycleAdjuster) + 1			'Set it to near-immediate load state
			End If
		case 40:								'War ghost, probably just the mode opening (rest is pretty fast)
			if (ScoopTime > Int(9010/CycleAdjuster)) Then
				TargetTimerSet 10, TargetUp, 50	'These might not be up yet, so do it now quickly
				ScoopTime = Int(9010/CycleAdjuster)
			End If
		case 50:								'Hotel ghost. Elevator move 1, elevator move 2 + scoop eject
			if (HellLocation > hellStuck) Then		'Did the Hellavator not make it to the stuck position yet?
				ElevatorSet hellStuck, 5		'Speed up the hellavator!
			End If
		case 55:								'Hotel ghost. Waiting for ball to get ejected from scoop
			if (ScoopTime > Int(9010/CycleAdjuster)) Then				'Waiting for ball to start Control Box Search?
				ScoopTime = Int(9010/CycleAdjuster)				'KIck it out now!
			End If
		case 60:								'Scoop eject while Ghost is talking about devouring friends
			if (ScoopTime > Int(9010/CycleAdjuster)) Then				'Waiting for ball to start Control Box Search?
				ScoopTime = Int(9010/CycleAdjuster)				'KIck it out now!
			End If
		End Select
	skip = 0									'Reset skip no matter what
';	video "K", "A", "A", 0, 0, 255	'White to black transition
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
	End If
End Sub

sub spiritGuideStart()															'When you shoot into the scoop with Spirit Guide lit
	'pulse(46)
	light 46, 0																	'Turn off its light
	'volumeSFX(3, 100, 100)														'Temp music volume increase
	if (tournament) Then
		spiritGuide(player) = spiritProgress(player)							'If in tourney mode, see what award is next	
	End If
	Do while (spiritGuide(player) < 99)											'Repeat this until we give an award that doesn't conflict with anything going on
		if (tournament = 0) Then
			spiritGuide(player) = random(18)									'18 Get a random number
		End If
																				'If that award is valid, we add 100 to its value to leave this loop
																				'There are several points awards so there's always something valid
		Select Case spiritGuide(player)											'Turn off the lights when we hit them
			case 0: 'Light GLIR
				if (Advance_Enable = 1 and (GLIR(player) > 0 and GLIR(player) < 100) and Mode(player) = 0 and minion(player)) Then
					spiritGuide(player) = spiritGuide(player) + 100				'Approved, continue
				End If
			case 1: 'Lite HOSPITAL
				if (Advance_Enable = 1 and (ModeWon(player) AND 2) = 0 and hosProgress(player) < 3 and theProgress(player) < 3) Then	'Hospital hasn't been won, or in progress?
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 2: '500,000 points
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 3: 'Reveal Minion?
				if (Advance_Enable = 1 and minion(player) = 1 and videoMode(player) = 0) Then	'Not in a mode, and Minion is able to be started, and we're not waiting for Video Mode start?
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 4: 'Lite theater?
				if (Advance_Enable = 1 and (ModeWon(player) AND 4) = 0 and theProgress(player) < 3 and hosProgress(player) < 3) Then	'Can't enable Theater and Hospital at same time
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 5: '1,000,000 points
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 6: 'Advance Bonus?
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 7: 'Lite War Fort?
				if (Advance_Enable = 1 and (ModeWon(player) AND 8) = 0 and fortProgress(player) < 50 and barProgress(player) < 50 and videoMode(player) = 0) Then	'Can't enable War and Bar at the same time
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 8: '2,000,000
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 9: ' 30 seconds Ball Save
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 10: 'Lite Haunted Bar?
				if (Advance_Enable = 1 and (ModeWon(player) AND 16) = 0 and fortProgress(player) < 50 and barProgress(player) < 50 and videoMode(player) = 0) Then	'Can't enable War and Bar at the same time
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 11: '3,000,000 points
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 12: 'Start Multiball?
				if (Advance_Enable = 1 and lockCount(player) = 0 and multiBall = 0 and minionMB = 0 and videoMode(player) = 0) Then
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 13: 'Lite Hotel?
				if (Advance_Enable = 1 and (ModeWon(player) AND 32) = 0 and hotProgress(player) < 3) Then
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 14: '3,666,000
				spiritGuide(player) = spiritGuide(player) + 100					'Award approved, proceed
			case 15: 'Award EVP
				spiritGuide(player) = spiritGuide(player) +  100				'Award approved, proceed
			case 16: 'Lite Prison Lock
				if (Advance_Enable = 1 and (ModeWon(player) AND 64) = 0 and priProgress(player) < 3) Then	'If not in a mode, or doing this already, it's a good award
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			case 17: 'Lite extra ball?
				if (ball > 2 and allowExtraBalls) Then							'Won't give you one on Ball 1 or 2, hahaha! Or if they're disabled
					spiritGuide(player) = spiritGuide(player) + 100				'Award approved, proceed
				End If
			End Select
		if (tournament and spiritGuide(player) < 99) Then						'Current award wasn't valid?
			spiritGuide(player) = spiritGuide(player) + 1						'If in tourney mode, see what award is next
			spiritProgress(player) = spiritProgress(player) + 1					'Advance our progress as well
			if (spiritGuide(player) > 17) Then									'Did we somehow get them ALL???
				spiritGuide(player) = 0											'Reset back to 0
				spiritProgress(player) = 0		
			End If
		End If
	Loop
	killQ()																		'Disable an Enqueued videos
	spiritProgress(player) = spiritProgress(player) + 1							'Advance our progress since we collected that one
	if (spiritProgress(player) > 17) Then										'Did we somehow get them ALL???											
		spiritProgress(player) = 0												'Reset back to 0		
	End If
	if (Advance_Enable) Then
		playMusicOnce "S", "G"													'Switch to Spirit Guide Theme
		playSFX 2, "S", "G", random(9), 255										'Spirit Guide!	
		video "S", "G", spiritGuide(player) - 35, 0, 84, 255					'Play video A-R	
		ScoopTime = Int(60000/CycleAdjuster)									'Award is given as ball is shot out (we'll have to play with the timing)		
	Else
		video "U", "Q", spiritGuide(player) - 35, 0, 32, 255					'Play video A-R	
		ScoopTime = Int(25000/CycleAdjuster)									'Award is given as ball is shot out (we'll have to play with the timing)	
		playSFX 2, "S", "G", "B", 255											'Orchestra hits...
		playSFX 1, "S", "G", random(9), 255										'Spirit Guide!	
	End If
End Sub

sub spiritGuideAward()															'It gives you the award and spits out ball
	Dim X
	'volumeSFX(3, musicVolume(0), musicVolume(1)) 'Revert to normal volume
	spiritGuide(player) = spiritGuide(player) - 100								'Set the number back to normal
	Select Case spiritGuide(player)												'Award whatever prize we came up with. It's already approved, so just DO IT
		case 0: 'Light GLIR
			if (GLIRneeded(player) < 9) Then									'Getting free GLIR also increases spellings required to get more (to be fair)
				GLIRneeded(player) = GLIRneeded(player) + 1						'Increase target #	needed, max is 9		
			End If
			GLIR(player) = GLIRneeded(player)									'Set counter to new target #				
			GLIRlit(player) = 1													'Set flag			
			rollOvers(player) = 0												'Clear rollovers
			blink(52)															'Blink GLIR for a bit
			blink(53)
			blink(54)
			blink(55)
			showScoopLights()													'Update lights		
			displayTimerCheck(89999)											'Check if anything was running, set new value						
			playSFX 0, "F", "1", 65 + random(4), 200							'"Photo Hunt is Lit!" prompt. Higher priority, will override normal rollover sound
			video "F", "1", "A", allowSmall, 45, 200							'GLIR, photo hunt is lit!			
		case 1: 'Lite HOSPITAL
			hosProgress(player) = 3
			for x=0 To hosProgress(player)-1									'in case we did a Double Advance
				light x + 8, 7													'Completed lights to SOLID
			Next
			pulse hosProgress(player) + 8										'Pulse DOCTOR GHOST light
			video "H", hosProgress(player), "A", allowSmall, 75, 200		'Prompts to shoot for it
			playSFX 0, "H", hosProgress(player), random(4) + 65, 255
			DoorSet DoorOpen, 500												'Set door to creak open, 25 cycles per position
		case 2: '500,000 points
			AddScore(500000)
		case 3: 'Reveal Minion?
			minionStart()
		case 4: 'Lite theater?
			theProgress(player) = 3												'Set progress
			light 36, 7
			light 37, 7
			light 38, 7
			pulse(12)
			if (hosProgress(player) = 3) Then									'Can only start one or the other, Theater has priority
				light 11, 0
			End If
			playSFX 0, "T", "3", random(4) + 65, 255
			video "T", "3", "A", allowSmall, 90, 255							'Play video
			light theProgress(player) + 34, 7									'Solid progress light
			pulse(12)															'Blink light 12 for Theater Start	
			DoorSet DoorOpen, 50												'Open the door.
		case 5: '1,000,000 points
			AddScore(1000000)
		case 6: 'Advance Bonus?
			orb(player) = 63													'Manually set them to Rolled Over
			checkOrb(1)
		case 7: 'Lite War Fort?
			video "W", "0", "0", 0, 90, 255										'Prompt for Army Ghost Lit		
			playSFX 0, "W", "3", random(4) + 65, 250 							'Prompt for Mode Start					
			fortProgress(player) = 50											'50 indicates Mode is ready to start.				
			popLogic(3)															'EVP pops	
			spiritGuideEnable(0)		
			showScoopLights()													'Update the Scoop Lights
		case 8: '2,000,000
			AddScore(2000000)
		case 9: ' 30 seconds Ball Save
			saveTimer = 30 * cycleSecond										'Huge ball saver!
			spookCheck()														'See what to do with the light
			'blink(56)															'Blink the SPOOK AGAIN light
		case 10: 'Lite Haunted Bar?
			video "B", "4", "0", 0, 90, 255										'Prompt for Bar Ghost Lit		
			playSFX 0, "B", "3", random(4) + 65, 255 							'Advance sound 3							
			barProgress(player) = 50											'50 indicates Mode is ready to start.			
			popLogic(3)															'Pops won't do anything else until you start the mode
			spiritGuideEnable(0)	
			showScoopLights()													'Update the Scoop Lights
		case 11: '3,000,000 points
			AddScore(3000000)
		case 12: 'Start Multiball?												'NEEDS MANUAL SETTINGS!!!
			stopMusic()
			blink(26)															'Need to do a few things manually...
			blink(27)
			blink(28)		
			multiBallStart(0)
			if (hellMB = 1) Then
				hellMB = 10														'Set flag that music / mode has begun!
				volumeSFX 3, musicVolume(0), musicVolume(1)						'Back to normal
'				playMusic "M", "B"												'The multiball music!
				musicplayer "bgout_MB.mp3"
				multipleBalls = 1												'When MB starts, you get ballSave amount of time to loose balls and get them back
				ballSave()														'That is, Ball Save only times out, it isn't disabled via the first ball lost							
			End If
		case 13: 'Lite Hotel?
			hotProgress(player) = 3
			playSFX 0, "L", hotProgress(player), random(4) + 65, 255		'First 3 sets of Hotel advance sounds.
			video "L", hotProgress(player), "A", allowSmall, 60, 255		'Adance videos
			light 26, 7														'Light hotel status solid
			light 27, 7
			light 28, 7
			pulse(29)															'Pulse HOTEL GHOST
			ElevatorSet hellUp, 200												'Move the elevator into 2nd floor position
			blink(41)
			light 24, 0															'Turn off CALL ELEVATOR lights
			light 25, 0
		case 14: '3,666,000
			AddScore(3666000)
		case 15: 'Award EVP
			popCount = EVP_Target - 1											'Set it so we'll get one
			evpPops()
		case 16: 'Lite Prison Lock
				priProgress(player) = 3											'Set progress
				light 3, 7														'Make 3 lights solid
				light 4, 7														'As player locks balls, lights go from Pulse to Solid
				light 5, 7
				blink(6)														'Blink "Prison Lock"
				playSFX 0, "P", "3", random(3) + 65, 255						'Play 1 of 3 audio clips	
				video "P", "3", "A", allowSmall, 90, 255							'Run video	
		case 17: 'Lite extra ball?
			extraBalllight 1
		End Select
	spiritGuide(player)	= 255													'Flag that Spirit Guide needs to be re-lit
End Sub

sub spiritGuideEnable(enableYesNo)
	spiritGuideActive = enableYesNo				'Set incoming state
	showScoopLights()							'Scoop lights will show updated state
End Sub

sub spiritGuidelight 
	if (spiritGuide(player)	= 255) Then											'Needs to be re-lit?
		spiritGuide(player)	= 1													'Set Spirit Guide as active
		if (spiritGuideActive) Then												'Can we hit Spirit Guide?
			pulse(46)
			videoQ "S", "P", "Z", allowSmall, 0, 10								'Spirit Guide LIT!				
		Else
			light 46, 0
			videoQ "S", "P", "Y", allowSmall, 0, 10								'Spirit Guide ready after mode ENDS
		End If
	End If
End Sub

sub spookCheck()									'See what the Spook Again light should be doing
	if (drainTimer or tiltFlag) Then				'First, see if game is drained or tilted.
		if (extraBalls) Then						'If extra ball active, leave the light solid
			light 56, 7
		Else
			light 56, 0								'Otherwise turn it off (either ball save = 0 or you tilted)
		End If
	Else											'Not in a tilt/drain condition? We either have ball save, extra ball lit or nothing lit
		if (extraBalls) Then						'An extra ball?
			if (saveTimer > cycleSecond2) Then		'If save time is active, keep blinking it until save timer is over
				blink 56
			Else
				light 56, 7							'No more save timer? Light SPOOK AGAIN solid (this code will get called when Save Timer finishes)
			End If		
		Else
			if (saveTimer <= cycleSecond2) Then		'Save timer either done or just about done?
				light 56, 0							'Turn it off
			Else
				blink 56							'Else it's still active so blink
			End If	
		End If
	End If
End Sub

sub StartGame()										'Resets all variables, player progress, sets up initial lights
	Dim x
	animatePF 0, 0, 0								'Kill attract mode animations
	menuAbortFlag = 0								'In case user tries to enter a menu during a game
	Enable()										'Allow solenoids
	videoPriority(0)								'Reset video priority	
	killNumbers()									'Clear all numbers
	backglass 1, 1									'Can't remember what this does
	setGhostModeRGB 0, 0, 0							'Set ghost to off
'	ballSearchDebounce(0)							'In case the trap debounce was changed during a ball search
	kickFlag = 0									'Clear flag, ball kick complete
	drainTries = 0	
	GIpf(224)									'All GI on to start
'	sfxVolume(0) = sfxDefault
'	sfxVolume(1) = sfxDefault
'	volumeSFX(0, sfxVolume(0), sfxVolume(1))		
'	volumeSFX(1, sfxVolume(0), sfxVolume(1))		
'	volumeSFX(2, sfxVolume(0), sfxVolume(1))	
'	musicVolume(0) = musicDefault
'	musicVolume(1) = musicDefault
'	volumeSFX(3, musicVolume(0), musicVolume(1))
	repeatMusic(1)									'Set music to repeat
'	randomSeed(micros())							'Reset randomizer EP- Already did this at the top of the script
	allLamp(0)										'Clear the lights
	'Set all game start variables here:
	player = 1
	numPlayers = 1
	ball = 1
	'SET ALL STARTING LAMPS
	pulse(3)										'PRISON 1 and THEATER 1 are different in production version
	pulse(8)
	pulse(22)
	pulse(26)
	pulse(36)										'PRISON 1 and THEATER 1 are different in production version
	pulse(0)										'Wiki, Tech and Psychic pulse
	pulse(1)
	pulse(51)
	'Reset all player Progress
	for x=1 To 4
		popMode(player) = random(2) + 1				'Starts either 1 = Fort or 2 = Bar (3 = EVP but never starts in that mode)
		if (SwLFlip = 1) Then							'Left flipper at start makes it begin with Bar
			popMode(player) = 2
		End If
		if (SwRFlip = 1) Then							'Right flipper = fort
			popMode(player) = 1
		End If
		if (popMode(player) = 1) Then				'Start out Advancing Fort?
			light 20, 0
			light 21, 0
			light 22, 0
			pulse(21)
		End If
		if (popMode(player) = 2) Then				'Start Out Advancing Bar?
			light 20, 0
			light 21, 0
			light 22, 0
			pulse 22
		End If
		Mode(x) = 0
		playerScore(x) = 0						'Clear scrores
		SetScore(x)								'Clear them on DMD
		replayPlayer(x) = 0						'Nobody's gotten a replay yet
		ModeWon(x) = 0							'Clear the bits of what modes they've won
		modeRestart(x) = 126 			'B01111110				'At the start each player gets 1 re-start chance per mode
		tourComplete(x) = 0				'B00000000				'Which tours player has completed	
		hosProgress(x) = 0
		theProgress(x) = 0
		barProgress(x) = spotProgress			'Can be changed in settings
		fortProgress(x) = spotProgress			'Can be changed in settings
		hotProgress(x) = 0
		priProgress(x) = 0
		deProgress(x) = 0
		lockCount(x) = 0						'Reset # of locked Hellavator balls
		spiritProgress(x) = 0					'If tourney mode, players get awards in sequence
		spiritGuide(x) = 1						'Spirit guide is always lit to start for each player	
		EVP_Jackpot(x) = 1000000				'Starts at 1 million
		EVP_Total(x) = 0						'How many EVP's each player has collected
		EVP_EBtarget(x) = EVP_EBsetting			'Load the setting for how many EVP's each player must get to earn Extra Ball
		photosTaken(x) = 0						'Total photos a player got.
		areaProgress(x) = 0						'How many mode-advancing shots each player has made
		ghostsDefeated(x) = 0
		photosNeeded(x) = 3
		photoSecondsStart(x) = 21				'How many seconds to get a ghost photo
		GLIR(x) = 1								'At the start of the game, spell GLIR once to light Photo hunt, then twice, 3 times, ect.
		GLIRneeded(x) = 1						'How many GLIR spells player needs to light PHOTO HUNT
		GLIRlit(x) = 0							'Zero PHOTO HUNTS lit to start
		minion(x) = 1							'Each player starts with Minion Fight enabled
		minionTarget(x) = 3						'3 hits for first Minion Battle
		minionHits = 3							'Reset this
		minionsBeat(x) = 0						'How many minions you've beaten.
		minionHitProgress(x) = 0				'No minion damage progress yet
		orb(x) = 0
		extraLit(x) = 0							'No extra balls lit
		wiki(x) = 0
		tech(x) = 0
		psychic(x) = 0	
		rollOvers(x) = 0
		hellLock(x) = 1							'Always start with Hell lock enabled		
		storeLamp(x)							'Store the status of every lamp in that player's memory
		hellJackpot(x) = 1000000				'Starting MB jackpot value
		hitsTolight(x) = 1						'How many times you have to press "Call" before hellavator moves / lights for lock	(starts with just 1						
		saveCurrent(x) = saveStart				'Set each player's Ball Save time to the default to start. Spelling TECH can increase it!
		videoMode(x) = 0						'Video mode not lit to start
	Next
	scoreMultiplier = 1							'This will almost always be 1
	comboTimerStart = comboSeconds * longSecond	'Compute actual timer setting
	minionDamage = 1							'Default damage
	TargetSet(TargetUp)							'Put targets UP by default so we can enagage Minion Battle!
	callHits = 0								'How many times you've hit Call this ball (resets per player)	
	'deProgress(1) = 1							'TEST DEMON MODE READY TO START
	'blink(13)									'BLINK LIGHT	
	trapDoor = 0								'Flags if the ball should be trapped or not
	trapTargets = 0
	doorLogic()									'Figure out what to do with the door
	elevatorLogic()								'Can lock balls, Hellavator is Lit		
	targetLogic(1)								'Where the Ghost Targets should be, up or down
	targetReset()								'Reset state of targets
	multiTimer = 0								'Used in attract mode, so clear it just in case
	multiCount = 0
	slingCount = 0	
	spiritGuideEnable(1)						'Mode 0, it can always be lit		
	bonusMultiplier = 1							'Reset multiplier (it's per ball so don't need unique variable per player)		
	modeTimer = 0	
	HellBall = 0
	badExit = 0									'Haven't gone in VUK yet
	tiltCounter = 0								'Reset to zero
	ghostLook = 0
	sweetJumpBonus = 0							'Reset score (hitting it adds value)
	sweetJump = 0								'Reset video/SFX counter
	Advance_Enable = 1							'Game starts with all modes eligible for advancement.
	AutoEnable = 255 							' 255 /enables everything
	activeBalls = 0								'Starts at ZERO	
	setCabMode defaultR, defaultG, defaultB		'Set the cab mode to default color
	comboKill()
	dirtyPoolMode(1)							'Check for Dirty Pool Balls
	Update(0)									'Update with the current info
	video "K", "9", "9", 0, 0.2, 255			'STATIC transition
	skillShotNew(1)								'Prep a Skill Shot!	
	scoreBall = 0								'No points scored on this ball as yet
	comboEnable = 1								'OK combo all you want
	GLIRenable(1)	
'	playMusic "L", "1"
	musicplayer "bgout_L1.mp3"
	playSFX 0, "A", "A", 1 + random(4), 255		'Team leader intro lines
	ballQueue = 0
	drainSwitch = 63							'Set starting drain switch, just in case
	loadBall()									'Manually load a ball into shooter lane.	
	GhostMove 90, 10							'EP- Adding this since I added the ghost turning away at machine reset
End Sub

Sub SwitchCheck()								'EP- This is event driven so we don't need to check switches every cycle
End Sub

sub switchDeadCheck()
	if (coinDoorState = 0) Then					'Do we care if door is open, and is it open?
		switchDead = 0							'Reset timer
		Exit Sub								'No ball search with door open
	End If
	if (LFlip=0 and (RFlip)=0 and ballSearchEnable=1 and Sw57=0) Then 'We're not cradling the ball, and the ball isn't sitting in the shooter lane? Do a ball search!
		if (switchDead = deadTop + 1) Then
			if (trapTargets = 0) Then
				if (TargetLocation >= TargetUp) Then	'Targets up?
					TargetSet(TargetDown)				'Put targets directly down
					TargetTimerSet 12000, TargetUp, 10'After a second, put them back up
				Else
					TargetSet(TargetJog)				'Put targets up partially
					TargetTimerSet 8000, TargetDown, 10 'And back down quickly
				End If					
			Else
				TargetSet(TargetJog)					'Put targets down partially
				TargetTimerSet 8000, TargetUp, 10		'After a second, put them back up										
			End If
			if (HellLocation = hellUp) Then
				hellCheck = 10					'Set state 1
				ElevatorSet hellDown, 100
			End If
			if (HellLocation = hellDown) Then
				hellCheck = 20					'Set state 2
				ElevatorSet hellUp, 100
			End If			
		End If		
		if (switchDead = deadTop + (searchTimer)) Then
			if (trapDoor = 0) Then							'Don't kick this if a ball is SUPPOSED to be trapped behind door
'				Coil LeftVUK, vukPower
				VUKKicker KiDoor, vukPower	
			Else
				if (hosProgress(player) > 5 and hosProgress(player) < 9 and hosTrapCheck = 0) Then	'Ball trapped for Hospital Mode?
					Coil LeftVUK, vukPower
					VUKKicker LeftVUK, vukPower
					activeBalls  = activeBalls  +  1										'Add the ball we just kicked out back to the Active Ball count
					LeftTimer = 0											'A little bit of a gap
'					swDebounce(23) = 500									'Manually set the debounce
					hosTrapCheck = 1										'Hospital ball search mode
				End If
			End If
		End If	 
		if (switchDead = deadTop + (searchTimer * 2)) Then
			Coil ScoopKick, scoopPower
			VUKKicker KiVUK3, scoopPower
			if (DoorLocation = DoorOpen) Then
				doorCheck = 20					'Set state 1
				DoorSet DoorClosed, 500
			End If
			if (DoorLocation = DoorClosed) Then
				doorCheck = 10					'Set state 2
				DoorSet DoorOpen, 1
			End If			
		End If
		if (switchDead = deadTop + (searchTimer * 3)) Then
'			Coil drainKick, drainStrength
		End If	
		if (switchDead = deadTop + (searchTimer * 4)) Then
'			Coil Bump0, PopPower		
		End If
		if (switchDead = deadTop + (searchTimer * 5)) Then
'			Coil Bump1, PopPower		
		End If
		if (switchDead = deadTop + (searchTimer * 6)) Then
'			Coil Bump2, PopPower	
			switchDead = Int(50000/CycleAdjuster)
		End If	
	Else
		switchDead = 0											'Ball isn't actually trapped, reset timer
	End If
End Sub

sub TargetSet(dTarget)												'Puts the Target Bank to a specified position
	if (dTarget = TargetUp) Then
		dirtyPoolCheck()
	End If
	TargetLocation = dTarget
'	TargetTarget = dTarget											'EP- I added this
'	myservo(Targets).write(TargetLocation)
	WaGhostTarget.TimerEnabled = 1
End Sub

sub TargetTimerSet(dDelay, dTarget, dSpeed)
'	TargetSpeed = 0													'Clear this flag so we don't move until after delay, EP- commenting out because this was causing it not to move when it should
	TargetDelay = Int(dDelay/cycleadjuster)							'How long before Targets start to move
	If TargetDelay < 1 Then TargetDelay = 1
	TargetTarget = dTarget											'Where to move to.	
	TargetNewSpeed = ((((TargetFast - TargetSlow)/500) * (500 - dSpeed)) + DoorSlow)*2		'What the speed will be when we start	
	TargetTimer = 0													'Reset cycle timer
End Sub

sub targetLogic(resetMinion)
	'showGameStatus()				'For debugging
	if (videoMode(player) > 0 and Advance_Enable and Mode(player) = 0) Then		'Video mode is available, and we're not in a mode?
		videoModeLite()
		Exit Sub
	End If
	if (hosProgress(player) > 5 and hosProgress(player) < 9) Then				'Friend trapped behind door?
		TargetSet(TargetUp)														'Keep targets UP		
		Exit Sub
	End If
	if (hosProgress(player) = 90) Then											'Bashing Dr. Ghost? (not paging him)
		TargetSet(TargetDown)													'Keep targets DOWN		
		Exit Sub
	End If	
	if (barProgress(player) = 60 or barProgress(player) = 80) Then				'Ghost waiting for your embrace, or Bashing Ghost Whore Multiball?
		TargetSet(TargetDown)													'Put targets down, so player can restart Ghost Whore
		Exit Sub
	End If
	if (barProgress(player) = 70) Then											'Ghost Whore has your friend trapped still?
		dirtyPoolMode(0)														'Don't check for Dirty Pool
		TargetSet(TargetUp)														'Keep targets UP
		Exit Sub
	End If
	if (fortProgress(player) = 60) Then											'Fighting the Army Ghost Soldiers?
		TargetSet(TargetUp)														'Targets should be UP
		Exit Sub
	End If
	if (fortProgress(player) > 69 and fortProgress(player) < 100) Then			'Fighting the Army Ghost Himself?
		TargetSet(TargetDown)													'Make sure targets are down so we can hit him
		Exit Sub
	End If
	if (minionMB > 9) Then														'Doing a Minion MB?
		Exit Sub																'Do nothing
	End If
	if (minion(player) > 9) Then												'Was fighting a minion during Photo Hunt?
		Exit Sub
	End If
	TargetSet(TargetUp)															'Default is TARGETS UP
	if (resetMinion) Then
		pulse(17)																'Ghost targets strobe for MINION BATTLE!
		pulse(18)
		pulse(19)
		light 16, 0																'Turn off Jackpot by default
		minionEnd(1)															'The default is to enable the Minion Battle	
	End If
End Sub

Sub targetReset()
	targetsHit = 0													'We've hit no targets
	TargetBits = 7												'Set targets as NOT HIT
	gTargets(0) = 0													'Set G Targets to NOT HIT
	gTargets(1) = 0
	gTargets(2) = 0
End Sub

sub Timers()																'Check all game function timers.
	'SwitchLogic()															'Debounce timers and stuff
	if (drainTimer) Then													'Are we in a drain?
		drainTimer = drainTimer - 1		
		DrainLogic()
		ballClear()															'Make sure locks are clear
	Else																	'Normal function
		if (HellSpeed) Then													'Is the elevator supposed to be moving?
'			MoveElevator()													'Do routine.
		End If	
'		if (TargetSpeed) Then												'Is the target supposed to be moving?
'			MoveTarget()
'		End If
'		if (DoorSpeed) Then													'Is the door supposed to be moving?
'			MoveDoor()														'Do routine.
'		End If	
	End If
	if (plungeTimer) Then													'Auto-plunge in progress?
		plungeTimer = plungeTimer - 1										'Decrement counter
		if (plungeTimer = Int(25001/CycleAdjuster)+1 and Sw59 = 0) Then	'bitRead(switches(7), 3) = 0) Then		'At first event point, but ball not ready to be loaded?				
			'Serial.println("BALL NOT READY")
			plungeTimer = plungeTimer + Int(10000/CycleAdjuster)			'Give it more time to roll in place
		End If
		if (plungeTimer = Int(25000/CycleAdjuster)) Then						'Second event point. A ball must have been in the load position to get here
			'Serial.print("LOAD BALL: ")
			'Coil(LoadCoil, loadStrength)									'Try to load ball from trough
			ServeBall()														'EP- my version of trying to load the ball
			skip = 0
		End If
		if (plungeTimer > Int(5001/CycleAdjuster)+1 and plungeTimer < Int(25000/CycleAdjuster)) Then					'At any point after the ball load?
			if (Sw57 = 1) Then 										'bitRead(switches(7), 1) = 1) Then								'As soon as the ball hits the switch...
				'Serial.println("LOAD GOOD")
				activeBalls = activeBalls + 1								'...Set a new ball as officially active! If you swat it away before autoplunge, doesn't matter
				drainSwitch = drainSwitch - 1								'Advance which switch drains the ball
				'Serial.print("-Drain Switch = ")
				'Serial.println(drainSwitch, DEC)
				if (run = 1) Then											'Start of game or new ball?
					run = 2
					'spookCheck()											'Save timer won't be started until Skill Shot collected. Thus spookCheck won't give proper result
					blink(56)												'A new game/ball will always have a ball save, so blink it manually here												
					launchCounter = 0
					plungeTimer = 0											'Don't autoplunge it
					'Serial.println("Start-of-Ball Load Complete")
				Else
					plungeTimer = Int(5000/CycleAdjuster)						'Set plunge point. This gives ball a 4000 cycle delay to "settle"				
				End If
			End If
		End If
		if (plungeTimer = Int(5001/CycleAdjuster)+1) Then							'Ready to "take the plunge?" Make sure ball is there...
			if (Sw57 = 0) Then 										'bitRead(switches(7), 1) = 0) Then							'Ball isn't there?
				'Serial.println("LOAD FAIL - RETRY")
				plungeTimer = Int(25001/CycleAdjuster)+1							'Reset timer to try and re-load ball from trough
			End If
		End If
'		if (plungeTimer = 1000 and bitRead(switches(7), 1) = 1) Then		'At second event point, and a ball is there to launch?
		if (plungeTimer = Int(1000/CycleAdjuster) AND Sw57 = 1) Then			'EP-
			'Serial.println("Auto-plunging NOW")
'			Coil(Plunger, plungerStrength)									'EP- Gotta put in my ball launcher routine
			AutoPlunger.AutoFire
			DOF 124, 2
			if saveTimer > 42 Then DOF 131, 2
		End If
		if (plungeTimer = 0 and ballQueue and countBalls() > 0) Then		'Was another autolaunched ball queued during previous launch? (somehow?)
			ballQueue = ballQueue - 1
			AutoPlunge(autoPlungeFast + 5000)								'Launch another fairly quickly
		End If
	End If
	if (LeftOrbitTime) Then
		LeftOrbitTime = LeftOrbitTime - 1
	End If
	if (ScoopTime) Then
		ScoopTime = ScoopTime - 1
		if (drainTimer = 0) Then											'Flash before the ball shoots out
'			switch(ScoopTime) Then
			Select Case ScoopTime											'EP- Multiply the following case number by 12 to get their original numbers, and do some rounding too
				case 750:											'9000:
					skip = 0												'If you were waiting on something, the wait is over!
					playSFX 2, "S", "G", "V", 100
'					GIword OR= (1 << 4)										'EP- Commenting out all GIword since I cannot figure out what it does
					Light 42, 7
					light 43, 0
					light 44, 0
					light 45, 0
					light 46, 0
					light 47, 7				
				case 670:
'					GIword &= ~(1 << 4)
					Light 42, 0
				case 580:
					playSFX 2, "S", "G", "W", 100
'					GIword OR= (1 << 4)		
					Light 42, 7
					light 46, 7
				case 500:
'					GIword &= ~(1 << 4)
					Light 42, 0
				case 420:
					playSFX 2, "S", "G", "X", 100
'					GIword OR= (1 << 4)	
					Light 42, 7
					light 45, 7
				case 330:
'					GIword &= ~(1 << 4)
					Light 42, 0
				case 250:
					playSFX 2, "S", "G", "Y", 100
'					GIword OR= (1 << 4)
					Light 42, 7
					light 44, 7
				case 170:
'					GIword &= ~(1 << 4)
					Light 42, 0
				case 130:
					animatePF 240, 10, 0									'Scoop explode animation		
				case 80:
					playSFX 2, "S", "G", "Z", 100
'					GIword OR= (1 << 4)
					Light 42, 7
					light 43, 7
				case 1:
'					GIword &= ~(1 << 4)		
					Light 42, 0
					showScoopLights()										'Restore the lights
			End Select
		End If
		if (ScoopTime = 83) Then											'Scoop timer just about done? We check it at 1000 so the kick post can't retrigger the scoop
			if (hellMB = 1) Then
				hellMB = 10													'Set flag that music / mode has begun!
				volumeSFX 3, musicVolume(0), musicVolume(1)					'Back to normal
'				playMusic "M", "B"											'The multiball music!	
				musicplayer "bgout_MB.mp3"
				multipleBalls = 1											'When MB starts, you get ballSave amount of time to loose balls and get them back
				ballSave()													'That is, Ball Save only times out, it isn't disabled via the first ball lost							
			End If
			if (priProgress(player) = 9) Then
				priProgress(player) = 10	 								'Set mode as started
				modeTimer = 0				 								'Reset timer
			End If
			if (fortProgress(player) = 59) Then								'We don't blink these lights until player gets the ball, so they notice them more I guess?
				fortProgress(player) = 60
				blink(17)													'Blink the targets for the Soldier.
				blink(18)
				blink(19)			
			End If
'			Coil(ScoopKick, scoopPower)										'Kick out ball
			KiVUK1.Enabled = 0
			MoveBall BallMover2, KiVUK1, KiVUK3, 205, 25, 0
			Playsound SoundFX("ballrelease")
			DOF 122, 2
			KiVUK1.TimerEnabled = 1											'Start a timer that will re-enabled the VUK very shortly after the kick-out
			Sw22 = 0
			if (Tunnel) Then
				Tunnel = 0													'Clear flag, if set
			End If
			if (spiritGuide(player) > 99 and spiritGuide(player) < 200) Then			'Are we supposed to award Spirit Guide thing?
				spiritGuideAward()
			End If
			ballSaveScoop()													'Grace period in case ball goes down drain
		End If
	End If
	if (MagnetTimer) Then													'Ghost Magnet enabled?
		MagnetCount = MagnetCount + 1										'Increment PWM timer
		if (MagnetCount > 0) Then											'End of cycle?
			MagnetCount = 0				 									'Reset PWM counter
			MagnetTimer = MagnetTimer - 1									'Decrement main timer.
'			if (SolTimer(Magnet) = 0) Then									'Magnet not on?
			if (magFlag) Then
'**				centercounter = centercounter + 1
				mMagnaSave.MagnetOn = 1
'**				If centercounter >= 5 Then	mMagnaSave.GrabCenter = 0
'**			elseIf (magFlag < 1) AND (mMagnaSave.MagnetOn = 1) Then
			else
				mMagnaSave.MagnetOn = 0										'EP- my implementation of the magnet
'**				centercounter = 0
			End If
		End If
		if (fortProgress(player) = 99) Then									'Ending War mode?
			if (MagnetTimer < 2) Then										'Just about done?
				WarOver()													'Finish the mode
			End If			
		End If
		if (theProgress(player) = 50) Then									'Ending theater mode?
			if (MagnetTimer < 2) Then										'Just about done?
				TheaterOver()												'Finish the mode
			End If
		End If
		if (MagnetTimer = 10) Then
			if (minion(player) = 11) Then
				magFlag = 0													'Clear the flag so magnet is no longer pulsed (but timing stays the same)							
				minionEnd(1)												'End the mode, with flag to advance Minion Hits				
			End If						
		End If
		if (MagnetTimer = 1) Then											'Just about done?
			if (minion(player) <> 11) Then
				magFlag = 0													'Clear the flag so magnet is no longer pulsed (but timing stays the same)
'				swDebounce(24) = 5000										'Manually enable the ghost switch debounce so if we hit them with our job, won't reactivate				
			End If
		End If
	End If
	if (orbTimer) Then
		orbTimer = orbTimer - 1
	End If
	if (LeftTimer) Then
		LeftTimer = LeftTimer - 1
		if (drainTimer = 0) Then
			if (LeftTimer = Int(6000/CycleAdjuster)) Then
				light 40, 7
			End If
			if (LeftTimer = Int(5000/CycleAdjuster)) Then
				light 40, 0
			End If		
			if (LeftTimer = Int(4000/CycleAdjuster)) Then
				light 40, 7
			End If
			if (LeftTimer = Int(3000/CycleAdjuster)) Then
				light 40, 0
			End If	
			if (LeftTimer = Int(2000/CycleAdjuster)) Then
				light 40, 7
			End If
			if (LeftTimer = Int(1000/CycleAdjuster)) Then
				light 40, 0
			End If	
		End If
		if (LeftTimer = Int(1000/CycleAdjuster)) Then
'			switchDebounce(23)												'Set the debounce just in case
'			Coil(LeftVUK, vukPower)
			VUKKicker KiDoor, vukPower										'EP- My version of the VUK kicker
			skip = 0
			if (hosProgress(player) = 90) Then								'Just started Doctor Ghost Battle?
				hosProgress(player) = 10									'Ball is kicked, officially started!
			End If
		End If
	End If
	if (rampTimer) Then
		rampTimer = rampTimer - 1
	End If
	if (centerTimer) Then
		centerTimer = centerTimer - 1
	End If
	if (popsTimer) Then
		popsTimer = popsTimer - 1
	End If
	if (TargetDelay) Then													'Target set to move after a delay?
		TargetDelay = TargetDelay - 1										'Decrement
		if (loopCatch = checkBall) Then										'Trying to catch the ball?		
			if (TargetDelay = 25) Then										'Almost ready to check?
				MagnetSet(100)												'Pulse magnet again
			End If
			if (TargetDelay < 1) Then										'Timed out? Ball must not be there. Bummer.
				magFlag = 0													'Clear the pulse flag
				TargetTimerSet 1, TargetDown, 2								'Keep targets down so you can re-trap
				loopCatch = catchBall										'Reset state, we still need to catch the ball	
				killQ()														'Disable any Enqueued videos	
				video "D", "Z", "A", allowSmall, 20, 255 					'Speed Demon Bonus!
				showValue 100000, 40, 1 									'It's a combo value * Ghosts defeated because why not?	
				playSFX 2, "D", "Z", "X", 255								'Vrooom! Just like a Mustang!				
			End If			
			if (TargetDelay < 25 and Sw24 = 1) Then							'After second pulse, we consider a ball in opto to be a good catch
				MagnetSet(100)												'Pulse it again to make sure it stays there while targets are going up
				TargetDelay = 0												'Clear this just in case
				TargetSpeed = TargetNewSpeed								'Allow targets to move up	
				TargetSet targetTarget
'				cabDebounce(ghostOpto) = 10000								'Make sure it doesn't re-trigger opto				
				loopCatch = ballCaught										'External logic will take it from here. Allow targets to go up
			End If			
		Else
			if (TargetDelay < 1) Then										'Ready to move targets?
				TargetSpeed = TargetNewSpeed								'Set Speed flag to start targets moving	
				TargetSet targetTarget
				TargetDelay = 0												'Clear this just in case
			End If			
		End If
	End If
	if (loadChecker) Then													'Did we just try to load a ball?
		loadChecker = loadChecker - 1
		if (loadChecker = 1) Then											'Timer just about done?
			if (Sw57 = 0) Then										'bitRead(switches(7), 1) = 0) Then							'Did ball not load into shooter lane properly?				
				'Serial.println("LOAD FAIL, RE-TRYING...")
				loadBall()													'Try and re-load it.
			End If
		End If		
	End If
	if (saveTimer > 0 and popsTimer = 0 and kickTimer = 0) Then				'Save timer doesn't decrement during pops action or when a ball is being kicked from drain
		saveTimer = saveTimer - 1
		if (saveTimer = cycleSecond2 or saveTimer = (cycleSecond2 - 10) or saveTimer < 10) Then		'Light turns off about TWO seconds before it's actually done. Double check near very end as well
			spookCheck()													'Check what to do with the light
		End If
	End If
	if (modeTimer) Then
		modeAction()
	End If
	if (displayTimer) Then
		displayTimer = displayTimer - 1										'Decrement timer
		if (displayTimer > 0 and displayTimer < Int(45000/CycleAdjuster)) Then				'Flashing ORB win?		
			if (displayTimer = 1) Then										'Just about done?
				'orb(player) = 0											'Clear player's ORB variable so it can be reset
				checkOrb(0)
				displayTimer = 0
			End If
		End If
		if (displayTimer > Int(45000/CycleAdjuster) and displayTimer < Int(90000/CycleAdjuster)) Then				'Flashing GLIR spelling?		
			if (displayTimer = Int(45001/CycleAdjuster)+1) Then				'Just about done?
				checkRoll()													'See what status the lights should be and set them to that (just in case they changed during the blinking)
				displayTimer = 0
			End If
		End If			
	End If
	if (ghostFadeTimer) Then
		ghostFadeTimer = ghostFadeTimer - GhostFadeSpeed
		if (ghostFadeTimer <= 1) Then										'Just about done?
			Dim xRed, xGreen, xBlue
'**			ghostFadeTimer = ghostFadeAmount								'Reset timer
'**			If FlGhostB.alpha > Int(ghostModeRGB(2)/2) Then FlGhostB.Alpha = FlGhostB.Alpha - gFadeSpeed
'**			If FlGhostB.alpha < Int(ghostModeRGB(2)/2) Then FlGhostB.Alpha = FlGhostB.Alpha + gFadeSpeed
'**			If FlGhostR.alpha > Int(ghostModeRGB(0)/2) Then FlGhostR.Alpha = FlGhostR.Alpha - gFadeSpeed
'**			If FlGhostR.alpha < Int(ghostModeRGB(0)/2) Then FlGhostR.Alpha = FlGhostR.Alpha + gFadeSpeed
'**			If FlGhostG.alpha > Int(ghostModeRGB(1)/2) Then FlGhostG.Alpha = FlGhostG.Alpha - gFadeSpeed
'**			If FlGhostG.alpha < Int(ghostModeRGB(1)/2) Then FlGhostG.Alpha = FlGhostG.Alpha + gFadeSpeed
'**			If FlGhostG.alpha = Int(ghostModeRGB(1)/2) AND FlGhostR.alpha = Int(ghostModeRGB(0)/2) AND FlGhostB.alpha = Int(ghostModeRGB(2)/2) Then
'**				If GhostModeRGB(0) = 0 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost0"
'**				If GhostModeRGB(0) = 0 AND GhostModeRGB(1) = 255 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost1"
'**				If GhostModeRGB(0) = 128 AND GhostModeRGB(1) = 128 AND GhostModeRGB(2) = 128 Then PrGhost.Image = "Ghost2"
'**				If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 131 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost3"
'**				If GhostModeRGB(0) = 0 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 255 Then PrGhost.Image = "Ghost4"
'**				If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 255 Then PrGhost.Image = "Ghost5"
'**				If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost6"
'**				If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 255 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost7"
'**				If GhostModeRGB(0) = 200 AND GhostModeRGB(1) = 140 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost8"
'**				GhostFadeTimer = 0
'**			End If
			xRed =   Bi2Dec(Right(Dec2Bi(GiLight2.Color), 8))
			xGreen = Bi2Dec(Mid(Dec2Bi(GiLight2.Color), 9, 8))
			xBlue =  Bi2Dec(Left(Dec2Bi(GiLight2.Color), 8))
			If xRed > ghostModeRGB(0) Then xRed = xRed - gFadeSpeed
			If xRed < ghostModeRGB(0) Then xRed = xRed + gFadeSpeed
			If xGreen > ghostModeRGB(1) Then xGreen = xGreen - gFadeSpeed
			If xGreen < ghostModeRGB(1) Then xGreen = xGreen + gFadeSpeed
			If xBlue > ghostModeRGB(2) Then xBlue = xBlue - gFadeSpeed
			If xBlue < ghostModeRGB(2) Then xBlue = xBlue + gFadeSpeed
			For each bulb in Ghost_RGB
				bulb.color = RGB(xRed, xGreen, xBlue)
			Next
			if (xRed = ghostModeRGB(0)) AND (xGreen = ghostModeRGB(1)) AND (xBlue = ghostModeRGB(2)) Then
				ghostFadeTimer = 0
				For each bulb in Ghost_RGB
					bulb.color = RGB(ghostModeRGB(0), ghostModeRGB(1), ghostModeRGB(2))
				Next
			End If
		End If	
	End If
	if (RGBtimer) Then														'Changing the cabinet lighting?
		Dim Bulb, tRed, tGreen, tBlue, lRed, lGreen, lBlue, RGBFlag
		RGBFlag = 0
		RGBtimer = RGBtimer - 1
		if (RGBtimer < 1) Then												'Time to change?
			RGBtimer = RGBspeed												'Reset timer
'**			Dim X
'**			Dim RGBflag														'Set flag to 0. This checks if all 3 have reached their target (since some might be closer to others when starting)
'**			RGBflag = 0
'**			for x = 0 To 2													'Make current colors match the target
'**				if (left_RGB(x).alpha > Int(targetRGB(x) / 10)) Then
'**					left_RGB(x).alpha = left_rgb(x).alpha - 1
'**				End If
'**				if (left_rgb(x).alpha < Int(targetRGB(x) / 10)) Then
'**					left_rgb(x).alpha = left_rgb(x).alpha + 1
'**				End If
'**				if (left_rgb(x).alpha = int(targetRGB(x) / 10)) Then
'**					RGBflag = RGBflag + 1									'Did we reach it? Increase flag counter
'**				End If
'**				Left_Lights(x).alpha = Left_RGB(x).alpha * 10
'**				Right_Lights(x).alpha = Left_Lights(x).alpha
'**				right_rgb(x).alpha = left_rgb(x).alpha									'Make both sides the same color
'**			Next
			lRed = Bi2Dec(Right(Dec2Bi(GiLight9.Color), 8))					'-EP take one light's color, change it to a binary string, look at the 8 bits on the right, and convert that into a decimal
			lGreen = Bi2Dec(Mid(Dec2Bi(GiLight9.Color), 9, 8))				'-EP Do the same but look at the middle 8 bits
			lBlue = Bi2Dec(Left(Dec2Bi(GiLight9.Color), 8))					'-EP Do the same but look at the left 8 bits
			tRed = Bi2Dec(Right(Dec2Bi(RGBTarget), 8))						'-EP and again, but looking at the target RGB color
			tGreen = Bi2Dec(Mid(Dec2Bi(RGBTarget), 9, 8))
			tBlue = Bi2Dec(Left(Dec2Bi(RGBTarget), 8))
			If lRed = tRed Then 
				RGBFlag = RGBFlag + 1
			ElseIf lRed > tRed Then											'-EP if the red bits haven't reached their target, then decrease their amount
				lRed = lRed - rgbfadeamount
				If lRed <= tRed Then
					lRed = tRed
					RGBFlag = RGBFlag + 1
				End If
			ElseIf lRed < tRed Then											'-EP rgbfadeamount is defined at the top of this Sub
				lRed = lRed + rgbfadeamount
				If lRed >= tRed Then
					lRed = tRed
					RGBFlag = RGBFlag + 1
				End If
			End If
			If lGreen = tGreen Then
				RGBFlag = RGBFlag + 1
			ElseIf lGreen > tGreen Then
				lGreen = lGreen - rgbfadeamount
				If lGreen <= tGreen Then
					lGreen = tGreen
					RGBFlag = RGBFlag + 1
				End If
			ElseIf lGreen < tGreen Then
				lGreen = lGreen + rgbfadeamount
				If lGreen >= tGreen Then
					lGreen = tGreen
					RGBFlag = RGBFlag + 1
				End If
			End If
			If lBlue = tBlue Then
				RGBFlag = RGBFlag + 1
			ElseIf lBlue > tBlue Then
				lBlue = lBlue - rgbfadeamount
				If lBlue <= tBlue Then
					lBlue = tBlue
					RGBFlag = RGBFlag + 1
				End If
			ElseIf lBlue < tBlue Then
				lBlue = lBlue + rgbfadeamount
				If lBlue >= tBlue Then
					lBlue = tBlue
					RGBFlag = RGBFlag + 1
				End If
			End If
			For Each bulb in Right_RGB
				bulb.Color = RGB(lRed, lGreen, lBlue)
			next
			For Each bulb in Left_RGB
				bulb.Color = RGB(lRed, lGreen, lBlue)
			next
			if (RGBflag = 3) Then											'If all 3 reached target, we're done!
				RGBtimer = 0												'Clear timer to finish mode
				'BackGlass Stuff
				Dim BGColorL
				If targetRGB(0) > 0 AND targetRGB(1) = 0 AND targetRGB(2) > 0 Then BGColorL = "purple"
				If targetRGB(0) > 0 AND targetRGB(1) = 0 AND targetRGB(2) = 0 Then BGColorL = "red"
				If targetRGB(0) > 0 AND targetRGB(1) > 0 AND targetRGB(2) > 0 Then BGColorL = "white"
				If targetRGB(0) = 0 AND targetRGB(1) > 0 AND targetRGB(2) = 0 Then BGColorL = "green"
				If targetRGB(0) = 0 AND targetRGB(1) = 0 AND targetRGB(2) > 0 Then BGColorL = "blue"
				backglassoff(2)
				SetBackGlass 1, BGColorL
				SetBackGlass 2, BGColorL
			End If
			doRGB()
		End If
	End If
	if (comboTimer) Then
		comboTimer = comboTimer - 1
		if (comboTimer = 0) Then											'Time's up for Combo?
			if (tourLights(comboShot) = 0) Then								'If camera icon isn't still being used for a Tour shot...
				light photoLights(comboShot), 0								'Turn off the Combo Shot Lamp			
			End If
			if (theProgress(player) = 100) Then								'CASE 3: Theater has been completed? Reset Sweet Jumps score counter
				sweetJumpBonus = 0											'Reset score (hitting it adds value)
				sweetJump = 0												'Reset video/SFX counter
			End If			
			comboCount = 1													'Reset the count for next time
			comboVideoFlag = 0												'Clear flag
		End If
	End If
	if (skillShot > 0 and run = 3) Then										'This timer lets us sense a half-ass skill shot attempt
		modeTimer = modeTimer + 1
		if (modeTimer > Int(100000/CycleAdjuster)) Then						'Keep it from getting too high, like Towlie
			modeTimer = Int(10000/CycleAdjuster)
		End If
	End If
	if (dirtyPoolTimer) Then
		dirtyPoolLogic()
	End If
	if (restartTimer) Then
		restartTimer = restartTimer - 1
		if (restartTimer = 1) Then
			restartSeconds = restartSeconds - 1
			restartTimer = longSecond
			if (restartSeconds = 0) Then									'Out of time?
				restartKill restartMode, 0									'Kill whatever we were trying to restart
			Else
'				numbers 0, numberStay | 4, 0, 0, restartSeconds - 1			'Update the Numbers Timer.
				numbers "", "", "", ""										'EP- Fairly certain this isn't right, but I'll see what it does
			End If
		End If
	End If
	if (lightningTimer) Then
		lightningTimer = lightningTimer + 1
		lightningFX(lightningTimer)	
	End If
	if (HellSafe) Then
		HellSafe = HellSafe - 1												'Decrement it
		if (HellSafe = 1 and HellBall = 10) Then							'Ball didn't hit the middle Subway switch in time?	
			'Serial.println("BALL MISSING RE-TRYING")			
			hellCheck = 20													'Set state, which means Go Back to Up, then come back down
			ElevatorSet hellUp, 100											'Send hellavator up
			'Set timer to Cycles it will take for hell to go back up + Cycles it'll take to come back down
			HellSafe = ((hellUp - hellDown) * 100) + ((hellUp - hellDown) * 200) + subwayTime
			Kihellevator.Kick 205, 10, 70
		End If
	End If
	if (lightStatus) Then
		animationTimer = animationTimer + 1
		if (animationTimer > animationTarget) Then
			animationTimer = 0			
			lightCurrent = lightCurrent + 1
			if (lightCurrent > lightEnd) Then
				if (lightStatus) AND (lightLoop) Then
					lightCurrent = lightStart
				Else
					if (scoringTimer) Then									'If Psychic scoring is active, we interrupted the animation
						animatePF 119, 30, 1								'Restart looping Psychic animation
					Else
						lightStatus = 0
					End If				
				End If							
			End If
		End If
	End If
	if (scoringTimer) Then
		scoringTimer = scoringTimer - 1
		if (scoringTimer = Int(50000/CycleAdjuster)) Then					'Just about done?
';			video "S", "P", "J", allowSmall, 0, 200							'Video for it
			playSFX 2, "S", "1", "Z", 200									'Replace with DOUBLE SCORING hurry-up prompt
		End If
		if (scoringTimer = 1) Then											'Done?
			scoreMultiplier = 1												'Multiplier done
			scoringTimer = 0												'Reset timer
';			video "S", "P", "K", allowSmall, 0, 200							'Psychic Scoring OVER!
			playSFX 2, "S", "2", "Z", 200									'Double Scoring Over prompt
			animatePF 0, 0, 0												'Kill animations
			light 51, 7														'Light Psychic solid (done)
		End If
	End If
End Sub

Sub Tilt()
	
End Sub

'FUNCTIONS FOR THEATER MODE 2............................
sub TheaterAdvance()							'Logic to advance Theater Mode 2
	Dim X
	AddScore(advanceScore)
	flashCab 0, 255, 0, 100					'Flash the GHOST BOSS color
	theProgress(player)  = theProgress(player)  +  1
	areaProgress(player)  = areaProgress(player)  +  1
	if (theProgress(player) < 3) Then										'First 3 advances?
		video "T", 48 + theProgress(player), "A", allowSmall, 90, 255			'Play first 3 videos, based off how far we are
		playSFX 0, "T", theProgress(player) + 48, random(4) + 65, 255
		for x=0 To theProgress(player)-1
			light 36 + x, 7											'Light all progress, in case we Double Advance
		Next		
		pulse(theProgress(player) + 36)								'Pulse the next one
	End If
	'MAKE SURE THIS DOESN'T COLLIDE WITH DOCTOR GHOST MODE START, BECAUSE RIGHT NOW IT COULD
	if (theProgress(player) = 3) Then									'Prompt shot for Mode Start.
		video "T", "3", "A", allowSmall, 90, 255						'Play first 3 videos, based off how far we are
		playSFX 0, "T", "3", random(4) + 65, 255
		light 36, 7												'Manually set them solid
		light 37, 7
		light 38, 7
		pulse(12)													'Blink light 12 for Theater Start	
		DoorSet DoorOpen, 250										'Open the door.
		if (hosProgress(player) = 3) Then								'Had doctor ready?
			light 11, 0											'Gonna have to wait!	
		End If
	End If
End Sub

sub TheaterStart()								'What happens when we shoot "Theater Ghost" when lit
	restartKill 2, 1							'In case we got the Restart
	comboKill()
	storeLamp(player)							'Store the state of the Player's lamps
	allLamp(0)									'Turn off the lamps
	spiritGuideEnable(0)						'No spirit guide during Theater
	hellEnable(0)								'Can't do multiball in this mode
	modeTotal = 0								'Reset mode points	
	AddScore(startScore)
	popLogic(3)								'Set pops to EVP
	minionEnd(0)								'Disable Minion mode, even if it's in progress
	setGhostModeRGB 255, 255, 0				'Yellow Ghost
	setCabModeFade 0, 255, 0, 100				'Green cab
	jackpotMultiplier = 1						'Reset this just in case
	if (countGhosts() = 5) Then						'Is this the last Boss Ghost to beat?
		blink(48)									'Blink that progress light
	End If	
	pulse(17)									'Pulse Ghost Targets (they add extra time)
	pulse(18)
	pulse(19)
	pulse(39)									'Jump light
	ElevatorSet hellUp, 300					'Make sure elevator is UP
	blink(41)
	theProgress(player) = 10					'Set flag for the mode being started
	Mode(player) = 2							'Set mode to 2
	Advance_Enable = 0							'Mode started, disable advancement until we are done
	TargetTimerSet 10, TargetUp, 50			'Just in case
	if (minion(player) = 10) Then					'In a minion battle?
		minionEnd(0)							'End mode, with flag to NOT re-enable it
	End If	
	DoorSet DoorClosed, 1						'Shut door fast!
	light 12, 0								'Turn off THEATER start mode light
	blink(58)									'Blink the Theater mode light.
	'VOICE CALL, GHOST APPEARS
	killQ()									'Disable any Enqueued videos
	video "T", "4", "A", allowSmall, 170, 255			'Ghost reveal!
	playSFX 0, "T", "5", 65 + random(4), 255		'Mode start dialog	
'	playMusic 'B', '1'						'Boss battle music!	
	musicplayer "bgout_B1.mp3"
	sweetJumpBonus = 0							'Starts at ZERO. Increases with each SWEET JUMP. Resets if you hit the Ghost Targets for more time
	sweetJump = 0
	modeTimer = Int(130000/CycleAdjuster)		'Set high so timer doesn't start for an extra second
	countSeconds = TheaterTime					'How many seconds to get to hit the shot
';	numbers 0, numberStay OR 4, 0, 0, countSeconds - 1		'Update the Numbers Timer. We do "-1" so it'll display a zero.
	shotValue = (10000 * countSeconds) + 500000			'Starting shot value 
	customScore "T", "P", "A", allowAll OR loopVideo, 120		'Shoot Ghost custom score prompt
';	numbers 8, numberScore OR 2, 0, 0, player				'Show player's score in upper left corner
';	numbers 9, 2, 70, 27, shotValue						'Shot Value
';	numbers 10, 9, 88, 0, 0								'Ball # upper right	
	strobe 26, 6								'Strobe HOTEL LIGHTS - path 1
	strobe 36, 4								'Strobe JUMP SHOT - combo on camera appearing or going away won't affect it
	KickLeft 106000, vukPower						'Kick back ball
	'KickLeft 90000, vukPower						'Kick back ball
	showProgress 1, player					'Show the Main Progress lights
	videoModeCheck()
	skip = 20									'Set theater skip mode 1
End Sub

sub TheaterPlay(yesNo)							'What happens when you shoot the strobing paths
	if (yesNo = 0) Then										'Sent with with a flag that this ISN'T the shot we want?
		playSFX 0, "T", "8", 65 + random(8), 255			'Ghost gives you shit
		TheaterStrobe()									'Make sure correct shot is LIT
		if (theProgress(player) = 13) Then					'Should we prompt to SHOOT GHOST?
			video "T", "4", "E", allowSmall, 80, 255		'Ghost upset, prompt to SHOOT GHOST TO FINISH	
		Else
			video "T", "4", "C", allowSmall, 80, 255		'Ghost upset, prompt to SHOOT NEXT STROBING	
		End If
		Exit Sub
	End If
	AddScore((10000 * countSeconds) + 500000)		'10k per second left + 250k per correct shot
	countSeconds = TheaterTime						'Reset timer
	shotValue = (10000 * countSeconds) + 500000			'Reset shot value 
';	numbers 0, numberStay OR 4, 0, 0, countSeconds - 1		'Update timer
';	numbers 9, 2, 70, 27, shotValue						'Update shot value
	if (theProgress(player) = 10) Then				'First shot?
		modeTimer = Int(120000/CycleAdjuster)		'Set high so timer doesn't start for an extra second
		ElevatorSet hellDown, 550					'Move elevator down
		light 41, 0
		killQ()									'Disable any Enqueued videos	
		video "T", "7", "A", allowSmall, 182, 255			'Scene 1
		playSFX 0, "T", "7", "A", 255				'Play dialog 1		
		light 26, 0								'Turn OFF the HOTEL STROBE
		strobe 8, 7								'Strobe the SPOOKY DOOR shot!
		customScore "T", "P", "B", allowAll OR loopVideo, 120		'Shoot DOOR custom score prompt
		theProgress(player) = 11					'Advance this
		DoorSet DoorOpen, 500						'Open door for next shot
		skip = 21									'Second skip event
		Exit Sub
	End If
	if (theProgress(player) = 11) Then				'Second shot?
		modeTimer = Int(100000/CycleAdjuster)			'Set high so timer doesn't start for an extra second
		killQ()									'Disable any Enqueued videos	
		video "T", "7", "B", allowSmall, 148, 255			'Scene 1
		playSFX 0, "T", "7", "B", 255				'Play dialog 1		
		light 8, 0								'Turn OFF the SPOOKY DOOR STROBE
		strobe 43, 5								'Strobe BASEMENT SCOOP path
		customScore "T", "P", "C", allowAll OR loopVideo, 120		'Shoot SCOOP custom score prompt
		theProgress(player) = 12					'Advance this
		KickLeft 76000, vukPower					'Kick it out slowly
		DoorSet DoorClosed, 500					'Close door for next shot
		skip = 22									'Third skip event
		Exit Sub
	End If
	if (theProgress(player) = 12) Then				'Third shot?
		modeTimer = Int(80000/cycleAdjuster)			'Set high so timer doesn't start for an extra second
		killQ()									'Disable any Enqueued videos	
		video "T", "7", "C", allowSmall, 133, 255			'Scene 1
		playSFX 0, "T", "7", "C", 255				'Play dialog 1
		light 43, 0								'Turn OFF the SPOOKY DOOR STROBE
		blink(16)									'Blink the GHOST TARGET lights
		blink(17)
		blink(18)
		blink(19)
		customScore "T", "P", "D", allowAll OR loopVideo, 120		'Shoot Ghost custom score prompt
		TargetTimerSet 5000, TargetDown, 400		'Put down the Ghost Targets very slowly		
		theProgress(player) = 13					'Advance this
		ScoopTime = Int(62500/CycleAdjuster)		'Kick out scoop, at a slower rate
		skip = 23									'Fourth skip event
		Exit Sub
	End If
End Sub

sub TheaterStrobe()								'Reset whatever light SHOULD be strobing
	strobe 36, 4								'Strobe JUMP SHOT - combo on camera appearing or going away won't affect it
	Select Case theProgress(player)
		case 10:
			strobe 26, 6								'Strobe HOTEL LIGHTS - path 1
		case 11:
			strobe 8, 7
		case 12:
			strobe 43, 5
		case 13:
			blink(16)									'Blink the GHOST TARGET lights
			blink(17)
			blink(18)
			blink(19)
	End Select
End Sub

sub TheaterWin()
	DOF 134, 2
	loadLamp(player)												'Restore what the lamps were doing before this mode started
	comboKill()
	spiritGuideEnable(1)
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(800/CycleAdjuster)
	ghostFadeAmount = Int(200/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	MagnetSet(1200)												'Catch ball and hold for remaining lines
	light 58, 7													'Set THEATER LIGHT solid!
	modeTimer = 0													'Disable timer
	killTimer(0)													'Turn off numbers	
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	light 36, 0
	light 37, 0
	light 38, 0
	light 39, 0
	light 12, 0													'Theater Mode Solid!
	AddScore(winScore)
	playSFX 0, "T", "7", "D", 255									'Mode win dialog
	killQ()														'Disable any Enqueued videos	
	video "T", "7", "D", noExitFlush, 215, 255						'Mode won, prevent numbers
';	numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233				'Load Mode Total Points as Number	
	modeTotal = 0													'Reset mode points		
	videoQ "T", "7", "E", noEntryFlush OR 3, 45, 233		'Mode Total Video	
	sweetJumpBonus = 0					'Reset score (hitting it adds value)
	sweetJump = 0						'Reset video/SFX counter	
'	playMusic 'M', '2'											'Normal music
	musicplayer "bgout_M2.mp3"
	theProgress(player) = 50										'Sets a flag so when MAGNET finishes and releases the ball, it will totally finish the mode
End Sub

sub TheaterOver()
	Mode(player) = 0												'Set mode active to None
	ModeWon(player) = ModeWon(player) OR 4							'Set THEATER WON bit for this player.
	if (countGhosts() = 6) Then										'This the final Ghost Boss? Light BOSSES solid!
		light 48, 7
	End If
	ghostsDefeated(player)  = ghostsDefeated(player)  +  1									'For bonuses
	Advance_Enable = 1												'Allow other modes to be started
	hellEnable(1)
	theProgress(player) = 100										'Mode done and can't be restarted	
	if (countGhosts() = 2 or countGhosts() = 5) Then	'Defeating 2 or 5 ghosts lights EXTRA BALL
		extraBallLight(2)							'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random 2), 255	'"Extra Ball is Lit!"			
	End If	
	if (videoMode(player) > 0) Then									'Video mode is available?
		videoModeLite()											'Enable it, leave targets down
	Else
		TargetTimerSet 10000, TargetUp, 100
		minionEnd(2)												'Re-enable Minion find but do NOT let it control targets since this mode needs to do that		
	End If
	demonQualify()													'See if Demon Mode is ready
	'checkModePost()							'Doing this manually so we can skip the Target Logic (we're handling that!)	
	doorLogic()								'Figure out what to do with the door
	checkRoll()								'See if we enabled GLIR Ghost Photo Hunt during that mode
	elevatorLogic()							'Did the mode move the elevator? Re-enable it and lock lights
	'targetLogic(1)								'Where the Ghost Targets should be, up or down
	popLogic(0)								'Figure out what mode the Pops should be in
	showProgress 0, player
End Sub

sub TheaterFail( reasonFail)
	loadLamp(player)												'Restore what the lamps were doing before this mode started
	comboKill()
	spiritGuideEnable(1)
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(100/CycleAdjuster)
	ghostFadeAmount = Int(100/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	sweetJumpBonus = 0												'Reset score (hitting it adds value)
	sweetJump = 0													'Reset video/SFX counter
	modeTimer = 0													'Disable timer
	killTimer(0)													'Turn off numbers	
	if ((ModeWon(player) AND 4)=4) Then								'Did we win this mode before?
		light 58, 7												'Make Theater Mode light solid, since it HAS been won
	Else
		light 58, 0												'Haven't won it yet, turn it off
	End If
	if (reasonFail = 0) Then											'Fail via drain we pass a 1, and thus, don't do the video or speech
		modeTotal = 0												'Reset mode points				
		Mode(player) = 0												'Set mode active to None
		Advance_Enable = 1												'Allow other modes to be started
		checkModePost()
		hellEnable(1)		
		if ((modeRestart(player) AND 4)=4) Then							'Able to restart theater?
			modeRestart(player) = modeRestart(player) AND 251							'Clear the restart bit
			playSFX 0, "T", "9", 65 + random(4), 255						'A-D fail quotes
			video "T", "4", "H", allowSmall, 105, 255					'Mode Fail, Shoot door to Restart		
			DoorSet DoorOpen, 100										'Make sure door is open			
			restartBegin 2, 11, 25000									'Enable a restart!  Mode 2, for 5 seconds, starting timer value
			theProgress(player) = 3									'Allows you to re-start the mode
			showProgress 0, player									'Re-load other stuff
			light 9, 0		
			light 10, 0
			light 11, 0
			strobe 8, 4												'Strobe lights under door
'			playMusic 'H', '2'										'Hurry Up Music!
			musicplayer "bgout_H2.mp3"
		Else
			light 12, 0												'Turn off Theater Ghost light
			playSFX 0, "T", "9", 65 + random(4), 255						'A-D fail quotes
			video "T", "4", "D", allowSmall, 111, 255					'Mode Fail, NO RESTART PROMPT
			theProgress(player) = 0									'Gotta start over
			pulse(36)													'Reset theater advance lights
			light 37, 0
			light 38, 0
			showProgress 0, player
'			playMusic 'M', '2'										'Normal music
			musicplayer "bgout_M2.mp3"
		End If
	Else
		modeTotal = 0												'Reset mode points				
		Mode(player) = 0											'Set mode active to None
		Advance_Enable = 1											'Allow other modes to be started
		if ((modeRestart(player) AND 4)=4) Then							'Able to restart theater?
			modeRestart(player) = modeRestart(player) AND 251							'Clear the restart bit
			theProgress(player) = 3									'Allows you to re-start the mode
		Else		
			theProgress(player) = 0
			pulse(36)													'Reset theater advance lights
			light 37, 0
			light 38, 0
		End If
		showProgress 0, player
		checkModePost() 											'Disable for testing
		hellEnable(1)		
	End If
				'Show the lights
End Sub
'END FUNCTIONS FOR THEATER MODE 2............................

Function tourGuide(whichBit,  whichMode,  whichLight, nullPoints,  nullSound)
	'Returns:
	'0 = You already got this one
	'1 = You completed this part of the tour!
	'10 = You completed this tour! (4 of 4)
	'99 = You completed ALL tours!
	if ((tourBits AND (shiftLeft(1, whichBit))) = (shiftLeft(1, whichBit))) Then				'Already hit this one?
		AddScore(nullPoints)						'A few points so shot logic doesn't have to worry about awarding anything
		if (nullSound) Then
			playSFX 2, "A", "Z", "Z", 255			'Generic shot WHOOSH sound		
		End If
		tourGuide = 0
		Exit Function									'Return a null
	End If
	'OK so we must not have hit this one yet, proceed:	
	light photoLights(whichLight), 0				'Turn out that light
	tourLights(whichLight) = 0						'Clear the tour lights for combo protection
	tourBits = tourBits OR shiftLeft(whichBit, 1)					'Set the bit
	tourTotal  = tourTotal  +  1									'Increase tour total
	if (tourTotal > 4) Then							'Just in case we forget to reset it for a mode
		tourTotal = 4
	End If
	'CHANGE TO A SOUND EFFECT!!!
	'playMusicOnce 'T', '0' + tourTotal									'Music for each advance
	if (whichMode = 8) Then												'Multiball?
		stopVideo(0)	
		video "C", "G", 65 + random(2), noExitFlush, 12, 255 			'Net catch left or right
		if (whichLight <> 2) Then										'Don't show number video on center shot since pops will override it
';			numbers 7, numberFlash OR 1, 255, 11, catchValue * 100500	'Value multiplies every time you clear all 4
			numbers "", "", catchValue*100500, catchValue*100500
			videoQ "C", "G", "C", noEntryFlush OR 3, 30, 255	'Mode Total:		
		End If
		AddScore(catchValue * 100500)									'And add it to the score	
		playSFX 0, "Q", "C", 65 + random(5), 250						'Sound + Heather compliment	
		if (tourTotal = 4) Then
			catchValue  = catchValue  +  1
			if (catchValue > 255) Then									'Could be possible. You never know.
				catchValue = 1
			End If														'Re-light the shots!
			tourReset(58)												'Tour: Left orbit, door VUK, up middle, right orbit (excludes Hotel and Scoop
		End If
	Else	
		if (tourTotal = 4) Then											'Completed this tour?
			tourComplete(player) = tourComplete(player) OR ShiftRight(whichMode, 1)		'Set flag that we completed the tour
			playSFX 1, "A", "X", "F", 255								'Tour complete sound
			if ((tourComplete(player) AND 126) = 126) Then
				video "R", "7", "A", 0, 45, 255 						'NEED A VIDEO FOR THIS!!!!!!!!!!				
				showValue 10000000, 40, 1								'10 MEEEEEELION!
				TourGuide = 99
				Exit Function											'Completed all tours!
			Else
				video "R", whichMode, 64 + tourTotal, 0, 45, 210 	'Show the correct video
				showValue 3000000, 40, 1								'Completing tour = 3 million
				TourGuide = 10
				Exit Function											'Completed this tours!
			End If		
		Else
			video "R", whichMode, 64 + tourTotal, 0, 45, 210 			'Show the correct video
			playSFX 1, "A", "X", "E", 255								'Tour advance sound
			showValue 500000 * tourTotal, 40, 1							'500k, 1 mil, 1.5 mil, then 3 million!
			TourGuide = 1
			Exit Function												'Return that we got 1
		End If		
	End If
End Function

sub tourReset(whichLights)												'Quickly sets the Tour Lights for a mode
	Dim X, bitChecker
	'photoLights() = Then7, 14, 23, 31, 39, 47End If
	bitChecker = 32
	for x=0 To 5
		if ((whichLights AND bitChecker) = bitChecker) Then
			blink(photoLights(x))
			tourLights(x) = 1											'Set that a Tour Light is here
		Else
			tourLights(x) = 0
		End If
		bitChecker = ShiftRight(bitChecker, 1)
	Next
	tourBits = 0
	tourTotal = 0
End Sub

sub tourClear()															'Gets rid of the Tour Lights  mode end or fail, tilt
	Dim X
	for x=0 To 5
		light photoLights(x), 0										'Turn off the light
		tourLights(x) = 0												'Clear the value so it won't interfere with combos / scoop light
	Next
	tourBits = 0
	tourTotal = 0	
End Sub

'FUNCTIONS FOR WAR FORT MODE 3............................
sub WarAdvance()
	AddScore(popScore)
	flashCab 0, 255, 0, 10					'Flash the GHOST BOSS color	
	areaProgress(player)  = areaProgress(player)  +  1	
	fortProgress(player)  = fortProgress(player)  +  1
	if (fortProgress(player) > 0 and fortProgress(player) < 26) Then ' and centerTimer = 0) Then					
		video "WA", "Z", fortprogress(player), allowBar OR allowSmall OR preventRestart, 40, 250				'Advance video	EP- Had to adjust since DMD doesn't have a progress bar
'		showProgressBar(4, 3, 12, 26, fortProgress player) * 4, 4
'		showProgressBar(5, 10, 12, 27, fortProgress player) * 4, 2	
	End If
	if (fortProgress(player) = 6) Then
		playSFX 0, "W", "1", random(4) + 65, 250 'Advance sound 1				
		Exit Sub
	End If
	if (fortProgress(player) = 18) Then
		playSFX 0, "W", "2", random(4) + 65, 250 'Advance sound 2					
		Exit Sub
	End If
	if (fortProgress(player) = 26) Then			'Did we fill the bar? Prompt for Mode Start!
		killQ()
		stopVideo(0)
		video "W", "0", "0", 0, 90, 255		'Prompt for Army Ghost Lit		
		playSFX 0, "W", "3", random(4) + 65, 250 'Prompt for Mode Start		
		'centerTimer = 25000					'Prevents pop bumper jackpot from overiding prompt video
		fortProgress(player) = 50				'50 indicates Mode is ready to start.				
		popLogic(3)							'EVP pops	
		spiritGuideEnable(0)		
		showScoopLights()						'Update the Scoop Lights
		'pulse(44)								'Pulse the ARMY GHOST start light			
		'light 43, 0							'Turn off PHOTO HUNT start. If eligible, it will light after mode over
		'light 46, 0							'Turn off SPIRIT GUIDE. If eligible, it will re-light during mode
		Exit Sub
	End If
	popToggle()	
	'playSFX(0, 'W', 'Z', random 10) + 65, 100	'Else, play the normal War Advance pop bumper sounds
	stereoSFX 1, "W", "Z", random(10) + 65, 100, leftVolume, rightVolume
End Sub

sub WarStart()
	Dim whichIntro, X
	light 44, 0								'Turn off blinking ARMY GHOST light before storing lamp state
	comboKill()	
	storeLamp(player)							'Store the state of the Player's lamps	
	allLamp(0)									'Turn off the lamps
	spiritGuideEnable(1)						'Spirit Guide available during mode. It will turn OFF until you start War Fort, turn ON after you make the shot to start War Fort
	modeTotal = 0								'Reset mode points		
	AddScore(startScore)							'One mil just for starting.
	comboKill()	
	minionEnd(0)								'Disable Minion mode, even if it's in progress
	TargetSet(TargetDown)						'Put them down so we'll notice them come UP
	setGhostModeRGB 255, 0, 255					'Magenta 
	setCabModeFade 0, 255, 0, 200				'cabinet color GREEN
	popLogic(3)								'Set pops to EVP
	tourReset(46)						'Tour: Left orbit, up middle, hotel path, right orbit (excludes Door and Scoop
												'Door is used for CONFEDERATE GOLD!
												'Scoop = Spirit Guide

	if (countGhosts() = 5) Then						'Is this the last Boss Ghost to beat?
		blink(48)									'Blink that progress light
	End If												
	light 44, 7								'Turn WAR FORT start light SOLID
	blink(59)									'Blink the Mode Light during battle.
	pulse(14)									'Pulse Door Camera (secret GOLD MODE!)
	Advance_Enable = 0							'Mode started, disable advancement until we are done
	modeTimer = 0								'We'll use this if player Goes for the Gold!
	goldHits = 0	
	goldTotal = 0								'Total Gold score
	Mode(player) = 4							'War Fort Mode officially started!
	gTargets(0) = 0							'Reset the 3 Ghost target status
	gTargets(1) = 0
	gTargets(2) = 0
	light 17, 0
	light 18, 0
	light 19, 0
	goldTimer = 0
	modeTimer = 0
	fortProgress(player) = 59					'Flag to BLINK the soldiers lights upon Scoop Kick. Then it switches to 60, mode begun!
	soldierUp = 7							'Set all soldiers to be up
	warHits = 0								'How many times we've hit the War ghost
	ghostLook = 1								'Allow ghost to look around again	
	'VOICE CALL, GHOST APPEARS
	whichIntro = random(3)
	If whichIntro = 0 Then X = 154
	If whichIntro = 1 Then X = 166
	If whichIntro = 2 Then X = 168
	playSFX 0, "W", "4", whichIntro + 65, 255	'Mode start dialog
	video "W", "0", whichIntro + 49, allowSmall, X, 255'Video that matches
'	playMusic 'B', '1'						'Boss battle music!
	musicplayer "bgout_B1.mp3"
	hellEnable(1)								'You can lock balls and get MB stacked on this mode
	doorLogic()								'See what the door should do
	TargetTimerSet 85000, TargetUp, 50		'Bring them up in about 8 seconds
	ScoopTime = Int(120000/CycleAdjuster)
	showProgress 1, player					'Show the progress, Active Mode style
	ghostAction = Int(320000/cycleAdjuster)
	videoModeCheck()	
	customScore "W", "A", 64 + soldierUp, allowAll OR loopVideo, 150		'Shoot score with targets in front
';	numbers 8, numberScore OR 2, 0, 0, player							'Show player's score in upper left corner
';	numbers 9, 9, 88, 0, 0											'Ball # upper right
	skip = 40
End Sub

sub WarFight()
	playSFX 0, "W", "6", 65 + random(3), 255	'Soldier hit noise + "Defense are down let's get this ghost!"
	modeTimer = 0								'Reset timer for exorcist quotes
	customScore "W", "C", "0", allowAll OR loopVideo, 100		'Shoot score with targets in front
';	numbers 8, numberScore OR 2, 0, 0, player							'Show player's score in upper left corner
';	numbers 9, 0, 0, 0, 0												'Cancel #9
	fortProgress(player) = 70					'Now we are fighting the ghost himself!
	pulse(16)									'Pulse the MAKE CONTACT light
	TargetSet(TargetDown)						'Put down the targets
'	playMusic 'G', 'S'						'Play annoying Ghost Squad theme!
	musicplayer "bgout_GS.mp3"
	jackpotMultiplier = 1						'Reset this just in case	
End Sub

sub WarLogic()
	Dim X
	if (ScoopTime = 0) Then							'Don't count while the ball is in the scoop
		if (fortProgress(player) = 60) Then			'Trying to knock down soldiers?		
			modeTimer  = modeTimer  +  1
			if (modeTimer = Int(120000/cycleAdjuster)) Then
				x = random(10)
				if (x < 5) Then
					playSFX 0, "W", "A", random(10), 200	'Random team leader prompts
				Else
					playSFX 2, "L", "G", random(8), 200		'Random lightning	
					lightningStart(Int(50000/CycleAdjuster))
				End If			
				modeTimer = 0
			End If		
		End If
		if (fortProgress(player) > 69 and fortProgress(player) < 100) Then			'Fighting the Army Ghost?		
			modeTimer  = modeTimer  +  1
			if (modeTimer = Int(120000/CycleAdjuster)) Then
				lightningStart(1)			'Do some lightning!
				x = random(10)
				if (x < 5) Then
					playSFX 0, "W", "B", random(10), 100	'Random team leader prompts
				Else
					playSFX 2, "L", "G", random(8), 100	'Random lightning	
					lightningStart(Int(50000/CycleAdjuster))
				End If				
			End If
			if (modeTimer = Int(150000/CycleAdjuster)) Then
				modeTimer = 0
			End If
		End If
	End If
End Sub

sub WarTrap()
	Dim whichBallWhack
	ghostLook = 0
	ghostBored = 0												'Prevents his look action from happening
	fortProgress(player)  = fortProgress(player)  +  1
	if (fortProgress(player) = 71) Then							'First hit where he throws it back?
		AddScore(EVP_Jackpot(player))
		light 19, 0											'Turn off Light 3 - his "health bar"
		whichBallWhack = random(4)							'Taunts 1-4
		playSFX 0, "W", "7", whichBallWhack + 65, 255			'Play SFX
		video "W", "7", whichBallWhack + 65, allowSmall, 68, 255		'Ghost hit, throws back ball
		customScore "W", "C", "1", allowAll OR loopVideo, 100		'Shoot score with targets in front
		'videoQ 'W', 'B', '1', allowSmall, 0, 200						'Ghost ready to fight!
		MagnetSet(300)											'Catch ball.			
		ghostFlash(50)
		ghostAction = Int(100000/CycleAdjuster)					'Set WHACK routine, turn back towards front			
	End If
	if (fortProgress(player) = 72) Then							'Second hit where he throws it back?
		AddScore(EVP_Jackpot(player))
		light 18, 0											'Turn off Light 2 - his "health bar"
		whichBallWhack = random(4)							'Taunts 5-8
		playSFX 0, "W", "7", whichBallWhack + 69, 255			'Play SFX
		video "W", "7", whichBallWhack + 69, allowSmall, 90, 255		'Ghost hit, throws back ball
		customScore "W", "C", "2", allowAll OR loopVideo, 100		'Shoot score with targets in front
		'videoQ 'W', 'B', '2', allowSmall, 0, 200
		MagnetSet(300)											'Catch ball.			
		ghostFlash(50)
		ghostAction = Int(100000/CycleAdjuster)					'Set WHACK routine, turn back towards front	
	End If
	if (fortProgress(player) = 73) Then									'Third hit?
		if (multiBall AND multiballHell) Then								'If MB active, give points but don't end mode until second ball is gone
			fortProgress(player) = 72									'Make this loop		
			AddScore(EVP_Jackpot(player))								'You can get a lot more jackpots this way!
			light 18, 0												'Turn off Light 2 - his "health bar"
			whichBallWhack = random(8)								'Taunts 5-8
			playSFX 0, "W", "7", whichBallWhack + 65, 255				'Play SFX
			video "W", "7", whichBallWhack + 65, allowSmall, 680, 255	'Ghost hit, throws back ball
			customScore "W", "C", "Z", allowAll OR loopVideo, 100			'Shoot score with targets in front
			sendJackpot(0)												'Update jackpot value
';			numbers 9, numberScore OR 2, 72, 27, 0						'Use Score #0 to display the Jackpot Value bottom off to right
			MagnetSet(400)												'Catch ball.			
			ghostFlash(50)
			ghostAction = Int(100000/CycleAdjuster)						'Set WHACK routine, turn back towards front
		Else															'If we aren't in a stacked MB, third hit wins mode
			MagnetSet(500)												'Catch ball.	
			WarWin()	
		End If
	End If			
End Sub

sub WarGoldStart()
	goldHits  = goldHits  +  1							'Increase # of Gold Hits
	if (goldHits = 1) Then
		video "W", "G", "A", allowSmall, 60, 255 			'2 HITS TO GO	
		playSFX 0, "W", "G", 65 + random(4), 255
		AddScore(50000)
'		DoorLocation = DoorClosed - 10				'Put it to be slightly opened
'		myservo(DoorServo).write(DoorLocation)		'Send that value to the servo
		PrDoor.ObjRotZ = DoorClosed + 10
		DoorSet DoorClosed, 1000					'Then make it go back closed
		ghostMove 10, 10							'Ghost looks at door.
		ghostBored = Int((5000 + random(15000))/cycleAdjuster)			'Set bored timer.
	End If
	if (goldHits = 2) Then
		video "W", "G", "B", allowSmall, 60, 255 			'1 HIT TO GO	
		playSFX 0, "W", "G", 69 + random(4), 255
		AddScore(50000)
'		DoorLocation = DoorClosed + 20				'Put it to be slightly opened
'		myservo(DoorServo).write(DoorLocation)		'Send that value to the servo
		PrDoor.ObjRotZ = DoorClosed + 20
		DoorSet DoorClosed, 1000					'Then make it go back closed
		ghostMove 10, 10							'Ghost looks at door.
		ghostBored = 5000 + random(15000)			'Set bored timer.		
	End If
	if (goldHits = 3) Then							'Gold Mode Start!
		AddScore(100000)
		goldHits = 10								'Set flag that we're collecting gold 
		video "W", "G", "C", allowSmall, 90, 255 	'Gold Mode Start!
		playSFX 0, "W", "G", 73 + random(4), 255	'Mode start dialog	
		DoorSet DoorOpen, 50						'Open the door		
		light 14, 0								'Turn off blinking camera
		strobe 8, 7								'Strobe the DOOR SHOT		
		countSeconds = GoldTime					'You've got 20 seconds to get a lot of gold!
		goldTotal = 0								'Keep track of score
		goldTimer = Int(30000/cycleAdjuster)		'Seconds countdown timer. Start a little higher to give player a chance
		ghostAction = Int(206000/CycleAdjuster)		'Guarding door
';		numbers 0, numberStay OR 4, 0, 0, countSeconds - 1		'Update display
	End If	
End Sub

sub WarGoldLogic()
		goldTimer  = goldTimer  -  1
		if (goldTimer = 1) Then								'Just about done?
			goldTimer = longSecond								'Reset Mode Timer
			countSeconds  = countSeconds  -  1								'Decrease seconds counter
			if (countSeconds) Then								'If Not Zero...
';				numbers 0, numberStay OR 4, 0, 0, countSeconds - 1		'Update display
				if (countSeconds > 1 and countSeconds < 7) Then	'Count down 5 4 3 2 1
					playSFX 2, "A", "M", 47 + countSeconds, 1
				Else
					playSFX 2, "Y", "Z", "Z", 1				'Beeps
				End If
			Else											'Time's up!
				WarGoldEnd()
			End If
		End If
End Sub

sub WarGoldEnd()
	DoorSet DoorClosed, 5						'Close the door
	killTimer(0)								'Turn off timer numbers
	light 8, 0								'Turn off strobe
	ghostAction = 0
	if (goldTotal) Then								'Did player collect some?
		video "W", "G", "G", allowSmall, 45, 255 	'Gold Mode Win!
		showValue goldTotal, 40, 0				'Flash the total points scored via Gold (don't add it to score since it has been already!
		playSFX 0, "W", "G", 85 + random(6), 255	'Mode end dialog		
		goldHits = 100								'Set flag so mode can't be re-started		
		light 14, 0								'Camera OFF we're done
	Else
		video "W", "G", "H", allowSmall, 60, 255 			'Gold Mode Fail!
		playSFX 0, "W", "G", 4 + random(4), 255		'Mode end dialog
		goldHits = 0								'Set flag so mode CAN be re-started!
		blink(14)									'Blink camera for re-start
	End If
	ghostAction = Int(319999/CycleAdjuster)
End Sub

sub WarWin()
	DOF 134, 2
	if (multiBall) Then							'Was a MB stacked?
		multiBallEnd(1)						'End it, with flag that it's ending along with a mode
	End If
	tourClear()	
	loadLamp(player)								'Load the original lamp state back in
	spiritGuideEnable(1)
	comboKill()
	killNumbers()							'Turn off numbers	
	if (goldHits < 100) Then
		goldHits = 0								'Enable Gold for next time if we didn't get it
	End If
	AddScore(winScore)
	ghostAction = Int(20000/CycleAdjuster)
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(200/cycleAdjuster)
	ghostFadeAmount = Int(200/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	light 59, 7							'War Fort solid = Mode Won!
	light 16, 0							'Turn off MAKE CONTACT light
	light 17, 0							'Clear the lights for a bit
	light 18, 0
	light 19, 0
	ghostLook = 1													'Ghost will now look around again.
	ghostAction = 0
	ghostMove 90, 50	
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()		
	killQ()													'Disable any Enqueued videos	
	video "W", "9", "A", noExitFlush, 87, 255 					'Play Death Video
';	numbersPriority 0, numberFlash OR 1, 255, 11, modeTotal, 233			'Load Mode Total Points	
	modeTotal = 0													'Reset mode points		
	videoQ "W", "9", "B", noEntryFlush OR 3, 45, 233	'Mode Total:	
	playSFX 0, "W", "8", random(4) + 65, 255	'Mode end dialog	
	fortProgress(player) = 99
'	playMusic 'M', '2'							'Normal music
	musicplayer "bgout_M2.mp3"
	if (videoMode(player) = 0) Then									'Video mode is available?
		TargetTimerSet 60000, TargetUp, 50	'Put targets back up, but not so fast ball is caught	
	End If	
End Sub

sub WarFail()
	Dim X
	tourClear()	
	loadLamp(player)								'Load the original lamp state back in
	spiritGuideEnable(1)
	comboKill()
	killNumbers()							'Turn off numbers	
	if (goldHits < 100) Then
		goldHits = 0								'Enable Gold for next time if we didn't get it
	End If	
	if ((ModeWon(player) AND 8)=8) Then								'Did we win this mode before?
		light 59, 7												'Make light solid, since it HAS been won
	Else
		light 59, 0												'Haven't won it yet, turn it off
	End If
	ghostModeRGB(0) = 0
	ghostModeRGB(1) = 0
	ghostModeRGB(2) = 0
	ghostFadeTimer = Int(100/CycleAdjuster)										'Fade out ghost
	ghostFadeAmount = Int(100/CycleAdjuster)
	setCabModeFade defaultR, defaultG, defaultB, 100				'Reset cabinet color
	killScoreNumbers()												'Disable any custom score numbers (so they won't pop up next time we build a custom score display)
	killCustomScore()	
	light 16, 0							'Turn off "Make Contact"
	light 17, 0
	light 18, 0	
	light 19, 0
	ghostLook = 1							'Ghost will now look around again.
	ghostAction = 0
	ghostMove 90, 50
	modeTotal = 0							'Reset mode points			
	Mode(player) = 0						'Set mode active to None
	'fortProgress(player) = 50				'50 indicates Mode is ready to start. You can re-start the Ghost Whore fight
	if ((modeRestart(player) AND 16)=16) Then							'Able to restart War Fort?
		modeRestart(player) = modeRestart(player) AND 239							'Clear the restart bit	
		fortProgress(player) = 50									'Mode start light re-lit
		pulse(44)													'Pulse the ARMY GHOST start light	
		popLogic(3)												'Set pops to EVP
		showProgress 0, player
	Else															'End mode, and let the ball drain
		light 44, 0												'Make sure Army Ghost light is OFF
		dirtyPoolMode(1)											'Don't want to trap balls anymore
		fortProgress(player) = 0									'Gotta start over
		if (barProgress(player) < 50) Then								'Haven't completed the Bar yet?
			popLogic(2)											'Set pops to advance Bar
		Else
			popLogic(1)											'Else, have them re-advance War Fort until we get it
		End If
		showProgress 0, player
	End If	
	Advance_Enable = 1
	checkModePost()
	for x=0 To 5									'Make sure the MB lights are off
		light 26 + x, 0
	Next
	hellEnable(1)
	showProgress 0, player					'Show the progress, Active Mode style	
End Sub

sub WarOver()
	Dim X
	Advance_Enable = 1						'Allow other modes to be started			
	Mode(player) = 0						'Set mode active to None
	fortProgress(player) = 100				'Flag that reminds us this mode has been won
	ModeWon(player) = ModeWon(player) OR 16	'Set WAR FORT WON bit for this player.	
	if (videoMode(player) > 0) Then									'Video mode is available?
		blink(17)
		blink(18)
		blink(19)
		videoMode(player) = 1										'Set to active
		loopCatch = catchBall										'Flag that we want to catch the ball in the loop
	End If	
	if (countGhosts() = 6) Then										'This the final Ghost Boss? Light BOSSES solid!
		light 48, 7
	End If
	minionEnd(2)								'Re-enable Minion find but do NOT let it control targets since this mode needs to do that
	'Manually do checkModePost logic, omitting Target Logic
	doorLogic()								'Figure out what to do with the door
	elevatorLogic()							'Did the mode move the elevator? Re-enable it and lock lights
	popLogic(0)								'Figure out what mode pops should be in
	if (countGhosts() = 2 or countGhosts() = 5) Then	'Defeating 2 or 5 ghosts lights EXTRA BALL
		extraBallLight(2)							'Light extra ball, no prompt we'll do there
		'videoSFX('S', 'A', 'A', allowSmall, 0, 255, 0, 'A', 'X', 'A' + random 2), 255	'"Extra Ball is Lit!"			
	End If	
	demonQualify()									'See if Demon Mode is ready
	for x=0 To 5									'Make sure the MB lights are off
		light 26 + x, 0
	Next
	hellEnable(1)
	showProgress 0, player						'Show the progress, Active Mode style	
End Sub

'END FUNCTIONS FOR WAR FORT MODE 3............................

sub videoModeLite()					'Allows player to trap ball under ghost to start Video Mode
	TargetTimerSet 10, TargetDown, 1
	blink(17)
	blink(18)
	blink(19)
	videoMode(player) = 1										'Set to active
	loopCatch = catchBall										'Flag that we want to catch the ball in the loop
End Sub

sub videoModeCheck()								'See if video mode ready and if so, pause it until mode is complete
	if (videoMode(player) = 1) Then
		videoMode(player) = 10
		loopCatch = 0				
	End If
End Sub

Function Switch(switchGet)							'EP- Since swithces actually throw Hit Events, this is not needed
End Function

Function cabSwitch(switchGet)						'EP- See above
End Function

Sub switchDebounce(whichSwitch)						'EP- See above
End Sub

Sub switchDebounceClear(startingX, EndingX)			'EP- See above
End Sub

Sub switchTest()
End Sub

Sub switchBinary()
End Sub

Sub solenoidsOff()
End Sub

Sub SetGhostModeRGB(R, G, B)						'Separate sub to set the ghost color I could probably just set the color in code itself.  Would be useful if I actually did some fading
	GhostModeRGB(0) = R
	GhostModeRGB(1) = G
	GhostModeRGB(2) = B
	DoRGB()
	Dim bulb
	for each bulb in Ghost_RGB
		bulb.Color=RGB(R,G,B)
	next
	Li64.intensity = GILight1.intensity * 80
'**	FlGhostR.alpha = R/2
'**	FlGhostG.alpha = G/2
'**	FlGhostB.alpha = B/2
'**	If GhostModeRGB(0) = 0 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost0"
'**	If GhostModeRGB(0) = 0 AND GhostModeRGB(1) = 255 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost1"
'**	If GhostModeRGB(0) = 128 AND GhostModeRGB(1) = 128 AND GhostModeRGB(2) = 128 Then PrGhost.Image = "Ghost2"
'**	If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 131 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost3"
'**	If GhostModeRGB(0) = 0 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 255 Then PrGhost.Image = "Ghost4"
'**	If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 255 Then PrGhost.Image = "Ghost5"
'**	If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 0 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost6"
'**	If GhostModeRGB(0) = 255 AND GhostModeRGB(1) = 255 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost7"
'**	If GhostModeRGB(0) = 200 AND GhostModeRGB(1) = 140 AND GhostModeRGB(2) = 0 Then PrGhost.Image = "Ghost8"	
End Sub

Sub setCabMode(lR, lG, lB)						'Set the current cabinet mode color, cabinet immediately turns this color (game start, ball end, etc)
	cabModeRGB(0) = lR							'Store colors in memory
	cabModeRGB(1) = lG
	cabModeRGB(2) = lB
	cabColor cabModeRGB(0), cabModeRGB(1), cabModeRGB(2), cabModeRGB(0), cabModeRGB(1), cabModeRGB(2)
	RGBTarget = RGB(cabModeRGB(0), cabModeRGB(1), cabModeRGB(2))
	doRGB()
	backglassoff(2)
	setBackGlass 1, "white"
	SetBackGlass 2, "white"
End Sub

Sub setcabmodefade(lR, lG, lB, theSpeed)							'Set the current cabinet mode color and fades to it at a certain speed (good for mode starts and ends)
	cabModeRGB(0) = lR												'Store colors in memory
	cabModeRGB(1) = lG
	cabModeRGB(2) = lB
	RGBspeed = theSpeed/10
	RGBtimer = RGBspeed												'How quickly it should change
	targetRGB(0) = cabModeRGB(0)					'Fade to mode colors
	targetRGB(1) = cabModeRGB(1)
	targetRGB(2) = cabModeRGB(2)
	RGBTarget = RGB(targetRGB(0), targetRGB(1), targetRGB(2))
		If lG = 255 Then
		'setCabModeFade 0, 255, 0, 600
		DOF 156, 1
	ElseIf lB = 255 Then
		'setCabModeFade 0, 0, 255, 50
		DOF 157, 1
	Else
		'setCabModeFade 0, 0, 0, 50
		resetallRGBDof()
	End If
End Sub

Sub flashCab(lR, lG, lB, flashSpeed) 		 						'Flash cab to this color, then fade back to normal mode color
	cabColor lR, lG, lB, lR, lG, lB									'Flash of color, sets current color to this
	RGBspeed = flashSpeed/10										'RGB timer will fade back to default mode color
	RGBtimer = RGBspeed												'How quickly it should change
	targetRGB(0) = cabModeRGB(0)									'Tell system to fade back to normal mode cabinet colors
	targetRGB(1) = cabModeRGB(1)
	targetRGB(2) = cabModeRGB(2)
	RGBTarget = RGB(targetRGB(0), targetRGB(1), targetRGB(2))
	If lR = 0 Then
		If lG = 0 Then
			'flashCab 0, 0, 255, 50
			DOF 150, 2
		Else
			If flashSpeed = 10 Then
				'flashCab 0, 255, 0, 10
				DOF 151, 2
			ElseIf flashSpeed = 100 Then
				'flashCab 0, 255, 0, 100
				DOF 152, 2
			Else
				'flashCab 0, 255, 0, 200
				DOF 153, 2
			End If
		End If
	ElseIf lR = 16 Then
		'flashCab 16, 0, 0, 75
		DOF 154, 2
	Else
		'flashcab 255, 255, *
		DOF 155, 2
	End If
End Sub

Sub cabColor(lR, lG, lB, rR, rG, rB)					'Set cabinet lights directly to a certain color. Doesn't store value
	dim bulb
	'dim rF,gF,bF
	dim rN,gN,bN,rL,gL,bL
	rL=0
	gL=0
	bL=0
	rN=0
	gN=0
	bN=0
	If lR = 0 Then
		If lB = 128 Then
			'cabColor 0, 0, 128, 0, 0, 128
			DOF 158, 1
		ElseIf lB = 255 Then
			'cabColor 0, 0, 0, 255, 0, 255
			DOF 159, 1
		Else
			'cabColor 0, 0, 0, 0, 0, 0
			resetallRGBDof()
		End If
	ElseIf lR = 35 Then
		'cabColor 35, 0, 35, 35, 0, 35
		resetallRGBDof()
	ElseIf lR = 70 Then
		'cabColor 70, 0, 70, 70, 0, 70
		resetallRGBDof()
	ElseIf lR = 105 Then
		'cabColor 105, 0, 105, 105, 0, 105
		resetallRGBDof()
	ElseIf lR = 140 Then
		'cabColor 140, 0, 140, 140, 0, 140
		DOF 160, 1
	ElseIf lR = 175 Then
		'cabColor 175, 0, 175, 175, 0, 175
		DOF 160, 1
	ElseIf lR = 210 Then
		'cabColor 210, 0, 210, 210, 0, 210
		DOF 161, 1
	ElseIf lR = 255 Then
		If lG = 255 Then
			'cabColor 255, 255, 255, 255, 255, 255
			DOF 162, 1
		Else
			If rR = 0 Then
				'cabColor 255, 0, 255, 0, 0, 0
				DOF 163, 1
			Else
				'cabColor 255, 0, 255, 255, 0, 255
				DOF 164, 1
			End If
		End If
	End If
	for each bulb in Left_RGB
		bulb.Color=RGB(lR,lG,lB)
		If lR > 0 then rL = 255 else rL = 0
		If lG > 0 then gL = 255 else gL = 0
		If lB > 0 then bL = 255 else bL = 0
		bulb.colorFull=RGB(rL,gL,bL)
	next
	for each bulb in Right_RGB
		bulb.Color=RGB(rR,rG,rB)
		If lR > 0 then rN = 255 else rN = 0
		If lG > 0 then gN = 255 else gN = 0
		If lB > 0 then bN = 255 else bN = 0
		bulb.colorFull=RGB(rN,gN,bN)
	next
'**	FlGlobalRedL.alpha = lR/10
'**	FlGlobalRedL1.alpha = lR
'**	FlGlobalGreenL.alpha = lG/10
'**	FlGlobalGreenL1.alpha = lG
'**	FlGlobalBlueL.alpha = lB/10
'**	FlGlobalBlueL1.alpha = lB
'**	FlGlobalRedR.alpha = lR/10
'**	FlGlobalRedR1.alpha = lR
'**	FlGlobalGreenR.alpha = lG/10
'**	FlGlobalGreenR1.alpha = lG
'**	FlGlobalBlueR.alpha = lB/10
'**	FlGlobalBlueR1.alpha = lB
'BackGlass Stuff
	Dim BGColorL, BGColorR
	If lR > 0 AND lG = 0 AND lB > 0 Then BGColorL = "purple"
	If lR > 0 AND lG = 0 AND lB = 0 Then BGColorL = "red"
	If lR > 0 AND lG > 0 AND lB > 0 Then BGColorL = "white"
	If lR = 0 AND lG = 0 AND lB = 0 Then BGColorL = "off"
	If lR = 0 AND lG > 0 AND lB = 0 Then BGColorL = "green"
	If lR = 0 AND lG = 0 AND lB > 0 Then BGColorL = "blue"
	If rR > 0 AND rG = 0 AND rB > 0 Then BGColorR = "purple"
	If rR > 0 AND rG = 0 AND rB = 0 Then BGColorR = "red"
	If rR > 0 AND rG > 0 AND rB > 0 Then BGColorR = "white"
	If rR = 0 AND rG = 0 AND rB = 0 Then BGColorR = "off"
	If rR = 0 AND rG > 0 AND rB = 0 Then BGColorR = "green"
	If rR = 0 AND rG = 0 AND rB > 0 Then BGColorR = "blue"
	backglassoff(2)
	SetBackGlass 1, BGColorL
	SetBackGlass 2, BGColorR
End Sub

Sub cabLeft(lR, lG, lB)					'Sets current left cab color to this value
	If lR = 255 Then
		DOF 165, 1
	Else
		resetallRGBDof()
	End If
	Dim bulb
	For Each bulb in Left_RGB
		bulb.Color = RGB(lR, lG, lB)
	Next
'**	FlGlobalRedL.alpha = lR/10
'**	FlGlobalRedL1.alpha = lR
'**	FlGlobalGreenL.alpha = lG/10
'**	FlGlobalGreenL1.alpha = lG
'**	FlGlobalBlueL.alpha = lB/10
'**	FlGlobalBlueL1.alpha = lB
End Sub

Sub cabRight(lR, lG, lB)					'Sets current right cab color to this value
	If lR = 255 Then
		DOF 165, 1
	Else
		resetallRGBDof()
	End If
	Dim bulb
	For Each bulb in Right_RGB
		bulb.Color = RGB(lR, lG, lB)
	Next
'**	FlGlobalRedR.alpha = lR/10
'**	FlGlobalRedR1.alpha = lR
'**	FlGlobalGreenR.alpha = lG/10
'**	FlGlobalGreenR1.alpha = lG
'**	FlGlobalBlueR.alpha = lB/10
'**	FlGlobalBlueR1.alpha = lB
	'doRGB()
End Sub

Sub setCabColor(cR, cG, cB, theSpeed)
	RGBspeed = theSpeed/10
	RGBtimer = RGBspeed							'How quickly it should change
	targetRGB(0) = cR * 0.3
	targetRGB(1) = cG * 0.3
	targetRGB(2) = cB * 0.3
	RGBTarget = RGB(targetRGB(0), targetRGB(1), targetRGB(2))
End Sub

Sub DoRGB()
'	EP- More hardware stuff, sending bits and bytes to the controller to physically turn on lights and what-not
End Sub

Sub RGBByte(RGBvalue)
End Sub

sub animatePF(startingFrame, totalFrames, repeatLights)
	if (startingFrame = 0 and totalFrames = 0) Then			'Kill animation?
		lightStatus = 0										'Clear status flag and return
		Exit Sub
	Else
'		lightStatus = lightAnimate | (repeatLights << 6)	'Set the animate bit, plus repeat flag if set EP- I don't get this; LightAnimate never changes (in this code), so lightStatus is only ever equal to 128 or 192 (if repeatLights is either 0 or 1 and after the bit shift)
'															lightStatus is only ever (as far as I can tell) 0, 128, or 192; It seems it only matters, in the code, if lightStatus is either 0 or not 0.
'															There must be something in the propeller coding to determine if a PF animation loops or not
		lightStatus = lightAnimate + repeatLights
		LightLoop = repeatLights							'EP- This is nowhere in the code, but I'm assuming this will get me what I want
	End If
	lightStart = startingFrame								'Set starting frame
	lightCurrent = startingFrame							'Set current position (at start)
	lightEnd =	startingFrame + totalFrames 				'Calculate end position now to save a step later
End Sub

sub blink(whichLamp)
	Dim X
	if (lampState(whichLamp) = 3) Then						'Was this light strobing?
		for x=whichLamp To (whichLamp + strobeAmount(whichLamp))-1
'			lamp(x) = 0										'Clear all strobing lights.
			light_inserts(x).Intensity = 0
'**			light_inserts(x).OnImage = "Playfield-On0-2"
'**			If whichLamp = 40 Then Fl40.alpha = 0
'**			If whichLamp = 41 Then Fl41.alpha = 0
'**			If whichLamp = 42 Then Fl42.alpha = 0
		Next
	End If
	lampState(whichLamp) = 1
End Sub

sub pulse(whichLamp)
	Dim X
	if (lampState(whichLamp) = 3) Then 						 'Was this light strobing?
		for x=whichLamp To (whichLamp + strobeAmount(whichLamp))-1
'			lamp(x) = 0										 'Clear all strobing lights.
'**			light_inserts(x).OnImage = "Playfield-On0-2"
			light_inserts(x).Intensity = 0
'**			If whichLamp = 40 Then Fl40.alpha = 0
'**			If whichLamp = 41 Then Fl41.alpha = 0
'**			If whichLamp = 42 Then Fl42.alpha = 0
			lampState(x) = 0
		Next
	End If
	lampState(whichLamp) = 2						 						
End Sub

sub light(whichLamp, howBright)
	Dim X
	if (lampState(whichLamp) = 3) Then 						 'Was this light strobing?
		for x=whichLamp To (whichLamp + strobeAmount(whichLamp))-1
'			lamp(x) = 0									 'Clear all strobing lights above it
'**			light_inserts(x).OnImage = "Playfield-On0-2"
			light_inserts(x).Intensity = 0
'**			If whichLamp = 40 Then Fl40.alpha = 0
'**			If whichLamp = 41 Then Fl41.alpha = 0
'**			If whichLamp = 42 Then Fl42.alpha = 0
			lampState(x) = 0
		Next
	End If
	if (howBright) Then										'Any light at all?
		if (whichLamp > 39 and whichLamp < 43) Then DOF 200 + whichLamp, 1
		lampState(whichLamp) = 10							'Normal lamp state, will NOT OR with animation		
	Else
		if (whichLamp > 39 and whichLamp < 43) Then DOF 200 + whichLamp, 0
		lampState(whichLamp) = 0							'No light (OK to OR with animation)
	End If
'	lamp(whichLamp) = howBright
	light_inserts(whichLamp).Intensity = howBright*8                      'bigus
'**	light_inserts(whichLamp).OnImage = "Playfield-On" & howBright & "-2"
'**	If whichLamp = 40 Then Fl40.alpha = howbright * 36.42
'**	If whichLamp = 41 Then Fl41.alpha = howbright * 36.42
'**	If whichLamp = 42 Then Fl42.alpha = howbright * 36.42
End Sub

sub strobe(whichLamp, howMany)
	Dim X
	lampState(whichLamp) = 3						 						
	strobeAmount(whichLamp) = howMany						'Set total number of lights to strobe (includes starting light)			
	for x=whichLamp+1 To (whichLamp + strobeAmount(whichLamp))-1
		lampState(x) = 33									'Clear lamp states of strobing lights. Example, if one was set to blink, strobe overwrites it.			
	Next
End Sub

sub storeLamp(whichPlayer)									'Stores current lamp values into specified player's lamp memory
	Dim X, lampPointer
	lampPointer = (whichPlayer - 1) * 64					'Start at 0, 64, 128, 192
	for x=0 To 63
		lampPlayers(lampPointer) = light_inserts(x).Intensity
'**		lampPlayers(lampPointer) = light_inserts(x).OnImage
		statePlayers(lampPointer) = lampState(x)
		strobePlayers(lampPointer) = strobeAmount(x)		
		lampPointer = lampPointer + 1
	Next
End Sub

sub loadLamp(whichPlayer)									'Load specified player's lamp memory into current display
	Dim X, lampPointer
	lampPointer = (whichPlayer - 1) * 64					'Start at 0, 64, 128, 192
	for x=0 To 63
		light_inserts(x).Intensity = lampPlayers(lampPointer)
'**		light_inserts(x).OnImage = lampPlayers(lampPointer)
		lampState(x) = statePlayers(lampPointer)
		strobeAmount(x) = strobePlayers(lampPointer)		
		lampPointer = lampPointer + 1
	Next
	spookCheck()
End Sub

sub allLamp(allValue)										'Turns off strobes, blinks, pulses. Sets all lamps to single value.
	Dim X
	for x=0 To 63
		light_inserts(x).Intensity = allValue
'**		light_inserts(x).OnImage = "Playfield-On" & allValue & "-2"
'**		If x = 40 Then Fl40.alpha = allValue * 36.42
'**		If x = 41 Then Fl41.alpha = allValue * 36.42
'**		If x = 42 Then Fl42.alpha = allValue * 36.42
		if (allValue) Then
			lampState(x) = 10
		Else
			lampState(x) = 0
		End If
		strobeAmount(x) = 0		
	Next
	spookCheck()											'See what Spook Again light should be doing
End Sub

sub updateRollovers()	'Call this to update the GLIR and ORB lights
		if ((orb(player) AND 36)=36) Then	'O lit?
			light 32, 7
		Else
			light 32, 0
		End If
		if ((orb(player) AND 18)=18) Then	'R lit?
			light 33, 7
		Else
			light 33, 0
		End If
		if ((orb(player) AND 9)=9) Then	'B lit?
			light 34, 7
		Else
			light 34, 0
		End If
		if ((rollOvers(player) AND 136)=136) Then	'G lit?
			light 52, 7
		Else
			light 52, 0
		End If
		if ((rollOvers(player) AND 68)=68) Then	'L lit?
			light 53, 7
		Else
			light 53, 0
		End If
		if ((rollOvers(player) AND 34)=34) Then	'I lit?
			light 54, 7
		Else
			light 54, 0
		End If
		if ((rollOvers(player) AND 17)=17) Then	'R lit?
			light 55, 7
		Else
			light 55, 0
		End If
End Sub

Sub Coil(whichCoil, HowLong)
End Sub

Sub GIbg(theValue)
'	EP- This is where some Back Box lighting stuff would go
End Sub

Sub GIpf(theValue)
'	EP- This has something to do with Playfield illumination, but I don't know what.
'	GIword &= B11111111 << 8			'Clear lower byte
'	GIword |= theValue					'Set lower byte to incoming
	If ((theValue AND 32) = 32) Then
		GI 3, 1
	Else
		GI 3, 0
	End If
	If ((theValue AND 64) = 64) Then
		GI 2, 1
	Else
		GI 2, 0
	End If
	If ((theValue AND 64) = 64) Then
		GI 1, 1
	Else
		GI 1, 0
	End If
End Sub

Sub BackGlass(leftBG, rightBG)
'	EP- More back box stuff?
End Sub

Sub defaultSettings()
'	EP- This is where we'd load default settings... if I could ever figure out how to store them
End Sub

Sub LoadSettings()
'	EP- This is where we'd load settings... if I could ever figure out how to store them
End Sub

Sub saveSettings()
'	EP- This is where we'd store settings...
End Sub

Sub saveAudits()
End Sub

Sub clearAudits()
End Sub

Sub NameGame(str)
End Sub

Sub printName()
End Sub

Sub printInitials()
End Sub

Sub printVersion()
End Sub

sub houseKeeping() 															'Run this routine all the time. It does lighting, checks cabinet switches, and enables solenoids
	On Error Resume Next
	Dim X
	Dim strobeFlag:strobeFlag = 0											'Whether or not we should move the strobing lights on this cycle
	blinkTimer = blinkTimer + lightSpeed
	if (blinkTimer > blinkSpeed1) Then	
		blinkTimer = 0	
	End If
	strobeTimer = strobeTimer + lightSpeed
	if (strobeTimer > strobeSpeed) Then	
		strobeTimer = 0
		strobeFlag = 1														'Set strobe flag.
	End If
	pulseTimer = pulseTimer + lightSpeed
	if (pulseTimer > pulseSpeed) Then
		pulseTimer = 0
		if (pulseDir = 0) Then
			pulseLevel = pulseLevel + 1
			if (pulseLevel > 8) Then
				pulseDir = 1				
			End If
		End If
		if (pulseDir = 1) Then
			pulseLevel = pulseLevel - 1
			if (pulseLevel < 1) Then
				pulseDir = 0				
			End If
		End If
	End If
	for x=0 to 63															'Cycle through lights, cab switches and solenoids
		if (lampState(x) = 0)	Then										'Light is off?
			if (lightStatus) Then											'Animations active?
				light_inserts(x).Intensity = CInt(3*Mid(lightShow(lightcurrent), x+1, 1))   'Bigus
'**				light_inserts(x).OnImage = "Playfield-On" & CInt(Mid(lightShow(lightcurrent), x+1, 1)) & "-2"
'**				If x = 40 Then Fl40.alpha = CInt(Mid(lightShow(lightcurrent), x+1, 1)) * 36.42
'**				If x = 41 Then Fl41.alpha = CInt(Mid(lightShow(lightcurrent), x+1, 1)) * 36.42
'**				If x = 42 Then Fl42.alpha = CInt(Mid(lightShow(lightcurrent), x+1, 1)) * 36.42
			Else
				light_inserts(x).Intensity = 0
				If (X >39 and x < 43) Then DOF 300 + x, 0
'**				light_inserts(x).OnImage = "Playfield-On0-2"
'**				If x = 40 Then Fl40.alpha = 0
'**				If x = 41 Then Fl41.alpha = 0
'**				If x = 42 Then Fl42.alpha = 0
			End If
		End If
		if (lampState(x) = 1)	Then										'Light set to blink?
			if (blinkTimer < blinkSpeed0) Then			
				light_inserts(x).Intensity = 40                            'bigus
				If (X >39 and x < 43) Then DOF 300 + x, 1
'**				light_inserts(x).OnImage = "Playfield-On7-2"
'**				If x = 40 Then Fl40.alpha = 255
'**				If x = 41 Then Fl41.alpha = 255
'**				If x = 42 Then Fl42.alpha = 255
			End If
			if (blinkTimer > blinkSpeed0) Then			
				light_inserts(x).Intensity = 0
				If (X >39 and x < 43) Then DOF 300 + x, 0
'**				light_inserts(x).OnImage = "Playfield-On0-2"
'**				If x = 40 Then Fl40.alpha = 0
'**				If x = 41 Then Fl41.alpha = 0
'**				If x = 42 Then Fl42.alpha = 0
			End If
		End If
		if (lampState(x) = 2)  Then											'Light set to pulsate? (0-8, 7-0)
			light_inserts(x).Intensity = pulseLevel *7                       'bigus
'**			light_inserts(x).OnImage = "Playfield-On" & pulseLevel & "-2"
'**			If x = 40 Then Fl40.alpha = pulseLevel * 36.42
'**			If x = 41 Then Fl41.alpha = pulseLevel * 36.42
'**			If x = 42 Then Fl42.alpha = pulseLevel * 36.42
		End If
		if (lampState(x) = 3)	Then										'Light set to strobe?
			If (x + strobePos(x)) > 67 Then Exit Sub
			if (strobeFlag) Then											'Did we roll over timer, and ready to strobe?
				strobePos(x) = strobePos(x) + 1
				if (strobePos(x) = strobeAmount(x)) Then					 '(lampState(x) >> 2) - 1) Then
					strobePos(x) = 0
				End If
			End If
			light_inserts(x + strobePos(x)).Intensity = 60                  'bigus
			On Error Resume Next
			If (X >39 and x < 43) Then DOF 400 + x, 1
'**			light_inserts(x + strobePos(x)).OnImage = "Playfield-On7-2"
'**			If x = 40 Then Fl40.alpha = 255
'**			If x = 41 Then Fl41.alpha = 255
'**			If x = 42 Then Fl42.alpha = 255
			if (strobePos(x) = 0) Then
				light_inserts(x + strobeAmount(x) - 1).Intensity = 0
				If (X >39 and x < 43) Then DOF 400 + x, 0
'**				light_inserts(x + strobeAmount(x) - 1).OnImage = "Playfield-On0-2"
			Else
				light_inserts(x + (strobePos(x) - 1)).Intensity = 0
				If (X >39 and x < 43) Then DOF 300 + x, 0
'**				light_inserts(x + (strobePos(x) - 1)).OnImage = "Playfield-On0-2"
			End If
		End If	
	Next
End Sub

Sub EOBNumbers(whichNum, numValue)
	
End Sub

'''''''''''''''''''''''''''''''''

Sub GI(WhichGI, WhichState)
	Dim X
	Dim bulb
	If WhichState = 1 Then
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
		Select Case WhichGI
			Case 1:
				For X=0 to 5 'slings, rubbers(above slings), lanes
					for each bulb in GI_lights1
						bulb.state = 1
					next
'**				For X=0 to 5
'**					PlasticsOn(X).IsDropped = 0
'**					PlasticsOff(X).IsDropped = 1
				Next
			Case 2:
				For X=6 To 9 'right main plastic, left main plastic, right ramp surround, ghost surround
					for each bulb in GI_lights2
						bulb.state = 1
					next
'**				For X=6 To 9
'**					PlasticsOn(X).IsDropped = 0
'**					PlasticsOff(X).IsDropped = 1
				Next
			Case 3:
				For X=10 To 11 'right of bumpers, top loop
					for each bulb in GI_lights3
						bulb.state = 1
					next
'**				For X=10 To 11
'**					PlasticsOn(X).IsDropped = 0
'**					PlasticsOff(X).IsDropped = 1
'**					RaBack.Image = "Back Board Baked On"
				Next
		End Select
	Else
Table1.colorgradeimage = "ColorGrade_4"
		Select Case WhichGI
			Case 1:
				For X=0 to 5
					for each bulb in GI_lights1
						bulb.state = 0
					next
'**					PlasticsOn(X).IsDropped = 1
'**					PlasticsOff(X).IsDropped = 0
				Next
			Case 2:
				For X=6 To 9
					for each bulb in GI_lights2
						bulb.state = 0
					next
'**					PlasticsOn(X).IsDropped = 1
'**					PlasticsOff(X).IsDropped = 0
				Next
			Case 3:
				For X=10 To 11
					for each bulb in GI_lights3
						bulb.state = 0
					next
'**					PlasticsOn(X).IsDropped = 1
'**					PlasticsOff(X).IsDropped = 0
'**					RaBack.Image = "Back Board Baked"
				Next
		End Select
	End If
End Sub

''''''''''''''''''''''''''''''

Sub VideoSFX(v1, v2, v3, vidAttributes, progressBar, vP, channel, folder, clip0, clip1, sP)
	Video v1, v2, v3, vidAttributes, progressBar, vP
	PlaySFX channel, folder, clip0, clip1, sP
End Sub

Sub videoQ(v1, v2, v3, vidAttributes, progressBar, vP)
	Dim vid, animLength, topRight, bottomRight, topLeft, bottomLeft, Spacer, onTop, onBottom
	animLength = progressBar
	If (vidAttributes AND loopVideo) = 128 Then animLength = 999999
	If IsNumeric(v3) Then									'EP- Many calls pass in the ansii number for the letter
		v3 = CInt(v3)
		If v3 > 30 Then										'EP- But some clips actually are just a number 0-9
			v3 = Chr(v3)
		End If
	End If
	If IsNumeric(v2) Then
		v2 = CInt(v2)
		If v2 > 30 Then										'EP- But some clips actually are just a number 0-9
			v2 = Chr(v2)
		End If
	End If
	vid = Cstr(v1&v2&v3)&".gif"
	DMDSceneQ vid, onTop, 15, onBottom, 15, 14, animLength, 14
End Sub

Sub videocombo(v1, v2, v3, vidAttributes, progressBar, vP)
	Dim vid, animLength, topRight, bottomRight, topLeft, bottomLeft, Spacer, onTop, onBottom
	animLength = progressBar
	If (vidAttributes AND loopVideo) = 128 Then animLength = 999999
	If IsNumeric(v3) Then									'EP- Many calls pass in the ansii number for the letter
		v3 = CInt(v3)
		If v3 > 30 Then										'EP- But some clips actually are just a number 0-9
			v3 = Chr(v3)
		End If
	End If
	If IsNumeric(v2) Then
		v2 = CInt(v2)
		If v2 > 30 Then										'EP- But some clips actually are just a number 0-9
			v2 = Chr(v2)
		End If
	End If
	vid = Cstr(v1&v2&v3)&".gif"
	if (comboVideoFlag) Then						'If the "Combo!" video is playing, enqueue this next file instead
		videoQ v1, v2, v3, vidAttributes, progressBar, vP
		comboVideoFlag = 0
		Exit Sub									'Then abort out of this function
	End If
	DMDSceneQ vid, onTop, 15, onBottom, 15, 14, animLength, 14
End Sub

Sub VideoEoB(v1, v2, v3, vidAttributes, progressBar, vP)
	Dim vid
	vid = Cstr(v1&v2&Chr(v3))&".gif"
	DMDSceneEOB vid, " ", -1, AreaProgress(player), 14, 14, progressBar, 14, vP
End Sub

sub video(v1, v2, v3, vidAttributes, progressBar, vP)
'	EP- I'm completely rewriting this since we don't use the propellor and UltraDMD has different functions/methods
'	I'm using the progressBar argument as the video length since we don't have a progress bar
	Dim vid, animLength, topRight, bottomRight, topLeft, bottomLeft, Spacer, onTop, onBottom
	animLength = progressBar
	If (vidAttributes AND loopVideo) = 128 Then animLength = 999999
	If IsNumeric(v3) Then									'EP- Many calls pass in the ansii number for the letter
		v3 = CInt(v3)
		If v3 > 30 Then										'EP- But some clips actually are just a number 0-9
			v3 = Chr(v3)
		End If
	End If
	If IsNumeric(v2) Then
		v2 = CInt(v2)
		If v2 > 30 Then										'EP- But some clips actually are just a number 0-9
			v2 = Chr(v2)
		End If
	End If
	vid = Cstr(v1&v2&v3)&".gif"
	if (comboVideoFlag) Then						'If the "Combo!" video is playing, enqueue this next file instead
		videoQ v1, v2, v3, vidAttributes, progressBar, vP
		comboVideoFlag = 0
		Exit Sub									'Then abort out of this function
	End If
	DMDScene vid, onTop, 15, onBottom, 15, 14, animLength, 14, vP
'	dataOut(0) = v1
'	dataOut(1) = v2
'	dataOut(2) = v3
'	dataOut(3) = vidAttributes
'	dataOut(4) = progressBar
'	dataOut(5) = vP
'	sendData(0x02)
	'Video Attribute Bit Settings:
	'0 = No numbers allowed
	'B00000001 = Small numbers allowed (corners, most allow it for timers)											
	'B00000010 = Large numbers allowed (most will block these)
	'B00000011 = All numbers allowed (probably not used much)
	'B10000000 = Video will loop itself
End Sub

Sub numbers(uL, uR, bL, bR)
'	EP- I cannot for the life of me figure this out, I'm just going to interpret things as best as possible using the comments
'	If uL <> "" Then CustNumbersUL = uL
'	If uR <> "" Then CustNumbersUR = uR
'	If bL <> "" Then CustNumbersBL = bL
'	If bR <> "" Then CustNumbersBR = bR
'*******************
'* Remove the following if you ever figure out how to implement this
'*******************
	CustNumbersUL = ""
	CustNumbersUR = ""
	CustNumbersBL = ""
	CustNumbersBR = ""
'******************* Stop removing**************
'	dataOut(0) = 1							'Which graphic type it is (numbers)
'	dataOut(1) = whichNumber				'Which number we're setting. 0-7 standard numbers, 8-11 custom score display numbers
'	dataOut(2) = numType					'Send type of number. Default numbers always terminate with currently playing video	
'	dataOut(3) = numX
'	dataOut(4) = numY
'	makeLong(5, numValue)
'	dataOut(9) = 0							'Not used here, but we need to clear it in case there's crap in the buffer
'	sendData(0x04)
	'NumType (4 LSB's):
	'0 = Numbers off. Unless you set the "end flag" you must do this manually to turn off numbers, else they will appear on everything.
	'1 = Large number, XY position (good for score bonus, jackpot values)
    '2 = Small number, XY position. (good for timers, in the screen corners)
	'	  XY positions are ignored for modes 3-5:
    '3 = (2) small numbers, upper left and right corners (good for countdown timers)
    '4 = (2) small numbers, lower left and right corners (good for countdown timers)
    '5 = (4) small numbers, all four corners
	'6 = Small single number, show Double Zeros (such as for a small score)
	'7 = Flash the number after currently playing video ends
	'8 = Show all four scores on right hand side of screen for Match animation
	'9 = Display Ball # at specified number position
	'Set Number as Timer:
	'B0001000 OR'd into Numbertype
	'FX commands (3 MSB's)
	'001xxxxx - Flash the number every other frame
	'010xxxxx - Number displays Player's score (player # determined by number value)
	'100xxxxx - Reserved for future use
End Sub

Sub KillNumbers()
	CustNumbersUL = ""
	CustNumbersUR = ""
	CustNumbersBL = ""
	CustNumbersBR = ""	
End Sub

Sub KillScoreNumbers()
	CustNumbersUL = ""
	CustNumbersUR = ""
	CustNumbersBL = ""
	CustNumbersBR = ""	
End Sub

Sub KillCustomScore()
'	EP- I don't know what to do here
End Sub

Sub ShowValue(numValue, flashTime, scoreFlag)
	if (scoreFlag) Then
		AddScore(numValue)					'Increase the score (AddScore automatically applies multiplier)
	End If
	DMDScene "", numvalue, 15, "", 0, 14, flashTime*50, 14, 50
End Sub

sub killTimer(whichNumber)					'Terminate a permanent number (usually a timer number)
'	numbers whichNumber, numberStay, 0, 0, 0
	numbers "", "", "", ""					'EP- I'm just guessing here
End Sub

sub AddScore(scoreAmount)
	playerScore(player) = playerScore(player) + (scoreAmount * scoreMultiplier)	
	scoreBall = 1														'Flag that points were indeed scored this ball (free ball if you don't)
	SetScore(player)
	if (Advance_Enable = 0 or minion(player) > 0) Then					'In some sort of mode?
		modeTotal = modeTotal + (scoreAmount * scoreMultiplier)			'Increase the Mode Score too
	End If
	if (playerScore(player) >= replayValue and replayPlayer(player) = 0 and allowReplay > 0) Then					'Enough for a replay, and they're allowed?			
		replayPlayer(player) = 1							'Set flag so this can only happen once
		'video('S', 'R', 'P', 0, 0, 255)					'Replay sound and graphic	
		'playSFX(0, 'A', 'X', 'Z', 255)
		replayGet = replayGet + 1
		if (freePlay = 0) Then								'Only advance this if it's actually in freeplay mode
			credits = credits + 1
			DOF 126, 1
			DOF 111, 2
		End If	
		Update(255)											'Send current data, and 255 means Set Replay Notice Flag
	End If	
	If Not UltraDMD.IsRendering Then DMDScore()
End Sub

Sub CustomScore(v1, v2, v3, vidAttributes, length)
'	EP- I'm just going to treat this like a normal video call
	VideoQ v1, v2, v3, vidAttributes, length, 255
End Sub

sub Update(itsState)
'	EP- Hardware stuff, sending stuff to the propeller
End Sub

sub stopVideo(doWhat)								'0 Stop (1 or resume?) playing video
	'0 = Stop video
	' 1 = Resume video from where we left off, and finish/loop it (Not implemented yet)
'	video(doWhat, 0, 0, 0, 0, 0)
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
	End If
End Sub

sub playSFX(whichChannel, folder, clip0, clip1, priority)
'	EP- Complete re-write since we don't have any hardware stuff
	Dim clip2play
	If IsNumeric(clip1) Then						'EP- Many calls pass in the ansii number for the letter
		If clip1 > 9 Then							'EP- But some clips actually are just a number 0-9
			clip1 = Chr(clip1)
		End If
	End If
	If IsNumeric(clip0) Then
		If clip0 > 9 Then							'EP- But some clips actually are just a number 0-9
			clip0 = Chr(clip0)
		End If
	End If
	clip2play = Cstr(folder & clip0 & clip1)		'EP- And some calls actually add a number to the letter; I will have to change the call which means going through all 380+ calls
	If priority >= SndChannel(whichChannel) Then
		StopSound SndPlaying(whichChannel)
		PlaySound clip2play
		SndChannel(whichChannel) = priority
		SndPlaying(whichChannel) = clip2play
		TiChannel0.Enabled = 1
		TiChannel1.Enabled = 1
		TiChannel2.Enabled = 1
		TiChannel3.Enabled = 1
		TiChannel4.Enabled = 1
		TiChannel5.Enabled = 1
		TiChannel6.Enabled = 1
		TiChannel7.Enabled = 1
	End If
End Sub

Dim LeftRight:Leftright = -0.8
Sub stereoSFX(whichChannel, folder, clip0, clip1, priority, leftValue, rightValue)				'EP- VP can't really do stereo FX, so this is just going to be a regualr sound fx player
	Dim clip2play
	If IsNumeric(clip1) Then						'EP- Many calls pass in the ansii number for the letter
		If clip1 > 9 Then							'EP- But some clips actually are just a number 0-9
			clip1 = Chr(clip1)
		End If
	End If
	clip2play = Cstr(folder & clip0 & clip1)		'EP- And some calls actually add a number to the letter; I will have to change the call which means going through all 380+ calls
	If priority >= SndChannel(whichChannel) Then
		StopSound SndPlaying(whichChannel)
		PlaySound clip2play, 0, 1, LeftRight, 0, 0, 0, 0
		SndChannel(whichChannel) = priority
		SndPlaying(whichChannel) = clip2play
		TiChannel0.Enabled = 1
		TiChannel1.Enabled = 1
		TiChannel2.Enabled = 1
		TiChannel3.Enabled = 1
		TiChannel4.Enabled = 1
		TiChannel5.Enabled = 1
		TiChannel6.Enabled = 1
		TiChannel7.Enabled = 1
	End If
	LeftRight = LeftRight * -1
End Sub

Sub PlaySFXQ(whichChannel, folder, clip0, clip1, priority)
'	EP- I don't feel like setting up timers to constantly check if a channel is clear and as soon as it is clear to play this sound, but that's how you would do it
'	EP- As of right now, it's just the same as PlaySFX, so if the priority is high enough, it'll stop the sound
	Dim clip2play
	If IsNumeric(clip1) Then						'EP- Many calls pass in the ansii number for the letter
		If clip1 > 9 Then							'EP- But some clips actually are just a number 0-9
			clip1 = Chr(clip1)
		End If
	End If
	clip2play = Cstr(folder & clip0 & clip1)		'EP- And some calls actually add a number to the letter; I will have to change the call which means going through all 380+ calls
	If priority >= SndChannel(whichChannel) Then
		StopSound SndPlaying(whichChannel)
		PlaySound clip2play
		SndChannel(whichChannel) = priority
		SndPlaying(whichChannel) = clip2play
		TiChannel0.Enabled = 1
		TiChannel1.Enabled = 1
		TiChannel2.Enabled = 1
		TiChannel3.Enabled = 1
		TiChannel4.Enabled = 1
		TiChannel5.Enabled = 1
		TiChannel6.Enabled = 1
		TiChannel7.Enabled = 1
	End If
End Sub

Sub KillQ()
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
	End If
End Sub

sub volumeSFX(whichChannel, volLeft, volRight)
'	EP- Leaving blank for now, possibly fore-ev-er
End Sub

Sub StopMusic()
	EndMusic()
End Sub

Sub FadeMusic(fadeSpeed, fadeTarget)
'	EP- Leaving this blank since, as far as I know, VP can't fade music
End Sub

Sub SetScore(whichPlayer)
'	I'm leaving this blank as it seems to be about setting a bit to display on the DMD
End Sub

Sub ManualScore(whichScore, whatValue)
'	EP- Hopefully this doesn't actually update the score; I hope it just displays it, so I'll leave it blank for now
End Sub

sub videoPriority(newPriority)							'Manually sets a new priority, such as 0, allowing us to load a new low-priority video such as Skill Shot Loop
'	video 255, 0, 0, 0, 0, newPriority
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
	End If
End Sub

sub RepeatMusic(yesNo)
	MusicRepeats = yesNo
End Sub

'********************************************************************************************************************************************************************************
'* Shoopity's (EP) implementation of stuff
'********************************************************************************************************************************************************************************

'*******************************
'* Switches
'******************************
Sub WaSw16_Hit()
	DOF 109, 2
	if (wiki(player) < 255) Then								'Hasn't been completed yet?
		wiki(player) = wiki(player) + 1							'Increment counter
	Else
		AddScore(5000)											'Some points for hitting inert target
		playSFX 2, "H", "0", "0", 250							'REJECT sound
';		video "S", "W", "F", allowSmall, 0, 255					'VIDEO THAT SAYS COMPLETE REST TO RE-LITE WIKI
	End If
	if (wiki(player) < 4) Then									'Not spelled yet?
		video "S", "W", 64 + wiki(player), allowSmall, 30, 250	'Advance letters
		playSFX 2, "S", "A", "X", 250							'Normal WIKI sound
		AddScore(25000)			
	End If
	if (wiki(player) = 4) Then
		minionDamage = minionDamage + 1							'Increase the damage Minion will take, this ball only
		if (minionDamage > 5) Then								'Top off at 5
			minionDamage = 5				
		End If
		AddScore(100000)		
		video "S", "W", "D", 0, 30, 255							'WIKI completed!
		videoQ "S", "W", 69 + minionDamage, 0, 0, 238			'Show 2-5x Minion Damage (it will never be lower than 2 here)
		playSFX 0, "S", "A", "X", 255							'WIKI complete sound
		wiki(player) = 255										'Set TECH to complete (must complete all 3 light restart / re-light)
		light 0, 7												'Light WIKI solid			
		spiritGuidelight 										'If it needs to be re-lit, re-lite it
	End If	
End Sub

Sub WaSw17_Hit()
	DOF 125, 2
	if (tech(player) < 255) Then								'Hasn't been completed yet?
		tech(player) = tech(player) + 1							'Increment counter
	Else
		AddScore(5000)											'Some points for hitting inert target
		playSFX 2, "H", "0", "0", 250							'REJECT sound
';		video "S", "T", "F", allowSmall, 0, 255					'VIDEO THAT SAYS COMPLETE REST TO RE-LITE TECH
	End If
	if (tech(player) < 4) Then									'Not spelled yet?
		video "S", "T", 64 + tech(player), allowSmall, 30, 250
		playSFX 2, "S", "A", "Y", 250							'Normal TECH sound
		AddScore(25000)
	End If
	if (tech(player) = 4) Then
		AddScore(200000)		
		video "S", "T", 64 + tech(player), allowSmall, 30, 255	'TECH complete, Overclocked!
		playSFX 0, "S", "B", "Y", 255							'OVERCLOCKED sound
		tech(player) = 255										'Set TECH to complete (must complete all 3 light restart / re-light)
		light 1, 7												'Light tech solid	
		saveCurrent(player) = saveCurrent(player) + 2			'Add 2 seconds to this player's Ball Save timer			
		spiritGuidelight 										'If it needs to be re-lit, re-lite it
	End If
End Sub

Sub WaSw18_Hit()
	TargetBankTargetsHit()												'EP- Run the logic for not caring which target is hit
	Dim TargetHit
	If PrBankTop.Z >= TargetUp Then
'		switchDebounce(19)												'Debounce the other switches so only gets hit at once
'		switchDebounce(20)
		targetHit = 1													'Set flag
		if (minion(player) = 1 and minionsBeat(player) > 2) Then		'Can Minion be advanced, and it is Minion 3 or higher?
			targetBits = (targetBits AND 251)						'~B00000100									'Clear that bit
			light 17, 7													'Turn that light SOLID
			if (gTargets(0) = 0) Then									'Haven't hit this one yet?
				minionHits = minionHits - 1								'Used to set incrementing sound
				if (minionHits = 2) Then								'Make lights solid to count how many we've hit
					playSFX 2, "M", "J", "0", 250						'Minion target SFX (slightly longer)
					ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement	
				End If
				if (minionHits = 1) Then
					playSFX 2, "M", "J", "1", 250						'Minion target SFX (slightly longer)
					ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement	
				End If	
				gTargets(0) = 1											'Set the flag that we already hit this				
				ghostMove 90, 250										'Ghost only reacts if you HAVEN'T hit that target yet
				AddScore(minionTarget(player) * 10510)					'Increase score
				flashCab 0, 0, 255, 50									'Brief flash to blue
			Else
				playSFX 2, "H", "0", "0", 100							'Clunking sound
				AddScore(1000)											'Nominal points
			End If
			if (targetBits) Then										'Haven't cleared them all yet?				
				killQ()
				video "M", "I", 64 + targetBits, allowSmall, 30, 210	'Show which blocks are cleared			
				videoQ "M", "I", "G", allowSmall, 25, 100				'"Clear targets to find minions"
			Else
				minionStart()											'Start the battle!
			End If
		End If
		if (fortProgress(player) = 60 and gTargets(0) = 1) Then			'Ghost already hit?
			video "W", "B", "H", allowSmall, 32, 255					'Show ball missing him,
			'videoQ('W', 'A', 64 + soldierUp, allowSmall, 0, 200)		'then back to Soldier View
			playSFX 0, "W", "9", "Z", 255								'Soldier miss noise!
			AddScore(5000)
		End If
		if (fortProgress(player) = 60 and gTargets(0) = 0) Then			'Are we trying to knock down Ghost Soldiers?
			if (goldHits = 10) Then
				ghostAction = Int(229999/cycleadjuster)					'Set WHACK routine, turns back towards door
			Else
				ghostAction = Int(339999/cycleadjuster)					'Set WHACK routine, turn back towards front			
			End If
			AddScore(250000)
			soldierUp = soldierUp AND 251							'~B00000100							'Subtract that soldier
			'playSFX(0, 'W', '9', 'A' + random(16), 255)				'Soldier hit noise! (will be more random later on)
			light 17, 7													'Turn that light SOLID.
			video "W", "A", "H", allowSmall, 32, 255 					'Show soldier on left knocked down
			gTargets(0) = 1												'Set the flag that we already hit this
			if (soldierUp = 0) Then										'All soldiers down?
				WarFight()
			Else
				playSFX 0, "W", "9", 65 + random(16), 255				'Soldier hit noise!				
				customScore "W", "A", 64 + soldierUp, allowAll OR loopVideo, 150		'Shoot score with targets in front
				'videoQ('W', 'A', 64 + soldierUp, allowSmall, 0, 200)
			End If
		End If
		if (hotProgress(player) = 30) Then								'Are we trying to qualify Hotel Jackpots?
			if (gTargets(0) = 0) Then									'Target not hit yet
				playSFX 0, "L", "8", 65 + random(8), 255				'Jackpot multiplier sound + voice
				jackpotMultiplier = jackpotMultiplier + 1				
				video "L", "M", jackpotMultiplier, allowSmall, 26, 255	'Show multiplier		
				'videoQ('L', '8', 'E', allowSmall, 0, 200)				'Ramp re-lights Jackpot					
				light 17, 7												'Turn that light SOLID.
				gTargets(0) = 1											'Set the flag that we already hit this
				AddScore(100000)
				sendJackpot(0)											'Send updated jackpot value to score #0
				if (jackpotMultiplier = 3) Then							'Jackpot maxed out?
					customScore "L", "P", "C", allowAll OR loopVideo, 30		'Change prompt to only mention Ramp (no more point hitting ghost)
				End If
			Else
				playSFX 0, "L", "W", 65 + random(8), 255				'Oh noes!				
				video "L", "8", "A", allowSmall, 20, 240					'Ghost worried!
				'videoQ('L', '8', 'E', allowSmall, 0, 200)				'Ramp re-lights Jackpot					
				AddScore(10000)					
			End If
		End If
		if (priProgress(player) > 9 and priProgress(player) < 13) Then	'Are we freeing our friends from Ghost Prison?
			ghostFlash(100)
			targetBits = targetBits AND 251						'~B00000100									'Clear that bit
			light 17, 7													'Turn that light SOLID
			if (targetBits) Then										'Haven't cleared them all yet?				
				if (gTargets(0) = 1) Then
					AddScore(10)										'Pwned
					playSFX 2, "H", "0", "0", 100						'CLUNK!	
				Else
					AddScore(50070)										'Increase score
					gTargets(0) = 1										'Set the flag that we already hit this						
					playSFX 2, "P", "5", 85 + random(3), 200			'Random chain whack sound
'						video "P", "A", "Y", 0, 0, 255						'Flash transition EP- Removing because I don't want a flash transition
					customScore "P", "5", 64 + (targetBits AND 7), allowSmall OR loopVideo, 177		'Shoot score with targets in front
				End If					
			Else
				PrisonRelease()											'Release a friend
			End If
			modeTimer = 0												'Reset timer so ghost prompt doesn't override audio
		End If	
		if (barProgress(player) = 70) Then								'Trying to free our friend from Ghost Whore?				
			BarTarget(0)
		End If			
	End If
End Sub

Sub WaSw19_Hit()
	TargetBankTargetsHit()												'EP- Run the logic for not caring which target is hit
	Dim TargetHit
	If PrBankTop.Z >= TargetUp Then
'		switchDebounce(18)													'Debounce the other switches so only gets hit at once
'		switchDebounce(20)
		targetHit = 1														'Set flag
		if (minion(player) = 1 and minionsBeat(player) > 2) Then			'Can Minion be advanced?
			targetBits = targetBits AND 253							'~B00000010							'Clear that bit
			light 18, 7														'Turn that light OFF
			if (gTargets(1) = 0) Then									'Haven't hit this one yet?
				minionHits = minionHits - 1									'Used to set incrementing sound
				if (minionHits = 2) Then									'Make lights solid to count how many we've hit
					playSFX 2, "M", "J", "0", 250							'Minion target SFX (slightly longer)
					ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement	
				End If
				if (minionHits = 1) Then
					playSFX 2, "M", "J", "1", 250							'Minion target SFX (slightly longer)
					ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement	
				End If	
				gTargets(1) = 1												'Set the flag that we already hit this				
				ghostMove 90, 250											'Ghost only reacts if you HAVEN'T hit that target yet
				AddScore(minionTarget(player) * 10510)						'Increase score
				flashCab 0, 0, 255, 50										'Brief flash to blue
			Else
				playSFX 2, "H", "0", "0", 100								'Clunking sound
			End If
			if (targetBits) Then											'Haven't cleared them all yet?				
				killQ()
				video "M", "I", 64 + targetBits, allowSmall, 30, 210		'Show which blocks are cleared			
				videoQ "M", "I", "G", 2, 25, 100							'"Clear targets to find minions"
			Else
				minionStart()												'Start the battle!
			End If
		End If			
		if (fortProgress(player) = 60 and gTargets(1) = 1) Then				'Ghost already hit?
			video "W", "B", "I", allowSmall, 32, 255						'Show ball missing him,
			'videoQ('W', 'A', 64 + soldierUp, 2, 0, 200)					'then back to Soldier View
			playSFX 0, "W", "9", "Z", 255									'Soldier miss noise!
			AddScore(5000)
		End If
		if (fortProgress(player) = 60 and gTargets(1) = 0) Then				'Are we trying to knock down Ghost Soldiers?
			if (goldHits = 10) Then
				ghostAction = Int(229999/cycleadjuster)						'Set WHACK routine, turns back towards door
			Else
				ghostAction = Int(339999/cycleadjuster)						'Set WHACK routine, turn back towards front			
			End If
			AddScore(250000)
			soldierUp = soldierUp AND 253							'~B00000010							'Subtract that soldier
			'playSFX(0, 'W', '9', 'A' + random(16), 255)					'Soldier hit noise!
			light 18, 7														'Turn that light SOLID.	
			video "W", "A", "I", allowSmall, 32, 255 						'Show soldier in middle knocked down
			gTargets(1) = 1													'Set the flag that we already hit this
			if (soldierUp = 0) Then
				WarFight()
			Else
				playSFX 0,  "W", "9", 65 + random(16), 255					'Soldier hit noise!				
				customScore "W", "A", 64 + soldierUp, allowAll OR loopVideo, 150	'Shoot score with targets in front
				'videoQ('W', 'A', 64 + soldierUp, allowSmall, 0, 200)
			End If
		End If
		if (hotProgress(player) = 30) Then									'Are we trying to qualify Hotel Jackpots?
			if (gTargets(1) = 0) Then										'Target not hit yet
				playSFX 0, "L", "8", 65 + random(8), 255					'Jackpot multiplier sound + voice
				jackpotMultiplier = jackpotMultiplier + 1				
				video "L", "M", jackpotMultiplier, allowSmall, 26, 255		'Show multiplier
				'videoQ('L', '8', 'E', allowSmall, 0, 200)					'Ramp re-lights Jackpot					
				light 18, 7													'Turn that light SOLID.
				gTargets(1) = 1												'Set the flag that we already hit this
				AddScore(100000)
				sendJackpot(0)												'Send updated jackpot value to score #0
				if (jackpotMultiplier = 3) Then								'Jackpot maxed out?
					customScore "L", "P", "C", allowAll OR loopVideo, 30	'Change prompt to only mention Ramp (no more point hitting ghost)
				End If					
			Else
				playSFX 0, "L", "W", 65 + random(8), 255					'Oh noes!				
				video "L", "8", "A", allowSmall, 20, 240					'Ghost worried!
				'videoQ('L', '8', 'E', allowSmall, 0, 200)					'Ramp re-lights Jackpot					
				AddScore(10000)					
			End If
		End If
		if (priProgress(player) > 9 and priProgress(player) < 13) Then		'Freeing friends from prison?
			ghostFlash(100)			
			targetBits = targetBits AND 253							'~B00000010									'Clear that bit
			light 18, 7														'Turn that light SOLID
			if (targetBits) Then											'Haven't cleared them all yet?				
				if (gTargets(1) = 1) Then
					AddScore(10)											'Pwned
					playSFX 2, "H", "0", "0", 100							'CLUNK!	
				Else
					AddScore(50070)											'Increase score
					'video('P', '5', 64 + (targetBits & B00000111), allowSmall | loopVideo, 0, 200)			'Show which blocks are cleared	
					playSFX 2, "P", "5", 85 + random(3), 200				'Random chain whack sound	
					gTargets(1) = 1											'Set the flag that we already hit this	
'					video "P", "A", "Y", 0, 0, 255							'Flash transition
					customScore "P", "5", 64 + (targetBits AND 7), allowSmall OR loopVideo, 177		'Shoot score with targets in front
				End If
			Else
				PrisonRelease()												'Release a friend
			End If
			modeTimer = 0													'Reset timer so ghost prompt doesn't override audio
		End If
		if (barProgress(player) = 70) Then									'Trying to free our friend from Ghost Whore?				
			BarTarget(1)
		End If
	End If
End Sub

Sub WaSw20_Hit()
	TargetBankTargetsHit()												'EP- Run the logic for not caring which target is hit
	Dim TargetHit
	If PrBankTop.Z >= TargetUp Then
'		switchDebounce(18)													'Debounce the other switches so only gets hit at once
'		switchDebounce(19)
		targetHit = 1														'Set flag
		if (minion(player) = 1 and minionsBeat(player) > 2) Then			'Can Minion be advanced?
			targetBits = targetBits AND 254							'~B00000001							'Clear that bit
			light 19, 7														'Turn that light OFF
			if (gTargets(2) = 0) Then										'Haven't hit this one yet?
				minionHits = minionHits - 1									'Used to set incrementing sound
				if (minionHits = 2) Then									'Make lights solid to count how many we've hit
					playSFX 2, "M", "J", "0", 250							'Minion target SFX (slightly longer)
					ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement	
				End If
				if (minionHits = 1) Then
					playSFX 2, "M", "J", "1", 250							'Minion target SFX (slightly longer)
					ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement	
				End If	
				gTargets(2) = 1												'Set the flag that we already hit this				
				ghostMove 90, 250											'Ghost only reacts if you HAVEN'T hit that target yet
				AddScore(minionTarget(player) * 10510)						'Increase score
				flashCab 0, 0, 255, 50										'Brief flash to blue
			Else
				playSFX 2, "H", "0", "0", 100								'Clunking sound
			End If
			if (targetBits) Then											'Haven't cleared them all yet?				
				killQ()
				video "M", "I", 64 + targetBits, allowSmall, 30, 210		'Show which blocks are cleared			
				videoQ "M", "I", "G", allowSmall, 25, 100					'"Clear targets to find minions"
			Else
				minionStart()												'Start the battle!
			End If
		End If	
			if (fortProgress(player) = 60 and gTargets(2) = 1) Then			'Ghost already hit?
			video "W", "B", "J", allowSmall, 32, 255						'Show ball missing him,
			'videoQ('W', 'A', 64 + soldierUp, allowSmall, 0, 200)			'then back to Soldier View
			playSFX 0, "W", "9", "Z", 255									'Soldier miss noise!
			AddScore(5000)
		End If
		if (fortProgress(player) = 60 and gTargets(2) = 0) Then				'Are we trying to knock down Ghost Soldiers?
			if (goldHits = 10) Then
				ghostAction = Int(229999/cycleadjuster)						'Set WHACK routine, turns back towards door
			Else
				ghostAction = Int(339999/cycleadjuster)						'Set WHACK routine, turn back towards front			
			End If
			AddScore(250000)
			soldierUp = soldierUp AND 254							'~B00000001							'Subtract that soldier
			'playSFX(0, 'W', '9', 'A' + random(16), 255)					'Soldier hit noise!
			light 19, 7														'Turn that light SOLID.
			video "W", "A", "J", allowSmall, 32, 255 						'Show soldier on right knocked down
			gTargets(2) = 1													'Set the flag that we already hit this
			if (soldierUp = 0) Then
				WarFight()
			Else
				playSFX 0, "W", "9", 65 + random(16), 255					'Soldier hit noise!
				customScore "W", "A", 64 + soldierUp, allowAll OR loopVideo, 150		'Shoot score with targets in front		
				'videoQ('W', 'A', 64 + soldierUp, allowSmall, 0, 200)
			End If
		End If
		if (hotProgress(player) = 30) Then									'Are we trying to qualify Hotel Jackpots?
			if (gTargets(2) = 0) Then										'Target not hit yet
				playSFX 0, "L", "8", 65 + random(8), 255					'Jackpot multiplier sound + voice
				jackpotMultiplier = jackpotMultiplier + 1				
				video "L", "M", jackpotMultiplier, allowSmall, 26, 255		'Show multiplier
				'videoQ('L', '8', 'E', allowSmall, 0, 200)					'Ramp re-lights Jackpot					
				light 19, 7													'Turn that light SOLID.
				gTargets(2) = 1												'Set the flag that we already hit this
				AddScore(100000)
				sendJackpot(0)												'Send updated jackpot value to score #0
				if (jackpotMultiplier = 3) Then								'Jackpot maxed out?
					customScore "L", "P", "C", allowAll OR loopVideo, 30	'Change prompt to only mention Ramp (no more point hitting ghost)
				End If					
			Else
				playSFX 0, "L", "W", 65 + random(8), 255					'Oh noes!				
				video "L", "8", "A", allowSmall, 20, 240					'Ghost worried!
				'videoQ('L', '8', 'E', allowSmall, 0, 200)					'Ramp re-lights Jackpot					
				AddScore(10000)					
			End If
		End If
		if (priProgress(player) > 9 and priProgress(player) < 13) Then		'Are we trying to Free Friends from Prison?
			ghostFlash(100)
			targetBits = targetBits AND 254							'~B00000001									'Clear that bit
			light 19, 7														'Turn that light SOLID
			if (targetBits) Then											'Haven't cleared them all yet?				
				if (gTargets(2) = 1) Then
					AddScore(10)											'Pwned
					playSFX 2, "H", "0", "0", 100							'CLUNK!	
				Else
					AddScore(50070)											'Increase score
					'video('P', '5', 64 + (targetBits & B00000111), allowSmall | loopVideo, 0, 200)			'Show which blocks are cleared	
					playSFX 2, "P", "5", 85 + random(3), 200				'Random chain whack sound	
					gTargets(2) = 1											'Set the flag that we already hit this
'					video "P", "A", "Y", 0, 0, 255							'Flash transition
					customScore "P", "5", 64 + (targetBits AND 7), allowSmall OR loopVideo, 177		'Shoot score with targets in front						
				End If
			Else
				PrisonRelease()												'Release a friend
			End If
			modeTimer = 0													'Reset timer so ghost prompt doesn't override audio
		End If
		if (barProgress(player) = 70) Then									'Trying to free our friend from Ghost Whore?				
			BarTarget(2)
		End If
	End If
End sub

Sub TrSw24m_Hit()
	mMagnaSave.AddBall ActiveBall
End Sub

Sub TrSw24m_UnHit()
	mMagnaSave.RemoveBall ActiveBall
End Sub

Sub TrSw24_Hit()
	Sw24 = 1
	If (MagnetTimer = 0 and TargetLocation = TargetDown) Then							'GHOST HIT? (the loop)
		switchDead = 0																	'Since it's not a matrix switch, we set this manually	
		animatePF 179, 10, 0															'Center explode!
		if (photosToGo) Then
			killQ()																		'Disable any Enqueued videos		
			playSFX 2, "A", "Z", "Z", 255												'Whoosh!
			photoTimer = longSecond * 2													'Reset timer, with a little padding
			countSeconds = countSeconds + loopSecondsAdd
			photoValue = (countSeconds * 10000) + (100000 * (photosNeeded(player) - 2))	'Re-calculate next photo value	
'			numbers(9, 2, 68, 27, photoValue)											'Update display Photo Value
			numbers "", photoValue, "", ""
			ghostAction = Int(20000/CycleAdjuster)										'Whack routine
			if (countSeconds > 60) Then													'At limit?
				countSeconds = 25														'Reset
				AddScore(500000)														'Give secret bonus
				killQ()
'				numbers(1, numberFlash | 1, 255, 11, 500000)							'500k
				numbers "", "", "500,000", "500,000"
				video "F", "9", "V", noEntryFlush Or 3, 30, 255
				playSFX 2, "A", "Z", "Z", 255
			Else
				video "F", "9", "U", allowSmall, 30, 255								'Timer add message
				playSFX 2, "A", "Z", "Z", 255											'Whoosh!
			End If
';			numbers(0, numberStay | 4, 0, 0, countSeconds - 1)							'Update the Numbers Timer. We do "-1" so it'll display a zero.  EP- don't know what this is updating
			flashCab 255, 0, 0, 50														'Bright red, brief flash
		End If
		if (deProgress(player) = 20 and activeBalls > 1) Then							'Bashing Demon, and not on our last ball?
			DemonJackpot()			
		End If
		if (theProgress(player) > 9 and theProgress(player) < 50) Then					'Theater Ghost?
			TheaterWin()																'Mode complete!
		End If
		if (minion(player) = 10) Then													'Are we fighting a Minion?		
			minionHitLogic()
		End If
		if (fortProgress(player) > 69 and fortProgress(player) < 100) Then				'Are we fighting the War Ghost?
			WarTrap()
		End If
		if (loopCatch = catchBall) Then													'Trying to catch the ball?
			loopCatch = checkBall														'Change state that we're checking to see if ball actually caught
			MagnetSet(255)																'Hold the ball.
			TargetTimerSet 1000, TargetUp, 2											'Put targets up quickly to catch ball. This is also how much time before we check again if the ball is actually there	
		End If
		if (barProgress(player) = 80) Then												'Ghost Whore multiball?
			Dim X
			'lightningStart(1)
			lightningStart(Int(5998/CycleAdjuster)-1)														'Lightning FX	
			ghostFlash(50)
			ghostAction = Int(20000/CycleAdjuster)										'Ghost whacked
			whoreJackpot = whoreJackpot + 1												'Increase jackpot number. First hit will make this 1
			modeTimer = Int(30000/CycleAdjuster)										'Set timer so a quote happens soon after the hit
			if (whoreJackpot < 10) Then													'Play the normal-ish ones for first 9 hits
				playSFX 0, "B", "0", 65 + random(9), 255								'Sound depends on jackpot progress
				video "B", "0", 64 + whoreJackpot, allowSmall, 35, 10					'Gets knocked closer and closer to the well	
				x = EVP_Jackpot(player) + (whoreJackpot * 75000)						'Calculate Current value of jackpot		
				AddScore(x)																'The more you hit her, the more you score!
				showValue x, 40, 1														'Flash the value onscreen
				if (whoreJackpot = 9) Then												'Is next one a SUPER JACKPOT?
					manualScore 0, EVP_Jackpot(player) + ((whoreJackpot + 1) * 250000)
				Else																	'Else, default value
					manualScore 0, EVP_Jackpot(player) + ((whoreJackpot + 1) * 75000)
				End If
			Else																		'10th is a SUPER but then resets
				if (adultMode) Then
					playSFX 0, "B", "0", 74 + random(6), 255							'Hope the kids are in bed!
				Else
					playSFX 0, "B", "0", "O", 255										'More tame Super Jackpot callout
				End If
				video "B", "0", "J", allowSmall, 35, 10									'At 10+, show SUPER JACPOT
				x = EVP_Jackpot(player) + (whoreJackpot * 250000)						'Current value				
				AddScore(x)																'The more you hit her, the more you score!
				showValue x, 40, 1														'Flash the value onscreen
				whoreJackpot = 0														'Gotta start over now
				manualScore 0, EVP_Jackpot(player) + ((whoreJackpot + 1) * 75000)		'Show value for reset "Next Jackpot"
			End If		
		End If
		if (hosProgress(player) = 10) Then												'Doctor Ghost Multiball?
			lightningStart(1)
			ghostFlash(50)
			AddScore(EVP_Jackpot(player))
			playSFX 0, "H", "9", random(8) + 65, 255									'Jackpot sounds!
			video "H", "9", random(2) + 65, allowSmall, 48, 200							'Left or right ball animations
			ghostAction = Int(20000/CycleAdjuster)										'Set WHACK routine.
			if (lightningGo = 0) Then													'If a lightning FX isn't currently going
				modeTimer = Int(60000/CycleAdjuster)									'set Mode Timer so we're less likely to override the next one
			End If
		End If
		if (hotProgress(player) = 35) Then												'Eligible for Hotel Jackpots?
			HotelJackpot()
			lightningStart(1)
		End If
		if (priProgress(player) = 20) Then												'Bashing Prison Ghost?
			lightningStart(1)
			PrisonJackpot()
		End If
	End If
End Sub

Sub TrSw24_UnHit()
	Sw24 = 0
End Sub

Sub TrSw28_Hit()
	if (hotelPathLogic()) Then						'Function says we can set a combo?
		if (DoorLocation = DoorOpen) Then			'Can shoot through the door?
			comboSet random(2), comboTimerStart		'Door VUK set as combo
			DOF 118, 2
		Else										'If not, prison path is the combo (flow isn't as good oh well)
			comboSet 0, comboTimerStart
		End If
	End If
End Sub

Sub WaSw29_Hit()
	'See if we can press it first
	DOF 123, 2
	if (hotProgress(player) <> 3) Then													'Hotel Mode not ready to start?
		callHits  = callHits  +  1														'Increment # of call hits
	End If
	if (callHits = hitsToLight(player) or multiBall > 0) Then							'Did we hit it enough, or is Multiball active?
		callHits  = callHits  -  1														'Decrement this so future hits will do this same action
		callButtonLogic()																'Move hellavator, if we can			
	Else
		if (hotProgress(player) <> 3) Then
			AddScore(10000)
			video "Q", "P", (hitsToLight(player) - callHits), allowSmall, 56, 240		'Pushing button in vain, how many hits are left		
			playSFX 2, "Q", "P", (hitsToLight(player) - callHits), 200					'Sound effect to match
		Else
			AddScore(10000)
			playSFX 2, "H", "0", "0", 100												'Door clunking sound
		End If
	End If
End Sub

Sub WaSw30_Hit()
	DOF 110, 2
'	playSFX 2, "S", "A", "Z", 250									'EP- None of the other targets have a sound here
	if (psychic(player) < 255) Then									'Can be advanced?
		psychic(player) = psychic(player) + 1		
	Else			
		if (scoringTimer) Then										'Double scoring active?
';			video "S", "P", "I", allowSmall, 0, 255					'Scoring TIME EXTEND
			playSFX 2, "S", "A", "Z", 250							'Replace with DOUBLE SCORING time extension prompt
			scoringTimer = scoringTimer + (3 * longSecond)			'Add about 3 seconds
			AddScore(10000)											'Some points for hitting inert target
		Else
			AddScore(5000)											'Some points for hitting inert target
			playSFX 2, "H", "0", "0", 250							'REJECT sound
';			video "S", "P", "L", allowSmall, 0, 255					'VIDEO THAT SAYS COMPLETE REST TO RE-LITE PSYCHIC	
		End If
	End If
	if (psychic(player) < 7) Then									'Not spelled yet?
		video "S", "P", 64 + psychic(player), allowSmall, 30, 250
		playSFX 2, "S", "A", "Z", 250							
		AddScore(25000)
	End If
	if (psychic(player) = 7) Then									'Psychic Spelled?
		video "S", "P", "G", allowSmall, 30, 255						'Video for it
		playSFX 2, "S", "B", "Z", 250								'Replace with DOUBLE SCORING sound
		scoringTimer = 20 * longSecond								'Set double scoring timer
		scoreMultiplier = 2											'Double scoring!
		animatePF 119, 30, 1										'Psychic Scoring light animation (loops)
		AddScore(200000)											'Double points for spelling the longer word		
		psychic(player) = 255										'Reset counter
		blink(51)													'Blink the Psychic light
		spiritGuidelight 											'If it needs to be re-lit, re-lite it
	End If
End Sub

Sub TrSw31_Hit()
'	switchDead = 0						'Since it's not a matrix switch, we set this manually EP- Disabling for now, it has to do with a ball search
	doorDo()
	If WaDoor.IsDropped = 0 Then DOF 113, 2
End Sub

Sub TrSw32_Hit()
	balconyJump()
	DOF 118, 2
	rampTimer = 0							'Reset ramp timer.
	comboSet 4, comboTimerstart
End Sub

sub TrSw33_Hit()
	If orbTimer=0 Then
		if (rampTimer = 0) Then								'Ball didn't roll back down from ramp?
			rampTimer = Int(16000/cycleAdjuster)			'About 1.5 second before the time out			
			ghostLooking(165)
			balconyApproach()	
		Else
			playSFX 1, "T", "9", "V", 200					'Run abort sound
			video "T", "9", "V", allowSmall, 30, 200			'Run abort animation
		End If
	End If
End Sub

Sub TrSw34_Hit()
	if (skillShot) Then										'We'll count this as a Pop Skill shot, if somehow ball slipped through the pops  unlikely, but possible			
		if (skillShot = 1) Then								'Did we hit the Skill shot?
			skillShotSuccess 1, 0							'Success!
			DOF 117, 2
		Else
			skillShotSuccess 0, 255							'Failure, so just disable it
		End If			
	End If	
		if (centerTimer = 0 and popsTimer = 0) Then			'Wasn't a weak shot up the middle, or just came down from the Pops?
			centerTimer = longSecond						'Set timer. This prevents roll-backs, or Pop Values overriding what center shot triggered
			if (rampTimer = 0) Then							'Not a jump fail? Normal switch actions below:
				comboCheck(2)								'Combos to itself
			centerPathCheck()								'See what to do. Defaults to hopefully satisfying lighting sound + FX
			comboSet 2, comboTimerStart
		Else												'Did a jump fail?
			'PERSON FALLING + SCREAM	
			video "T", "9", "Z", allowSmall, 34, 200	'Kaminski falling
			if (theProgress(player) < 10) Then				'Haven't started theater?
				playSFX 0, "T", "J", random(9) + 65, 240
			Else											'Fall but no theater prompt
				playSFX 0, "T", "H", random(9) + 65, 240
			End If
			rampTimer = 0									'Reset timer		
		End If
	End If
End Sub

Sub TrSw35_Hit()
	if (skillShot) Then			
		if (skillShot = 3) Then														'Did we hit the Skill shot?
			skillShotSuccess 1, 0													'Success!
			DOF 117, 2
		Else
			skillShotSuccess 0, 255													'Failure, show message (high priority)
		End If
	End If
	if (ghostLook = 1) Then															'Ghost "watches" ball go down subway	
		ghostLooking(165)
	End If
	if (Advance_Enable and priProgress(player) > 3 and priProgress(player) < 7) Then	'We've hit the left orbit a 4th time and are ready to lock ball?
		PrisonAdvance2()		
	End If
	Tunnel = 2
End Sub

Sub TrSw36_Hit()
	if (HellBall = 10) Then					'Came from Hellavator?
		ballExitElevatorLogic()				'Do logic for that
	End If
	if (Tunnel <> 2) Then					'Unless it came from upper entry...
		Tunnel = 1							'Set flag so scoop will just kick, not advance.
	End If
End Sub

Sub TrSw38_Hit()										'Left orbit UPPER switch hit?
	if (skillShot) Then									'Going for Skill Shot?			
		skillShotSuccess 0, 255							'Failure, so disable it		
	End If 	
	if (LeftOrbitTime) Then								'If lower target WAS hit first, we count this as a Left Orbit Shot. Prevents event from activating via launch or pop actions
		LeftOrbitTime = 0								'Clear it
		comboCheck(0)
		ghostLooking(10)
		leftOrbitLogic()
		comboSet 0, comboTimerStart						'Sets a combo to itself
	End If
End Sub

Sub TrSw39_Hit()										'Left orbit LOWER switch hit?
	if (skillShot) Then									'Going for Skill Shot?			
		skillShotSuccess 0, 255							'Failure, so disable it		
	End If 
	if (LeftOrbitTime = 0) Then							'Upper orbit was not hit first?
		LeftOrbitTime = Int(15000/CycleAdjuster)		'Set timer to indicate upper motion (going UP to Zero)				
	End If
End Sub

Sub TrSw41_Hit()
	if (skillShot) Then			
		if (skillShot = 2) Then							'Did we hit the Skill shot?
			skillShotSuccess 1, 0						'Success!
			DOF 117, 2
		Else
			skillShotSuccess 0, 0						'Failure, so just disable it
		End If
	End If
	orbTimer = Int(40000/CycleAdjuster)
'	if (orb(player) & B00100100) Then					'Already lit?
	If ((orb(player) AND 36) = 36) Then
		AddScore(10000)
	Else												'Not yet lit?
		AddScore(30000)
		DOF 114, 2
'		orb(player) |= B00100100						'Set bits
		orb(player) = orb(player) OR 36
		checkOrb(1)										'Set lights, with video update
	End If
End Sub

Sub TrSw42_Hit()						'"R"
	if (skillShot) Then
		if (skillShot = 2) Then			'Did we hit the Skill shot?
			skillShotSuccess 1, 0		'Success!
			DOF 117, 2
		else
			skillShotSuccess 0, 0		'Failure, so just disable it
		End If		
	end If
	orbTimer = Int(40000/CycleAdjuster)
	if ((orb(player) AND 18) = 18) Then	'Already lit?
		AddScore 10000
	else								'Not yet lit?
		AddScore 30000
		DOF 115, 2
'		orb(player) |= B00010010		'Set bits
		orb(player) = orb(player) OR 18
		checkOrb 1						'Set lights
	End If
End Sub

Sub TrSw40_Hit()						'"B"
	if (skillShot) Then
		if (skillShot = 2) Then			'Did we hit the Skill shot?
			skillShotSuccess 1, 0		'Success!
			DOF 117, 2
		else
			skillShotSuccess 0, 0		'Failure, so just disable it
		End If		
	end If
	orbTimer = Int(40000/CycleAdjuster)
	if ((orb(player) AND 9) = 9) Then		'Already lit?
		AddScore 10000
	else								'Not yet lit?
		AddScore 30000
		DOF 116, 2
'		orb(player) |= B00001001		'Set bits
		orb(player) = orb(player) OR 9
		checkOrb 1						'Set lights
	End If
End Sub

Sub KiHellEvator_Hit()						'Is there a ball in the Hellavator?  Switch 43
	Set BallElevator = ActiveBall
	ballElevatorLogic()
	BallInElevator = 1
End Sub

Sub TrSw48_hit()
	DOF 127, 2
	playSFX 2, "F", "1", "K", 205		'Always a "bad" sound. Priority will override any "good" completion sound
'	rollOvers(player) |= B10001000		'Add bit
	rollOvers(player) = rollOvers(player) OR 136
	AddScore(10000)
	checkRoll()							'Puts the bit on the lamp and checks if full
End Sub

Sub TrSw49_Hit()
	DOF 128, 2
	if (badExit) Then						'If BadExit was set, clear it (checks that VUK'd ball rolled down the habitrail)
		badExit = 0
	End If
'	if ((rollOvers(player) & B01000100) = 0) Then	'Not already lit?
	If ((rollOvers(player) AND 68) = 0) Then
		playSFX 2, "F", "1", "0", 205		'Low priority sound
	Else
		playSFX 2, "F", "1", "L", 205		'Rollover when already lit sound FX (reduced version of normal)						
	End If		
'	rollOvers(player) |= B01000100			Add bit
	rollOvers(player) = rollOvers(player) OR 68
	AddScore(5000)
	checkRoll()								'Puts the bit on the lamp and checks if full
End Sub

Sub TrSw54_Hit()
	DOF 129, 2
'	if ((rollOvers(player) & B00100010) = 0) Then	'Not already lit?
	If (((rollOvers(player) AND 34) = 34)) Then
		playSFX 2, "F", "1", "0", 205				'Low priority sound
	Else
		playSFX 2, "F", "1", "L", 205				'Rollover when already lit sound FX (reduced version of normal)						
	End If		
'	rollOvers(player) |= B00100010					'Add
	rollOvers(player) = rollOvers(player) OR 34
	AddScore(5000)
	checkRoll()										'Puts the bit on the lamp and checks if full
End Sub

Sub TrSw55_Hit()
	DOF 130, 2
	playSFX 2, "F", "1", "K", 205					'Always a "bad" sound. Priority will override any "good" completion sound
'	rollOvers(player) |= B00010001					'Add bit
	rollOvers(player) = rollOvers(player) OR 17
	AddScore(10000)
	checkRoll()										'Puts the bit on the lamp and checks if full
End Sub

Dim BallMoverHold
Sub TrSw57_Hit()
	Sw57 = 1
	If (BallMoverHold Is Nothing) Then Set BallMoverHold = ActiveBall
	if (run = 3 and plungeTimer = 0) Then							'The ball has started? Check conditions
			if (skillShot > 0) Then									'Did it fall back after shitty skill shot attempt? Give player greif!
				'Serial.println("SKILL FAIL RUN=2")
				run = 2												'Reset condition
				if (launchCounter > 1) Then							'A couple failed attempts?				
					if (numPlayers = 1) Then						'In single player games, do not indicate Player #
						playSFX 0, "S", "H", random(8), 255			'Give player shit
					Else											'Multiplayer, show which player is up and has the skill shot
						playSFX 0, "S", "I", random(8), 255			'Give player shit
					End If
					launchCounter = 0
				End If				
			Else													'Ball was launched, it bounced back here somehow. Kick it out!
				'Serial.println("On shooter lane during game KICK (Run=3)")
'				Coil(Plunger, plungerStrength)
				AutoPlunger.AutoFire
			End If
		End If
End Sub

Sub TrSw57_UnHit()
	Sw57 = 0
End Sub
'*************************** End Switches **************************

Sub TargetBankTargetsHit()											'Any of the 3 targets hit?
	DOF 108, 2
																	'Some modes don't require you to be specific									
	if (minion(player) = 1 and minionsBeat(player) < 3) Then		'First 3 minions, hit any target 3 times to reveal
		minionHits = minionHits - 1
		flashCab 0, 0, 255, 50										'Brief flash to blue
		if (minionHits > 0) Then									'Haven't made 3 hits yet?
			AddScore(10000)					
			video "M", "H", minionHits, allowSmall, 40, 210			'Show how many hits we need to find minion
			if (minionHits = 2) Then								'Make lights solid to count how many we've hit
				'pulse(17)											'Apparently this was confusing, so just keep pulsing them I guess?
				'pulse(18)
				'light 19, 7
				playSFX 2, "M", "I", "0", 250						'Minion target SFX
			End If
			if (minionHits = 1) Then
				'pulse(17)
				'light 18, 7
				'light 19, 7
				playSFX 2, "M", "I", "1", 250						'Minion target SFX
			End If		
			ghostAction = Int(509998/cycleadjuster)				'Slight ghost movement					
		Else
			light 17, 7
			light 18, 7
			light 19, 7
			minionStart()				
		End If
	End If
	if (hotProgress(player) = 20) Then	'Looking for control box?
		modeTimer = 0												'Hit ghost for random taunt
		playSFX 0, "L", "5", 65 + random(22), 200					'Will not override advance dialog
		video "L", "5", "A", allowSmall, 60, 100					'Will not override video				
		AddScore(10000)
	End If
	if (Mode(player) = 1) Then										'Are we distracting Ghost Doctor? If so, we don't care which switch was hit.
		HospitalSwitchCheck()
	End If	
	if (theProgress(player) > 9 and theProgress(player) < 100) Then	'Doing the Theater Ghost play?
		countSeconds = countSeconds + 5								'Increase 5 seconds
		modeTimer = Int(40000/CycleAdjuster)						'Reset seconds countdown timer
		playSFX 0, "T", "0", 65 + random(8), 250					'Will not override advance dialog				
		if (countSeconds > TheaterTime) Then
			countSeconds = TheaterTime
			video "T", "4", "F", allowSmall, 54, 255				'Ghost talking TIMER MAXED OUT					
		Else
			video "T", "4", "G", allowSmall, 54, 255				'Ghost talking TIMER ADD 5 SECONDS
		End If
';		numbers(0, numberStay OR 4, 0, 0, countSeconds - 1)			'Update numbers station
		shotValue = (10000 * countSeconds) + 500000					'Recalculate shot value
';		numbers(9, 2, 70, 27, shotValue)							'Update Shot Value				
		AddScore(10000)
		sweetJumpBonus = 0											'BUT it resets your Sweet Jumps meter!
		sweetJump = 0
	End If
	if (minionMB = 10) Then											'Ball trapped there for Minion Multiball?
		minionMBjackpot(0)											'Score jackpot, release ball
'		cabDebounce(ghostOpto) = 10000								'Temporarily set it higher so ball behind targets won't re-trigger opto when it bounces up
	End If
	if (deProgress(player) = 10) Then								'Haven't weakened the demon yet?
		playSFX 0, "D", "Z", 65 + random(15), 255					'Prompt that we can't hit demon yet
		video "D", "D", "I", noExitFlush, 60, 255					'Shoot flashing shots!
		DemonState()		
	End If		
End Sub

'**************
'* SlingShots
'**************
Sub WaSlingLeft_Slingshot()
	Leftsling = True
	PlaySound Soundfx("left_slingshot")
	DOF 103, 2
	slingCount = slingCount + 1
	if (slingCount > 6) Then
		playSFX 2, "C", "B", 65 + random(21), 100				'Ghost wail + Team Dialog
		AddScore(20000)
		slingCount = 0
	Else
		playSFX 2, "C", "A", 65 + random(14), 99				'Low priority ghost wail
		AddScore(5000)
	End If
End Sub

Dim Leftsling:Leftsling = False

Sub LS_Timer()
	If Leftsling = True and Left1.ObjRotZ < -11 then Left1.ObjRotZ = Left1.ObjRotZ + 2
	If Leftsling = False and Left1.ObjRotZ > -24 then Left1.ObjRotZ = Left1.ObjRotZ - 2
	If Left1.ObjRotZ >= -7 then Leftsling = False
	If Leftsling = True and Left2.ObjRotZ > -216.5 then Left2.ObjRotZ = Left2.ObjRotZ - 2
	If Leftsling = False and Left2.ObjRotZ < -203.5 then Left2.ObjRotZ = Left2.ObjRotZ + 2
	If Left2.ObjRotZ <= -212.5 then Leftsling = False
	If Leftsling = True and Left3.TransZ > -23 then Left3.TransZ = Left3.TransZ - 4
	If Leftsling = False and Left3.TransZ < -0 then Left3.TransZ = Left3.TransZ + 4
	If Left3.TransZ <= -23 then Leftsling = False
End Sub

'Sub LeftSlingShot_Timer:Me.TimerEnabled = 0:End Sub

Sub WaSlingRight_Slingshot()
	Rightsling = True
	PlaySound Soundfx("right_slingshot")
	DOF 104, 2
	slingCount = slingCount + 1
	if (slingCount > 6) Then
		playSFX 2, "C", "B", 65 + random(21), 100				'Ghost wail + Team Dialog
		AddScore(20000)
		slingCount = 0
	Else
		playSFX 2, "C", "A", 65 + random(14), 99				'Low priority ghost wail
		AddScore(5000)			
	End If
End Sub

Dim Rightsling:Rightsling = False

Sub RS_Timer()
	If Rightsling = True and Right1.ObjRotZ > 11 then Right1.ObjRotZ = Right1.ObjRotZ - 2
	If Rightsling = False and Right1.ObjRotZ < 24 then Right1.ObjRotZ = Right1.ObjRotZ + 2
	If Right1.ObjRotZ <= 7 then Rightsling = False
	If Rightsling = True and Right2.ObjRotZ < 216.5 then Right2.ObjRotZ = Right2.ObjRotZ + 2
	If Rightsling = False and Right2.ObjRotZ > 203.5 then Right2.ObjRotZ = Right2.ObjRotZ - 2
	If Right2.ObjRotZ >= 212.5 then Rightsling = False
	If Rightsling = True and Right3.TransZ > -23 then Right3.TransZ = Right3.TransZ - 4
	If Rightsling = False and Right3.TransZ < -0 then Right3.TransZ = Right3.TransZ + 4
	If Right3.TransZ <= -23 then Rightsling = False
End Sub
'**************
'**************
'* Bumpers
'**************
Sub Bumpers_Hit(X)
	if X = 1 Then 
		DOF 105, 2
	elseif X = 0 Then 
		DOF 106, 2
	elseif X = 2 Then 
		DOF 107, 2
	end if
	popsTimer = longSecond	 			'30000					Set pops timer so ball doesn't trigger center shot if rolls down there
	popCheck()							'Check the pops!
	Playsound Soundfx("fx_bumper1")
End Sub

Set BallElevator = Nothing
Sub TiElevator_Timer()
	If (PrElevator.Z < HellTarget AND ElevatorDir = 1) OR (PrElevator.Z > HellTarget AND ElevatorDir = -1) Then
	DOF 112, 2
	End If
	If PrElevator.Z > HellTarget Then ElevatorDir = -1
	If PrElevator.Z < HellTarget Then ElevatorDir = 1
	PrElevator.Z = (PrElevator.Z + (HellSpeed * ElevatorDir))
	If PrElevator.Z > 40 Then KiHellEvator.Enabled = 1 Else KiHellEvator.Enabled = 0
	If PrElevator.Z >= HellTarget AND ElevatorDir = 1 Then me.Enabled = 0:PrElevator.Z = HellTarget
	If PrElevator.Z <= HellTarget AND ElevatorDir = -1 Then me.Enabled = 0:PrElevator.Z = HellTarget
	If Not (BallElevator Is Nothing) Then
		BallElevator.Z = PrElevator.Z + 2
		If PrElevator.Z <= HellDown + 2 Then
			BallElevator.X = kiTroughEnter2.X
			BallElevator.Y = kiTroughEnter2.Y
			BallElevator.Z = 100
			KiHellEvator.kick 0, 0, 0
			DOF 120, 2
			Set BallElevator = nothing
		End If
		If PrElevator.Z = HellUp Then KiHellEvator.kick 0, 0, 0
	End If
	HellLocation = Int(PrElevator.Z)
End Sub

Sub UpdateFlipperLogo_Timer
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
	PRGhost1.ObjRotZ = PrGhost.ObjRotZ
	PRDoor1.ObjRotZ = PrDoor.ObjRotZ
	dim bulb
	if NightDay <= 15 then
		for each bulb in GI_Lights1
			bulb.intensity = 12
		next
	end if
	if NightDay <= 15 then
		for each bulb in GI_Lights2
			bulb.intensity = 15
		next
	end if
	if NightDay <= 15 then
		for each bulb in GI_Lights3
			bulb.intensity = 15
		next
	end if
	if NightDay > 15 then
		for each bulb in GI_Lights1
			bulb.intensity = 8
		next
	end if
	if NightDay > 15 then
		for each bulb in GI_Lights2
			bulb.intensity = 8
		next
	end if
	if NightDay > 15 then
		for each bulb in GI_Lights3
			bulb.intensity = 8
		next
	end if
    Li40a.State = Li40.State
	Li41a.State = Li41.State
	Li42a.State = Li42.State
	Li40b.Intensity = Li40.Intensity * 5
	Li41b.Intensity = Li41.Intensity * 5
	Li42b.Intensity = Li42.Intensity * 5
	Li40a.Intensity = Li40.Intensity * 5
	Li41a.Intensity = Li41.Intensity * 5
	Li42a.Intensity = Li42.Intensity * 5

End Sub

Sub T1_hit: l1.state=1:end sub
Sub T1_unhit: l1.state=0:end sub
Sub T2_hit: l2.state=1:end sub
Sub T2_unhit: l2.state=0:end sub
Sub T3_hit: l3.state=1:end sub
Sub T3_unhit: l3.state=0:end sub

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function Dec2Bi(DecimalNum)
	Dim tmp
	Dim n
    n = Cstr(DecimalNum)
    tmp = n Mod 2
    n = n \ 2
    Do While n <> 0
        tmp = Cstr(n Mod 2) & tmp
        n = n \ 2
    Loop
	Do while len(tmp)<24
		tmp = "0" & tmp
	Loop
    Dec2Bi = tmp
End Function

Public Function Bi2Dec(Binary)
	Dim n
	Dim s
    For s = 1 To Len(Binary)
        n = n + (Mid(Binary, Len(Binary) - s + 1, 1) * (2 ^ (s - 1)))
    Next
    Bi2Dec = n
End Function

Sub WaGhost_Timer()
	GhostLocation = -(Int(PrGhost.ObjRotZ)-90)
	If PrGhost.ObjRotZ > GhostTarget Then GhostDir = -1
	If PrGhost.ObjRotZ < GhostTarget Then GhostDir = 1
	PrGhost.ObjRotZ = (PrGhost.ObjRotZ + (GhostSpeed * GhostDir))
	If PrGhost.ObjRotZ >= GhostTarget AND GhostDir = 1 Then
		WaGhost.TimerEnabled = 0
		PrGhost.ObjRotZ = GhostTarget
	End If
	If PrGhost.ObjRotZ <= GhostTarget AND GhostDir = -1 Then
		WaGhost.TimerEnabled = 0
		PrGhost.ObjRotZ = GhostTarget
	End If
End Sub

Sub WaDoor_Timer()
	If (PrDoor.ObjRotZ < DoorTarget AND DoorDir = 1) OR (PrDoor.ObjRotZ > doortarget AND DoorDir = -1) Then
		DOF 112, 2
	End If
	If PrDoor.ObjRotz > doortarget Then DoorDir = -1
	If PrDoor.ObjRotZ < doortarget Then DoorDir = 1
	PrDoor.ObjRotZ = (PrDoor.ObjRotZ + (DoorSpeed * Doordir))
	If PrDoor.ObjRotZ >= doortarget AND DoorDir = 1 Then
		WaDoor.TimerEnabled = 0
		PrDoor.ObjRotZ = doortarget
	End If
	If PrDoor.ObjRotZ <= doortarget AND DoorDir = -1 Then
		WaDoor.TimerEnabled = 0
		PrDoor.ObjRotZ = doortarget
	End If
	If PrDoor.ObjRotZ <= 60 Then WaDoor.IsDropped = 0 Else WaDoor.IsDropped = 1
End Sub

Sub WaGhostTarget_Timer()											'EP- Sub to move the primitive
	If (PrBankTop.Z < TargetLocation AND BankDir = 1) OR (PrBankTop.Z > TargetLocation AND BankDir = -1) Then
		DOF 112, 2
	End If
	Dim X, prim
	If PrBankTop.Z > TargetLocation Then BankDir = -1
	If PrBankTop.Z < TargetLocation Then BankDir = 1
	For Each prim in TargetBank
		prim.Z = prim.Z + (TargetSpeed * BankDir)
	Next
	If PrBankTop.Z <= TargetLocation AND BankDir = -1 Then
		WaGhostTarget.TimerEnabled = 0
		For Each prim in TargetBank
			prim.Z = TargetLocation
		Next
	End If
	If PrBankTop.Z <= 3 Then
		For Each X in TargetBankWalls
			X.IsDropped = 1
		Next
	End If
	If PrBankTop.Z >= 3 Then
		For Each X in TargetBankWalls
			X.IsDropped = 0
		Next
	End If
	If PrBankTop.Z >= TargetLocation AND BankDir = 1 Then
		WaGhostTarget.TimerEnabled = 0
		For Each prim in TargetBank
			prim.Z = TargetLocation
		Next
	End If
End Sub

Sub TrAutoPlunger_Hit()
	AutoPlunger.AddBall
End Sub

Sub TrAutoPlunger_UnHit()
	AutoPlunger.RemoveBall
End Sub

Sub MusicPlayer(song)
	EndMusic
	lastMusic(1) = CurrentMusic(1)
	PlayMusic song
	CurrentMusic(1) = song
End Sub

Sub PlayMusicOnce(clip1, clip2)
	Dim X
	X = "bgout_" & clip1 & clip2 & ".mp3"
	PlayMusic X
	RepeatMusic(0)
	MusicRestart = 1
End Sub

Sub Table1_MusicDone()
	If MusicRepeats = 1 Then
		MusicPlayer CurrentMusic(1)
	End If
	If MusicRestart = 1 Then
		MusicPlayer lastMusic(1)
		MusicRestart = 0
		RepeatMusic(1)
	End If
End Sub

'*********************
'*Kicker and trough stuff
'*********************
Sub KiDrain_Hit()										'Switch 63
	If ActiveBall.ID > 20 Then me.destroyball:Exit Sub:End If
	Playsound "drain"
	Set BallMover3 = ActiveBall							'Set the object so that we can...
	MoveBall BallMover3, KiMaintrough, me, 58, 8, 0		'Move the ball, to the main trough, coming from the drain
	BallsInTrough = BallsInTrough + 1					'Increase the number of tracked balls in the main trough
	Drain 0
End Sub

Sub KiMainTrough1_Hit()									'When the bottom most kicker gets hit...
	Set BallMover4 = ActiveBall							'Set the Object so that we can move the ball when the ServeBall Sub gets called
	me.UserValue = 1									'Set me to occupied
	Sw59 = 1
End Sub

Sub KiMainTrough2_Hit()									'For the rest of the kickers, when they get hit, we'll...
	me.UserValue = 1									' set it to occupied and...
	Sw60 = 1
	If MainTrough(0).Uservalue = 0 Then me.kick 58, 8, 0:me.UserValue = 0:Sw60 = 0	'If the next kicker in line is empty, then kick the ball out of me and set me to empty
End Sub

Sub KiMainTrough3_Hit()
	me.UserValue = 1
	Sw61 = 1
	If MainTrough(1).Uservalue = 0 Then me.kick 58, 8, 0:me.UserValue = 0:Sw61 = 0
End Sub

Sub KiMainTrough4_Hit()
	me.UserValue = 1
	Sw62 = 1
	If MainTrough(2).Uservalue = 0 Then me.kick 58, 8, 0:me.UserValue = 0:Sw62 = 0
End Sub

Dim BallMover, BallMover2, BallMover3, BallMover4
Sub KiBasement1_Hit()											'If the ball hits either of the basement entrances (behind the Hellevator)...
	Set BallMover = ActiveBall									'Set the object so we can move it and...
	MoveBall BallMover, KiTroughEnter, KiBasement1, 0, 0, 0		'Move the ball, to the Basement trough entrance, coming from me
	Playsound SoundFX("kicker_enter_center")
End Sub

Sub KiBasement2_Hit()											'Same as previous
	Set BallMover = ActiveBall
	MoveBall BallMover, KiTroughEnter, KiBasement2, 0, 0, 0
	Playsound SoundFX("kicker_enter_center")
End Sub

Sub KiVUK1_Hit()												'Switch 21
	Set BallMover = ActiveBall									'When the basement exit/VUK gets hit, set the Object so we can...
	MoveBall BallMover, KiVUK4, me, 0, 0, 0						'Move it back into the Basement trough, coming from me
	Sw22 = 1
	Playsound SoundFX("kicker_enter_center")
End Sub

Sub KiVUK3_Hit()												'Switch 22
	Set BallMover2 = ActiveBall									'When the bottom of the basement trough gets hit, set the Object so we can...
	ScoopTime = Int(9010/CycleAdjuster)							'The default. Can be changed by the following:
		if (Tunnel = 1)	Then			 						'Did ball get to the tunnel from the Hellavator?
			if (hellMB = 1) Then
				ScoopTime = Int(32500/CycleAdjuster)			'Sync to music and stuff. Re-test on the real, metal subway at Chuck's			
			End If
			if (theProgress(player) = 11) Then					'If ball rolled down from hellavator, remove that Skip Event
			 skip = 0
			End If
			if (hotProgress(player) = 15) Then 					'Did we just start Control Box Search?
				ScoopTime = Int(80000/CycleAdjuster)			 					'Kick it out, after a longer delay
				hotProgress(player) = 20						'Set flag that ball is out and can find Control Boxes!
				skip = 55										'Set skip event for ball scoop eject
			End If
			if (deProgress(player) = 8) Then					'Ready to start DEMON BATTLE?
				DemonStart()
			End If
		End If
		if (Tunnel = 2) Then									'Ball came down from rear? (not hellavator)
			if (Advance_Enable and priProgress(player) > 4 and priProgress(player) < 7) Then		'First 2 locks?
				if (priProgress(player) = 6) Then				'Second ball lock has shorted speech
					ScoopTime = Int(80000/CycleAdjuster)
				Else
					ScoopTime = Int(85000/CycleAdjuster)
				End If
			End If
			if (priProgress(player) = 9) Then					'Did we lock the third ball down through upper Basement subway?
				ScoopTime = Int(130000/CycleAdjuster)			'Delay for storytelling
				skip = 60										'Allow a skip once the ball is in position
			End If
		End If
		if (Tunnel = 0) Then									'Was ball just shot right into Basement?		
			ghostLooking(120)
			scoopDo()		
		End If
End Sub

Sub KiVUK1_Timer()										'Re-enable the VUK
	KiVUK1.Enabled = 1
	me.timerEnabled = 0
End Sub

Sub KiDoor_Hit()									'Switch 23
	Sw23 = 1
	if (trapDoor = 0) Then							'Ball isn't supposed to be trapped behind door? Then check the switch! (This prevents switch from counting during Ball Search)
		if (LeftTimer = 0) Then						'ball goes into VUK behind door?		
			if (leftVUKlogic() = 1) Then			'Call function. If it returns a 1, we are allowed to set a new combo
				if (HellLocation = hellDown) Then	'Only light the combo if the Hotel	Path is open
					comboSet 3, comboTimerStart		'Enable a combo at Hotel Path
				Else
					comboSet 4, comboTimerStart		'Else, Theater Path
				End If				
			End If				
		End If
	End If
	if (hosTrapCheck = 1) Then
		if (activeBalls > 1 and LeftTimer = 0) Then	'Ball back in VUK, and we still have 2+ balls active?
			activeBalls = activeBalls - 1			'Subtract the ball we just caught
			hosTrapCheck = 0						'Clear the flag
			DoorSet DoorClosed, 1					'Close the door and continue as normal			
		End If
	End If
End Sub

Sub KiDoor_UnHit()
	Sw23 = 0
	Playsound SoundFX("ballrelease")
	DOF 121, 2
End Sub

Sub KiDoor_Timer()
	me.kick -100, 50, 90
	me.TimerEnabled = 0
End Sub

Sub VUKKicker(whichkicker, power)
	whichkicker.kick -100, power, 85
End Sub

Sub MoveBall(ball, dest, source, S1, S2, S3)						'The precious ball mover routine that moves the Ball, to the Dest, and making sure the Source is what kicks it
	ball.X = dest.X
	ball.Y = dest.Y
	source.kick S1, S2, S3
End Sub
'********* End kicker stuff

Sub TiChannel0_Timer()
	me.enabled = 0
	SndChannel(0) = 0
	SndPlaying(0) = ""
End Sub

Sub TiChannel1_Timer()
	me.enabled = 0
	SndChannel(1) = 0
	SndPlaying(1) = ""
End Sub

Sub TiChannel2_Timer()
	me.enabled = 0
	SndChannel(2) = 0
	SndPlaying(2) = ""
End Sub

Sub TiChannel3_Timer()
	me.enabled = 0
	SndChannel(3) = 0
	SndPlaying(3) = ""
End Sub

Sub TiChannel4_Timer()
	me.enabled = 0
	SndChannel(4) = 0
	SndPlaying(4) = ""
End Sub

Sub TiChannel5_Timer()
	me.enabled = 0
	SndChannel(5) = 0
	SndPlaying(5) = ""
End Sub

Sub TiChannel6_Timer()
	me.enabled = 0
	SndChannel(6) = 0
	SndPlaying(6) = ""
End Sub

Sub TiChannel7_Timer()
	me.enabled = 0
	SndChannel(7) = 0
	SndPlaying(7) = ""
End Sub

Function Random(howrandom)
	Random = Int(Rnd*howrandom)
End Function

Sub CoinDoorOpenClose()									'EP- what to do when the user opens the door (i.e. pushes the End key)
	if (CoinDoorstate = 1) Then							'Is door open?
		video "A", "T", "Z", 0, 45, 100					'Play video
		DMDSceneQ "", MenuItem, 15, "", -1, 14, 16665, 14
		playSFX 2, "X", "X", "8", 255
	End If			
	if (CoinDoorstate = -1) Then						'Is door closed?
		playSFX 2, "X", "X", "9", 255
		DMDScene "AT0 Rev 2.gif", "", 0, "", 0, 14, 183, 14, 100
	End If
End Sub

Function ShiftLeft(Value, n)
	Dim Tmp
	Dim i
	Tmp = Value
	For i = 0 To n - 1
		Tmp = Tmp * 2
	Next
	ShiftLeft = CInt(Tmp)
End Function

Function ShiftRight(Value, n)
	Dim Tmp
	Dim i
	Tmp = Value
	For i = 0 To n - 1
		Tmp = Tmp \ 2
	Next
	ShiftRight = CInt(Tmp)
End Function

Sub RFlip(dir)
	if (AutoEnable) Then '& EnableFlippers) Then									'Flippers available? Then allow player to activate them. EP- EnableFlippers is a constant, I don't understand why you have to check if a constant is there... IT'S A CONSTANT
		if (skip) Then 																'and flipperCheck) Then									'Was either flipper hit during a skippable animation?							
			skippable()																'Check what to skip to!
		End If
		If dir = 1 Then
			RightFlipper.RotateToEnd
			PlaySound SoundFX("flipperupright"),0,1,0.1,0.25
			DOF 102, 1
			RollRight()
			If SkillShot Then
				If Sw57 = 1 Then
					skillShot = skillShot + 1
					if (skillShot > 3) Then
						skillShot = 1
					End If
					video "K", "9", "9", 0, 5, 255									'Static transition shot
					if (numPlayers = 1) Then										'In single player games, do not indicate Player #	
						customScore "K", "0", 64 + skillShot, 5, 999999				'Custom Score for skill shot
					Else															'Multiplayer, show which player is up and has the skill shot
						customScore "K", player, 64 + skillShot, 5, 999999			'Custom Score for skill shot
					End If
					PlaySFX 1, "S", "9", "9", 255									'Static sound
				End If
			End If
		Else
			RightFlipper.RotateToStart
			PlaySound SoundFX("FlipperDown"),0,1,0.1,0.25
			DOF 102, 0
		End If
	End If
	If dir = 1 Then
		If (cursorPos <> 50) Then
			playSFX 1, "O", "R", "W", 255
			modeTimer = Int(375000/cycleAdjuster)
			inChar = inChar + 1
			if (inChar > 92) Then
				inChar = 65
			End If
			NameEntry HSCheck, HSPlace
		End If
		If DMDAttract > 0 Then
			If CurAttract < 4 Then
				UltraDMD.CancelRendering
				DMDAttract = 4
			Else
				UltraDMD.CancelRendering
				DMDAttract = CurAttract + 1
				If DMDAttract > 8 Then DMDAttract = 4
			End If
		End If
	End If
	SwRFlip = dir
End Sub

Sub LFlip(dir)
	if (AutoEnable) Then '& EnableFlippers) Then									'Flippers available? Then allow player to activate them. EP- EnableFlippers is a constant, I don't understand why you have to check if a constant is there... IT'S A CONSTANT
		if (skip) Then 														'and flipperCheck) Then									'Was either flipper hit during a skippable animation?							
			skippable()																'Check what to skip to!
		End If
		If dir = 1 Then
			LeftFlipper.RotateToEnd
			PlaySound SoundFX("flipperupleft"),0,1,-0.1,0.25
			DOF 101, 1
			RollLeft()
			If SkillShot Then
				If Sw57 = 1 Then
					skillShot = skillShot - 1
					if (skillShot < 1) Then
						skillShot = 3
					End If
					video "K", "9", "9", 0, 2, 255									'Static transition shot
					if (numPlayers = 1) Then										'In single player games, do not indicate Player #	
						customScore "K", "0", 64 + skillShot, 5, 999999				'Custom Score for skill shot
					Else															'Multiplayer, show which player is up and has the skill shot
						customScore "K", player, 64 + skillShot, 5, 999999			'Custom Score for skill shot
					End If
					PlaySFX 1, "S", "9", "9", 255									'Static sound
				End If
			End If
		Else
			LeftFlipper.RotateToStart
			PlaySound SoundFX("FlipperDown"),0,1,-0.1,0.25
			DOF 101, 0
		End If
	End If
	If dir = 1 Then
		If (cursorPos <> 50) Then
			playSFX 1, "O", "R", "W", 255
			modeTimer = Int(375000/cycleAdjuster)
			inChar = inChar - 1
			if (inChar < 65) Then
				inChar = 92
			End If
			NameEntry HSCheck, HSPlace
		End If
		If DMDAttract > 0 Then
			Debug.Print "Going to HS"
			If CurAttract < 4 Then
				UltraDMD.CancelRendering
				DMDAttract = 4
			Else
				UltraDMD.CancelRendering
				DMDAttract = CurAttract - 1
				If DMDAttract < 4 Then DMDAttract = 8
			End If
		End If
	End If
	SwLFlip = dir
End Sub

Sub CabCoin()												'EP- Broke this out as this is event driven
	coinsIn = coinsIn + 1
	coinsInserted = coinsInserted + 1						'Master counter for moolah!
	if (coinsIn = coinsPerCredit) Then
		coinsIn = 0
		playSFX 0, "C", "B", 65 + random(20), 255			'Ghost wail + Team Dialog			
		credits = credits + 1
		DOF 126, 1
		if (credits > 99) Then
			credits = 99									'Once I would have asked "why would anyone try this?" but now I know better
		End If			
'		if (runType) Then
'			Update(0)										'Updates freeplay and coins.		
'		Else
'			Update(1)										'Updates freeplay and coins, attract mode to PRESS START!
'		End If			
	Else
		playSFX 0, "C", "A", 65 + random(12), 255			'Just a ghost wail
	End If
End Sub

Sub CabStart(runType)										'EP- Broke this out as this is event driven
	if (runType) Then										'Game running already? Have at least started Player 1?
		'if (ball = 1 and numPlayers < 4) Then				'Can we add a player?
		if (ball = 1) Then									'Can only add players on Ball 1
			if (freePlay = 0) Then							'Not on freeplay?
				if (credits) Then							'Then we need a credit
					credits = credits - 1
					addPlayer()
					Update(0)
				End If
			Else											'If on freeplay, go for it!
				addPlayer()									'Add player will handle past 4 players
				Update(0)									'Update credits					
			End If
		End If
	Else													'Game wasn't running? Start of the game with Player 1
		if (countBalls() = 4) Then							'Must have 4 balls to start
			if (freePlay = 0) Then							'Not on freeplay?
				if (credits) Then							'Then we need a credit
					credits = credits - 1
					If Credits < 1 Then DOF 126, 0
					run = 1									'Set condition to advance game
'					Update(0)								'Turn off attract mode
					AttractLights = 0
				End If
			Else											'If on freeplay, go for it!
				run = 1										'Set condition to advance game
'				Update(0)									'Turn off attract mode
				AttractLights = 0
			End If					
		Else
			video "A", "B", "0" + (4 - countBalls()), 0, 60, 255		'LOAD 1-4 MORE BALLS
			playSFX 2, "H", "0", "0", 255							'Door clunking sound					
		End If	
	End If
End Sub

Sub ServeBall()
	Dim X
	If Not (BallMover4 is Nothing) Then				'Only move the ball if the object has been correctly set (to reduce errors)
		BallMover4.X = KiLaunch.X					'EP- Move the ball to the launcher location
		BallMover4.Y = KiLaunch.Y
		KiMainTrough1.Kick 58, 10, 0				'EP- Kick it, but remember that KiMainTrough1 still owns the ball, so he's the one that has to kick it
		KiMainTrough1.UserValue = 0
		Playsound SoundFX("ballrelease")
		DOF 119, 2
		Set BallMover4 = Nothing
		BallsInTrough = BallsInTrough - 1			'Reduce number of balls in trough
	Else
'		EP- Play kicking sound without a ball... shouldn't ever happen, but you never know
	End If
	For Each X in MainTrough						'Kick all the balls in the trough
		X.Kick 0, 5, 0
		X.UserValue = 0
	Next
	Sw59 = 0										'EP- Set all the switches to 0 while they move to the next kicker position
	Sw60 = 0
	Sw61 = 0
	Sw62 = 0
End Sub

'*****************************************************
'* Variables
'*****************************************************
'America's Most Haunted
'Variables and Constants Definitions
Const CycleAdjuster = 	10

Const cycleMilliSecond=	12
Const cycleSecond=		1000	'12000				'How many kernel cycles per second
Const cycleSecond2=    	2000	'24000
Const cycleSecond4=    	4000	'48000
Const longSecond=		1250	'15000				'How many cycles appx a "long" second for timers

Const popScore=			10000
Const advanceScore=		50000
Const comboScore=		75000
Const startScore=		250000
Const winScore=			1000000
Const loopSecondsAdd=	3							'How many seconds you gain in Photo Hunt by shooting the loop

'Modify NumberType
Const numberScore=		64	'"01000000"				'Draws number as Player (Number Value's) score. Use to build custom score displays 
Const numberFlash=		32	'"00100000"
Const numberStay=		16	'"00010000"
Const returnPixels=		32	'"00100000"				'Before drawing this character, place the existing left and rightmost pixels in the Outbuffer data return buffer 

'Modify Video Type
Const loopVideo=		128	'"10000000"				'Should video start over after it ends?
Const preventRestart=	64	'"01000000"				'If video called is already playing, don't restart it (just let it keep playing)
Const noEntryFlush=		32	'"00100000"				'Do not flush graphics on video start (for instance, to have a number appear on an enqueued video)
Const noExitFlush=		16	'"00010000"				'Do not flush graphics on video end
Const allowBar=			4	'"00000100"				'Can show Progess Bar during a video?
Const allowLarge=		2	'"00000010"              'Can show large numbers on the video?
Const allowSmall=		1	'"00000001"				'Can show small numbers on the video?
Const allowAll=			3	'"00000011"				'Allow both large and small numbers on the video
Const manualStep=		8	'"00001000"				'Video frames must be advanced manually

'Graphic Mode Commands
Const clearScreen=		128	'"10000000"				'Erase buffer
Const loadScreen=		64	'"01000000"				'Load buffer dimo display memory

'Attract Mode Starting Podims
Const highScoreTable=	2							'Jump to High Score Table in attract mode
Const lastGameScores=	7							'Jump to Last Scores
Const holdTourneyScores=15							'Jump to Last Scores, hold on screen for 20 seconds to write down

'loopCatch Bits
Const catchBall=		1	'"00000001"				'Flag that means try and catch the ball next time it comes in the loop
Const checkBall=		2	'"00000010"				'Flag that we're checking if ball actually caught
Const ballCaught=		128	'"10000000"				'Flag that the ball has been caught in the loop

'Multiball Bits
Const multiballLoading=	1	'"00000001"				'Bit that says multiball is loading
Const multiballLoaded=	2	'"00000010"				'Bit that says all balls have been loaded for MB
Const multiballMinion=	128	'"10000000"				'Bit that says this is a Minion Multiball!
Const multiballHell=	64	'"01000000"				'Bit that says this is a Hellavator Multiball! (it can be both!)

dim adultMode:adultMode = 1								'Disables some of the worst lines. To some extent. A little.?

dim coinDoorState:coinDoorState = -1	'99						'If coin door is open (1) or not (0) or don't know (99) EP- open = 1, closed = -1
dim coinDoorDetect:coinDoorDetect = 1							'Whether or not we care if the door is open or not (1 = care, 0 = don't. Use 0 for games without a door switch)

dim skip:skip = 0									'If NOT ZERO, a skippable event is occurring. The value indicates which event is occurring, so the system knows what to do if player chooses to skip

dim tiltTimer:tiltTimer = 0								'If a tilt was detected
dim tiltSense:tiltSense = 45000							'If second tilt is detected before timer gets lower than this value, game goes dimo TILT	
dim timerTop:timerTop = 50000							'Starting timer value for tilt			
dim tiltFlag:tiltFlag = 0								'If a tilt occurred
dim tiltCounter:tiltCounter = 0								'How many warnings you got
dim tiltLimit:tiltLimit = 3								'Warning limit

dim whichMenu:whichMenu = 1							'Which menu we are in
dim whichSelection:whichSelection = 1						'What is selected
dim menuAbortFlag:menuAbortFlag = 0						'If you try to enter the menu during a game, it sets this flag, ends game, and puts you back in the main loop
dim audioSwitch:audioSwitch = 0							'Speaks which switch has been tested, in case your PF is in front of DMD

dim dataOut(16)								'What we're sending to the Propeller
'dim dataIn(16)								'We can get up to 16 bytes at a time from the Propeller
'dim eepromChecksum:eepromChecksum = 1						'Location 14 on the output buffer. When EEPROM data is fetched, same number is placed in location 14 of returned data

dim freePlay:freePlay = 1							'If the machine is Free Play or not (default = TRUE)
dim coinsIn:coinsIn = 0								'How many coins you've inserted. Once it equals coinsPerCredit, a credit is awarded!
dim coinsPerCredit:coinsPerCredit = 2						'Good old 25 cents per game!
dim credits:credits = 0       						'1 credit per coin event.  
dim replayValue:replayValue = 50000000					'Free credit if player exceeds this score
Dim replayPlayer(5)							'Flag if a player has acheived a replay this round
dim allowReplay:allowReplay = 1							'If game awards replays or not (default is YES)
dim allowMatch:allowMatch = 1							'If we should do match animation at end of game

dim gamesPlayed:gamesPlayed = 0							'Total games played since last reset
dim ballsPlayed:ballsPlayed = 0							'Counts balls played to compute average ball time
dim totalBallTime:totalBallTime = 0						'Total seconds a ball is in play. Divide by ballsPlayed to get average
dim averageBallTime:averageBallTime = 1						'Calculate and store here
dim secondsCounter:secondsCounter = 0						'Counts seconds to add to totalBallTime
dim extraBallGet:extraBallGet = 0						'How many extra balls have been earned
dim replayGet:replayGet = 0							'How many replays have been earned
dim matchGet:matchGet = 0							'How many matches succeeded
dim coinsInserted:coinsInserted = 0						'How many coins / tokens inserted

dim dollars:dollars = 0								'Displays Earnings
dim cents:cents = 0								'Displays Earnings

dim debugSwitch:debugSwitch = 0
dim ballSearchEnable:ballSearchEnable = 1						'If we should look for balls or no

' Mode-specific variables------------------------------------------

dim Mode(5)									'What mode is currently active.
dim ModeWon(5)								'What modes player has won
dim modeRestart(5)							'What modes player is eligible to restart Binary B01111110 like Mode Won
dim restartTimer:restartTimer = 0						'Timer for seconds
dim restartSeconds:restartSeconds = 0						'Timer for Restart
dim restartMode:restartMode = 0							'What mode we're trying to restart, so game knows what to "kill" if they miss
dim popMode(5)								'What the pop bumber is advancing for each player. ( 1 is fort, 2 is bar)
dim popActive:popActive = 0							'If any pop was hit during a cycle
dim tourBits:tourBits = 0							'Set the 4 LSB's in this to "tour" the haunted locations and enable bonus perks (just like COD!)
dim tourTotal:tourTotal = 0							'Counts the bits we've hit
dim tourLights(6)							'Which tour lights should be on. We keep a copy here so Combo Timeouts won't erase Tour Shots
dim tourComplete(5)							'Which tours the player has completed. Do all 6 for SUPER POdimS!
dim Advance_Enable:Advance_Enable = 1						'Default is 1. Set to 0 if a Ghost Battle is in progress.
dim loopCatch:loopCatch = 0							'Used for catching balls in the Ghost Loop and making sure they're caught before proceeding

' Hellvator Multiball-------------------------------

dim multipleBalls:multipleBalls = 0						'A flag that says a mode is using multiple balls, but isn't a Minion or Hellavator multiball. Confusing, I know
dim multiBall:multiBall = 0							'Multiball. Bit 7 = Minion MB, Bit 1 = All balls launched Bit 0 = Launching Balls
dim hellMB:hellMB = 0								'Flag used to tell Hell MB apart from Minion MB (also if we can catch ghosts or not)
dim catchValue:catchValue = 0							'How many times you've caught all 4 in multiball (adds multiplier)
dim lockCount(5)							'How many balls have been soft locked in the Hellavator
dim multiCount:multiCount = 0							'How many balls the game should auto-launch for a Multiball
dim multiTimer:multiTimer = 0
dim hellJackpot(5)							'Starting MB jackpot value
dim hitsTolight(5)							'How many times you have to press "Call" before hellavator moves / lights for lock							
dim callHits:callHits = 0							'How many times you've hit Call this ball (resets per player)

' Video Mode---------------------

dim videoMode(5)							'1:videoMode(5)							'1 = Mode Ready! 10 = Mode ready when current mode ends 100 = Instruction Screen 101 = Started!
dim ghostY:ghostY = 0								'Y position of ghost
dim videoModeEnable:videoModeEnable = 1						'If Video Mode can be started or not. I sure hope not. I hate video modes!
dim videoCount:videoCount = 0
dim videoSpeed:videoSpeed = 0							'Speed at which the video advances
dim videoCycles:videoCycles = 0
dim frameNumber:frameNumber = 0							'Which frame of video we are on
dim vidBank:vidBank = 0								'What we're loading next, A or B
dim videoSpeedStart:videoSpeedStart = 4						'Default speed at which the video advances

' Team Member Spelling & Ghost Minions -------------------------------

dim wiki(5)
dim tech(5)
dim psychic(5)
dim scoringTimer:scoringTimer = 0						'How long DOUBLE SCORING will go on
dim minionDamage:minionDamage = 1						'How much damage you cause per hit minion
dim minion(5)								'The state of the Minion fight per player
dim minionTarget(5)							'How many hits to beat the minion
dim minionHits:minionHits = 0							'How many times you've hit the minion. Resets on mode start / ball loss. Is signed if you go below 0 with double damage
dim minionsBeat(5)							'How many minions the player has beaten
dim minionHitProgress(5)					'Saves how many hits you previously got on a Minion if you start another mode before beating him
dim minionMB:minionMB = 0							'Flag to keep track of Minion Multiball
dim minionJackpot:minionJackpot = 0						'What current Jackpot is
Const minionMB1	= 2							'Which Minions give MB (Needs to be 1 below, IE, if third minion gives MB, set to 2, for 9th, set to 8
Const minionMB2	= 8

dim comboSeconds:comboSeconds = 0						'How many seconds combos are lit for. Can be changed in menu
dim comboTimerStart:comboTimerStart = 7200				'72000					'Cycle counter for how long combos are lit. Default = 6 seconds
dim comboVideoFlag:comboVideoFlag = 0						'If a combo was hit, this flag makes it so the next video is enqueued (so we see COMBO + normal shot video)
dim comboCount:comboCount = 1							'How many combos player has made
dim comboTimer:comboTimer = 0							'Time left to get a combo
dim comboShot:comboShot = 0							'Which Camera Shot has the combo lit (0-5)
dim comboEnable:comboEnable = 0							'0 = No combos allowed (some modes) 1 = Combos OK! (mode modes, but check!)

dim hellLock(5)												'If you can lock balls in the Hellavator or not
dim spiritGuide(5)											'If spirit guide is lit, and what was awarded if you shoot it
dim spiritGuideActive:spiritGuideActive = 0					'During multi-ball and some other things, Spirit Guide is disabled
dim spiritProgress(5)										'If in tourney mode, this tracks players progress through spirit guide (but still skip awards they've already claimed)

Const teamWiki=		128	'"10000000"							'Bit values for teamMod flags
Const teamTech=		64	'"01000000"
Const teamPsychic=	32	'"00100000"

dim EVP_Target:EVP_Target = 10								'How many pops to get an EVP
dim popCount:popCount = 0									'How many pops we have
dim EVP_Total(5)											'How many EVP's each player has collected
dim EVP_EBtarget(5)											'How many EVP's each player must get to earn Extra Ball
dim EVP_EBsetting:EVP_EBsetting = 10						'Defaults to 10, can be changed in menu if I remember to add it in			
dim EVP_Jackpot(5)											'Jackpot value per player.
dim jackpotMultiplier:jackpotMultiplier = 0					'Current multiplier for the mode
dim photosTaken(5)											'Total photos a player got.
dim areaProgress(5)											'How many mode-advancing shots each player has made
dim ghostsDefeated(5)										'Total ghosts defeated per ball
dim orb(5)													'Which ORB roll over lanes have been hit
dim bonus:bonus = 0											'Total bonus at end of ball
dim bonusMultiplier:bonusMultiplier = 0						'Multipliers per ball
dim scoreMultiplier:scoreMultiplier = 1						'Can be used for double scoring and stuff. Right now just for Psychic Scoring
dim zeroPointBall:zeroPointBall = 1							'If you score zero podims on a ball 1 means you get it back, 0 means too bad sucker!

'General Mode Variables------------------------------------------

dim gTargets(3)												'Which of the Ghost Targets have been cleared.
dim targetBits:targetBits = 7								'Which targets have NOT been cleared (starts at B00000111)
dim targetsHit:targetsHit = 0								'How many of the 3 targets you have hit
dim saveStart:saveStart = 5									'50000								'The default amount of ball save time, in seconds EP- apparently this is fixed in the newer code
dim saveCurrent(5)											'How much Save Start time each player has (can be increased during game)
dim saveTimer:saveTimer = 0									'Timer for Ball Save
dim scoopSaveStart:scoopSaveStart = 151						'1510					'Ball save time, in milliseconds, when ball is ejected from scoop (default 1.5 seconds)
dim drainTimer:drainTimer = 0								'Timer for events after a ball drain
dim modeTimer:modeTimer = 0									'Timer for stuff in modes, like random taunts and hurry ups
dim displayTimer:displayTimer = 0							'Timer for display actions
dim skillShot:skillShot = 0									'If skill shot is enabled, and which one we're going for
dim launchCounter:launchCounter = 0							'For debug purposes. Counts how many times it's tried to load the ball

'Hospital - Mode 1

dim hosProgress(5)
dim hosTrapCheck:hosTrapCheck = 0						'Flag if a ball search has to occur and kick out the VUK
dim DoctorState:DoctorState = 0							'0 = Guarding door, 1 = Distracted
dim DoctorTimer:DoctorTimer = 0							'Count up timer. When it reaches limit, ghost moves back towards door a bit.
dim DoctorTarget:DoctorTarget = 0						'Target amount before move. With each hit, ghost moves a little faster.
dim DoctorSeconds:DoctorSeconds = 0						'Hurry-up timer display. Not really in seconds.
dim doctorHits:doctorHits = 0							'Only prompt on Doctor hits every 3 times
dim patientStage:patientStage = 0						'What stage of Ghost Patient you're at
dim patientsSaved:patientsSaved = 0						'How many you saved, through Murder!
dim badExit:badExit = 0									'If = 0, then ball ejected properly, rolled down habitrail and hit left inlane switch

'Theater - Mode 2

dim theProgress(5)										'The progress in Theater Mode
dim sweetJumpBonus:sweetJumpBonus = 0					'How many podims you get for SWEET JUMPS
dim sweetJump:sweetJump = 0								'How many JUMPS you've done (directs what video plays)
dim shotValue:shotValue = 0								'Keeps track of what next shot is worth. Decrements each second.
Const TheaterTime =	21									'16 seconds, plus some slop for the display

'Bar - Mode 3

dim spotProgress:spotProgress = 0						'What level the pops start at (0 - 12 spot halfway)
dim barProgress(5)										'How many pops have advanced the Bar
dim whoreJackpot:whoreJackpot = 0						'How many Jackpots on Ghost Whore.
dim kegsStolen:kegsStolen = 0							'How many kegs have been stolen!

'War Fort - Mode 4

dim fortProgress(5)										'How many pops have advanced in the Fort Mode
dim soldierUp:soldierUp = 0			
dim warHits:warHits = 0
dim goldHits:goldHits = 0								'How many hits on the door
dim goldTimer:goldTimer = 0								'How long to get the gold!
dim goldTotal:goldTotal = 0								'How much you collected
Const GoldTime =	21									'20 SECONDS TO COLLECT GOLD

'Hotel - Mode 5

dim hotProgress(5)
dim ControlBox(6)										'Flag to set where the random control box is, and where we've checked already
dim HellBall:HellBall = 0								'Status of what the ball in the Hellavator is doing.
dim hellCheck:hellCheck = 0								'Used to check if a ball is stuck in the Hellavator

'Prison - Mode 6

dim priProgress(5)
dim Tunnel:Tunnel = 0									'Did the ball just roll through the tunnel? Used for Basement Scoop switch logic
dim teamSaved:teamSaved = 0								'How many members we've saved
dim convictState:convictState = 0						'Freeing convict ghosts. 1 = Need to open door 2 = Need to shoot scoop
dim convictsSaved:convictsSaved = 0						'How many you've saved. Maybe we use this for a bonus or something

'Ghost Photo Hunt - Mode 7

dim rollOvers(5)										'GLIR rollover targets. Use LSB's
dim GLIR(5)												'Flag if GLIR is lit and can be started
dim GLIRneeded(5)										'How many times each player must spell GLIR to light Photo Hunt
dim GLIRlit(5)											'If GLIR is lit for a player. If MSB bit set, prevents it from being started (usually when a Minion is active)
dim photosToGo:photosToGo = 0							'How many photos left to collect (from 9-0 to 3-0)
dim photosNeeded(5)										'Number of photos each player must collect. (starts at 3)
dim photoTimer:photoTimer = 0							'Timer used just for photo mode!
dim countSeconds:countSeconds = 0						'How many seconds left. Used for other modes, too
dim photoLocation(6)									'What shots have valid photos
dim photoCurrent:photoCurrent = 0						'Which location (0-5) currently has the photo
dim photoLights:photoLights = Array(7, 14, 23, 31, 39, 47)		'The lamp number of the Camera Icons, from left to right
dim photoStrobe:photoStrobe = Array(4, 6, 3, 5, 3, 4)	'How many south of Camera icon that you can strobe
dim photoSecondsStart(5)								'How many seconds you get to collect a photo
dim photoValue:photoValue = 0							'Current value of photos (decreases every second!)
dim photoPath:photoPath = Array(1, 3, 0, 2, 4, 5, 1, 3, 0, 1, 0)		'Sequence in which to make the shots if in Tournament Mode (with overflow just in case)
dim photoWhich:photoWhich = 0							'How many we've taken per round of Photo Hunt (used to guide tourney path)

'Demon Battle - Mode 10

dim deProgress(5)							'How far you are through the mode
dim demonLife								'How weak Demon is
'****EP- We don't need debounce stuff****
Const trapSwitchSlow=	500								'5000	'When finding balls, the "slow" reaction time for switches
Const trapSwitchNormal=	20								'200		'The default switch ramp time for trapped balls
					 'Switch 0.........................................7
dim switchDead:switchDead = 0									'Checks to see if the ball is stuck
Const defaultR =	128						'Default "mode 0" colors (medium white)
Const defaultG =	128
Const defaultB =	128
Const tempLamp =	5						'Memory area for temp light animations

dim lightningTimer:lightningTimer = 0		'To flash some lightning
dim lightningGo:lightningGo = 0				'If a lightning effect is occuring
dim lightningPWM:lightningPWM = 0			'For PWM lightning FX
dim leftRGB(3)								'RGB colors of left cabinet GI
dim rightRGB(3)								'RGB colors of right cabinet GI
dim cabModeRGB(3)							'The default colors of the cabinet for each Mode (not the same as generic white default colors)
dim targetRGB(3)							'RGB color the cabinet lighting is trying to get to

dim RGBtimer:RGBtimer = 0					'Times how quickly the RGB changes (100 cycles is good)
dim RGBspeed:RGBspeed = 0					'How quickly it changes

dim ghostRGB(3)								'The current RGB color of the ghost
dim ghostModeRGB(3)							'What color the ghost should be for the mode
dim ghostFadeTimer:ghostFadeTimer = 0		'Flag for if the ghost should fade
dim ghostFadeAmount:ghostFadeAmount = 0		'What amount the timer should reset to

dim GIword:GIword = 0						'The general illumination that will get sent out

dim animationTimer:animationTimer = 0		'How many kernel cycles before animation advances
dim lightStart:lightStart = 0				'What frame # the PF animation starts on									
dim lightCurrent:lightCurrent = 0			'What frame # the PF animation is currently on
dim lightEnd:lightEnd = 0					'Last frame in this animation. When lightCurrent++ > lightEnd, we revert to lightStart
dim lightStatus:lightStatus = 0				'Control byte for insert light animations

Const animationTarget=	80					'800					'12,000 HZ / 15 FPS = 800 kernel cycles per light frame
'Const lightAnimate	B10000000				'Bit 7 enables animation
dim lightAnimate:lightAnimate = 1			'EP- doing my best to understand this
'Const lightLoop		B01000000			'Bit 6 causes animation to loop back to lightStart until disabled
dim lightLoop:lightLoop = 1					'EP- doing my best to understand this

'dim lightData(8) 							'Bits of each byte control the lights, for a total of 64. What actually gets output to the pins.
'dim lightCol:lightCol = 0 					'Current Column byte we are displaying (rows = bits)
'dim lightColBit:lightColBit = 1 			' Shifts left with each row to trigger Darlingtoin arrays
'dim lightRowBit:lightRowBit = 1			'Used to build each byte of Column data
'dim lightGap:lightGap = 0					'Ghost-busting gap
'dim lightPWM:lightPWM = 0 					'PWM Timer for lights
'Const	lightcyclefreq	8000 				'Number of times per second to run the light routine (8000 / 8 columns / 8 cycles PWM = 125 HZ)

dim lamp(65)								'PWM values for all 64 lights
dim lampState(65)							'What state each light is in 0:lampState(65)							'What state each light is in 0 = standard, 1 = blink, 2 = strobe + 3, 4 = pulsate
dim strobeAmount(65)
dim lampPlayers(321)						'Stores a player's lamps
dim statePlayers(321)						'Stores a player's lamp states
dim strobePlayers(321)						'Stores a player's strobe states
dim lampnum:lampnum = 0						'Which lamp we are computing at the moment (used in dimerrupt)
dim lightNumber:lightNumber = 0				'The lamp number pulled from the op code

dim pulseDir:pulseDir = 0					'What direction the pulse is going
dim pulseLevel:pulseLevel = 0				'Current pulse level (for all lights)
dim pulseTimer:pulseTimer = 0				'Timer for the pulses
dim strobePos(65)							'Which lamp the stobe is on (0:strobePos(65)							'Which lamp the stobe is on (0 = target, 2 = third)
dim strobeTimer:strobeTimer = 0
dim blinkTimer:blinkTimer = 0				'Timer for blinking the lights

dim dirtyPoolTimer:dirtyPoolTimer = 0		'Checks if a ball is stuck under the ghost. Check this after modes where it's possible.
dim dirtyPoolChecker:dirtyPoolChecker = 1	'If the game should check for Dirty Pool. Modes that want to trap the ball should set this to 0 until complete.
dim trapTargets:trapTargets = 0				'If a ball is trapped behind targets, set this flag so ball search won't release it
dim trapDoor:trapDoor = 0					'Flag that a ball is to be held behind the door in the VUK (Hospital Mode / Demon locks)

dim LeftTimer:LeftTimer = 0
dim LeftPower:LeftPower = 0
dim RightTimer:RightTimer = 0
dim RightPower:RightPower = 0

dim centerTimer:centerTimer = 0				'Avoid double hits on Pop Bumper Path, and supresses video on Pops
dim popsTimer:popsTimer = 0					'If ball rolls out of pops, keeps Center Shot from triggering
dim rampTimer:rampTimer = 0					'Avoids double hits on the Ramp Approach switch
dim orbTimer:orbTimer = 0							'Avoids false scores on Balcony Approach switch

dim slingCount:slingCount = 0							'Counts the Sling Hits. Dialog once at 4 hits, resets when Timer is zero

dim lightSpeed:lightSpeed = 1							'How fast blinks, pulsates and strobes occur. Depends on kernel speed too. Default = 1

Const strobeSpeed=	60						'1000		bigus			'Number of cycles before the strobe advances
Const pulseSpeed=	55						'500						'Number of cycles before the pulse changes values
Const blinkSpeed0=	150						'2000					'Number of cycles before the blink changes
Const blinkSpeed1=	300						'4000					'Number of cycles before the blink changes

dim LeftVUKTime:LeftVUKTime = 0							'Kickout timer for left VUK (behind door)
dim ScoopTime:ScoopTime = 0							'Kickout timer for Basement Scoop

dim LeftOrbitTime:LeftOrbitTime = 0						'Timer that lefts us know which way the ball is going on Left Orbit.
dim UpperOrbitTime:UpperOrbitTime = 0						'Timer after upper switch hit on orbit. Used to avoid double advance on Prison Lock

dim LFlipTime:LFlipTime = -1							'Timer for flipper high current
dim RFlipTime:RFlipTime = -1							'Timer for flipper high current

dim LholdTime:LholdTime = 0							'Timers for hold coil PWM
dim RholdTime:RholdTime = 0							'Timers for hold coil PWM

'dim leftDebounce:leftDebounce = 0						'Flipper buttons don't use the built-in Cabinet Button Debounce
'dim rightDebounce:rightDebounce = 0						'These variables do it manually

dim plungeTimer:plungeTimer = 0							'Timer for Autoplunging!
dim ballQueue:ballQueue = 0							'If another ball is added DURING a plunge timer event. Unlikely, but possible.

'-----------------------------------------------------------------------

dim drainSwitch:drainSwitch = 63						'Which ball counter switch acts as drain
dim tournament:tournament = 0							'If game is in Tournament Mode or no (1 = YES 0 = NO)
dim ball:ball = 0								'Starts at ball 1, should ball = 4 game is over (man)
dim ballsPerGame:ballsPerGame = 4						'At which ball count the game ends. Should be the # of balls you want, plus 1. (so for a 1 ball game it'd be 2)
dim activeBalls:activeBalls = 0							'How many balls are on the playfield
dim extraBalls:extraBalls = 0						'Flag that gives current player an extra ball after drain / bonus
dim allowExtraBalls:allowExtraBalls = 1				'Should game allow extra balls?
dim extraLit(5)										'If player has an Extra Ball lit or not
dim scoreBall:scoreBall = 0							'Whetever or not a player scored on a ball or not
dim playerScore(5)									'Each player's score. Use 1-4, skipping 0   
dim numPlayers:numPlayers = 0						'Total # of players in the game
dim loadChecker:loadChecker = 0						'On first load, makes sure ball fully loaded.
dim modeTotal:modeTotal = 0							'Total podims you made in a mode
dim showScores:showScores = 0						'Don't show scores during attract mode until there's been a game completed

dim startingAttract:startingAttract = 1				'When machine resets, which part of the Attract Mode it should goto
dim attractLights:attractLights = 1					'If the machine should do lighting Attract Mode. Usually yes, but disabled in Debug Mode
dim player:player = 0								'Player currently playing
dim run:run = 0          							'What state the machine is in during attract and game start modes
dim kickTimer:kickTimer = 0							'How long before a ball is kicked out of the drain
dim kickPulse:kickPulse = 0							'To pulse the kicker coil
dim kickFlag:kickFlag = 0							'Flag that says ball has been kicked from the drain. Keeps Ball Switch 4 from accidentally triggering a double drain					
	
dim pPos(4)											'Sorts the scores at the end of a game 0:pPos(4)									'Sorts the scores at the end of a game 0 = highest, 3 = lowest
dim highScores(5)									'Best (0) and 5th (4)
dim initials(3)										'What has been entered on the initial screen
dim topPlayers(15)									'The top players initials
dim inChar:inChar = 65								'Which character the player is entering
dim cursorPos:cursorPos = 50						'Cursor position of character entry (0-2) Hitting START on character 2 finishes entry

'-----------------------------------------------------------------------

Const drainStart=	10000							'100000

dim ghostLook:ghostLook = 1				'Should the ghost look at shots as they're made?
dim ghostAction:ghostAction = 0			'Timer / control for making the ghost do things.
dim ghostBored:ghostBored = 0			'After looking someplace, eventually the ghost gets bored and turns back to center.

dim MagnetTimer:MagnetTimer = 0			'How long should the magnet stay on?
dim MagnetCount:MagnetCount = 0			'This is used to PWM the magnet.
dim magFlag:magFlag = 0					'If the magnet should be pulsing or not during the timer

dim HellLocation:HellLocation = 0		'Location of the Hellavator
dim HellTarget:HellTarget = 0			'Where were are trying to move the elevator
dim HellSpeed:HellSpeed = 0				'Speed (in cycles) to move elevator, set to 0 to indicate target acquired
dim HellTimer:HellTimer = 0				'Counts cycles between moves
dim HellSafe:HellSafe = 0				'Checks if ball successfully exits Hellavator

dim DoorLocation:DoorLocation = 0		'Location of the Spooky Door
dim DoorTarget:DoorTarget = 0			'Where were are trying to move the door to
dim DoorSpeed:DoorSpeed = 0				'Speed (in cycles) to move door, set to 0 to indicate target acquired
dim DoorTimer:DoorTimer = 0				'Counts cycles between moves
dim doorCheck:doorCheck = 0				'Checks for ball traps during ball search

dim TargetLocation:TargetLocation = 0	'Location of the Target
dim TargetTarget:TargetTarget = 0		'Where were are trying to move the Target
dim TargetDelay:TargetDelay = 0			'How long until the targets start moving
dim TargetNewSpeed:TargetNewSpeed = 0	'What speed to set once Delay Timer is up
dim TargetSpeed:TargetSpeed = 600		'Speed (in cycles) to move Target, set to 0 to indicate target acquired
dim TargetTimer:TargetTimer = 0			'Counts cycles between moves

dim GhostLocation:GhostLocation = 0		'Rotation of Ghost.
dim ghostTarget:ghostTarget = 0			'Where we want the ghost to go
dim ghostSpeed:ghostSpeed = 0			'How often the ghost changes location
dim ghostTimer:ghostTimer = 0			'Timer to set Ghost Speed

dim sfxVolume(2)						'Volume of SFX
dim musicVolume(2)						'Volume of the left and right channels
dim lastMusic(2)						'What music WAS playing
dim currentMusic(2)						'What music is currently playing
dim musicDefault:musicDefault = 35		'Default music volume
dim sfxDefault:sfxDefault = 75			'Default SFX volume

dim leftVolume:leftVolume = 100		
dim rightVolume:rightVolume = 10

dim SolTimer(24)										'32 bit system-based timer for solenoids
dim AutoEnable:AutoEnable = 0										'Which solenoids can auto-fire with PC commands  

'dim coilSettings():coilSettings() = {300, 15, 15, 10, 30, 30, 0}		'Flipper, Slings, Pops, Left Vuk, Right Scoop, Autolauncher, etc...	

dim coilDefaults:coilDefaults = Array(9, 9, 9, 8, 3, 9, 8, 4, 0)	'Flipper, Slings, Pops, Left Vuk, Right Scoop, Autolauncher, Load strength, Drain kick strength, null
dim coilSettings:coilSettings = Array(9, 9, 9, 9, 9, 6, 5, 6, 0)	'Flipper, Slings, Pops, Left Vuk, Right Scoop, Autolauncher, Load strength, Drain kick strength, null					   

Const autoPlungeFast=	208								'2084							'What setting gives an "instant" autoplunge
Const autoPlungeSlow=	292								'2917							'Slower version

dim autoPlungeCheck:autoPlungeCheck = 0									'If an autoplunge should wait for drained ball to be kicked back dimo trough before loading

'-------------------------Define Game Settings. Production Game Only-------------------------------------------------------------------


'Variable (User Changeable) Coil Settings------------------------------

dim FlipPower:FlipPower = 300 						'Default flipper high power winding ON time, in cycles
dim SlingPower:SlingPower = 15						'How hard the slings hit	
dim PopPower:PopPower = 15                       	'Default auto power for pop bumpers
dim vukPower:vukPower = 25							'Power of the left VUK behind door
dim scoopPower:scoopPower = 45							'Power of the right basement scoop	
dim plungerStrength:plungerStrength = 30					'How hard the autolauncher kicks it out
dim loadStrength:loadStrength = 6						'How hard the ball loader is
dim drainStrength:drainStrength = 12						'15 How hard it gets out of drain

dim drainTries:drainTries = 0							'If a drain kick doesn't work, this increments and is added to the Drain Strength until all 4 balls are loaded

dim drainPWMstart:drainPWMstart = 5850					'When to switch from Drain Kick power kick to PWM hold

'Static Coil / Magnet Settings-----------------------------------------
'Const loadStrength	10						'How hard the ball loader is
'Const drainStrength	10					'15 How hard it gets out of drain
Const holdTop=			50 '250				'Used to PWM the hold coil on flippers
Const holdHalf=			25 '125				'Save a calculation later	
Const magPWM=			100 '350			'How many cycles between magnet pulses to hold it on
Const magFlagTime=		2					'How many MS long each magnet cycle pulse is (stay under 10 else it's always on)


'Sets the ramp-up per switch. The switch must be on XXX many cycles in order to register a hit	
'dim cabRampDBTime():cabRampDBTime() = {200, 200, 200, 5, 5, 200, 200, 200, 			'unused, Door, User0, RFlip, LFlip, Menu, Enter, Coin
'									25, 0, 5, 200, 200, 2, 2, 200,} 				'Tilt, ghostOpto, doorOpto, unused, Start, ghostOpto, doorOpto, unused

'Sets the debounce time per switch. The switch must be off XXX cycles before it can be re-triggered
'dim cabDBTime():cabDBTime() = {200, 200, 200, 200, 200, 200, 200, 200,	  			'unused, Door, User0, RFlip, LFlip, Menu, Enter, Coin
'							  20000, 2500, 7500, 200, 200, 5000, 7500, 200,}		'Tilt, ghostOpto, doorOpto, unused, Start, ghostOpto, doorOpto, unused


'-----Assign Solenoid Pin #'s to a logic number listing
								'NEW    NEW
'dim SolPin():SolPin() = {22, 23, 32, 25, 31, 30, 2, 4, 7, 11, 12, 70, 71, 72, 73, 75, 78, 79, 80, 81, 82, 83, 84, 85}
'Solenoid pin #'s assigned to an array (0-23)

'		sol0
Const Magnet=			0
Const sol1=				1	'GI 1
Const sol2=				2	'GI 2
Const sol3=				3	'GI 3
Const sol4=				4
Const sol5=				5
Const leftBackglass=	6
Const rightBackglass=	7

Const LSling=			8 '
Const RSling=			9 '
Const ScoopKick=		10 '
Const LeftVUK=			11 '
Const sol12=			12
Const Bump0=			13
Const Bump1=			14
Const Bump2=			15

Const RFlipHigh=	78 '   'These need to be set to actual PIN#'s, not solenoid #'s.
Const RFlipLow=		79 '   'These need to be set to actual PIN#'s, not solenoid #'s.
Const LFlipHigh=	80 '   'These need to be set to actual PIN#'s, not solenoid #'s.
Const LFlipLow=		81 '    'These need to be set to actual PIN#'s, not solenoid #'s.

Const LoadCoil=		20	' 'Ball Load
Const drainKick=	21	'Drain Kick (Unused on Ben's prototype)
'Const Plunger=		22	' 'Plunger
Const sol23=		23
'      sol23

Const solenable=	28

'-------------------------------------------------------------------

'dimer-processor communication pins------------------

'Const ATN			14
'Const SDI			14
'Const CLK			15
'Const SDO			16

'-----------------------------------------------------------------------

'Cabinet switch shift register inputs------------------

'Const cdatain			45
'Const cclock			37
'Const clatch			43
'Const GIdata			29

Const startLight=		13

'Shift register bit # declarations (New Pinheck System)------------

'Const	Door				   1
'Const	User0				   2
'Const	RFlip 				   3
'Const	LFlip 				   4
'Const	Menu				   5
'Const	Enter				   6
'Const	Coin				   7
'Const	Tilt				   8
'Const TCBDTWN7			   9
'Const TCBDTWN6			   10
'Const TCBDTWN5			   11
'Const	Start				   12

'Const ghostOpto			   13 'TCBDTWN #1 / Opto 1
'Const doorOpto			   14 'TCBDTWN #2 / Opto 2

'Const	StartLight			   51

'-----------------------------------------------------------------------

'Servo Assignments

'Const HellServo	0
'Const DoorServo	1
'Const GhostServo	2
'Const Targets		3
'Const TargetDown	160				'PRODUCTION VERSION
Const TargetDown=		0
'Const TargetJog	40
'Const TargetUp	5				'PRODUCTION VERSION
Const TargetUp=			53
'Const hellUp		160
Const hellUp=			150
'Const hellStuck	60
Const hellStuck=		100
'Const hellDown	10
Const hellDown=			25
'Const DoorOpen	5				'PRODUCTION VERSION
Const DoorOpen=			80
'Const DoorClosed	90 				'95
Const DoorClosed=		-18
'Const GhostDistracted	120			'Where the ghost turns to when distracted (decreases each time) Make sure it's a multiple of 20 + GhostAtDoor
Const GhostDistracted=	120
'Const GhostMiddle		70			'Halfway back to the Spooky Door
Const GhostMiddle=		70
'Const GhostAtDoor		20			'Ghost guarding the Spooky Door.
Const GhostAtDoor=		20
'Const subwayTime		15000		'Amount of time it takes for ball to exit hellavator and hit Middle Subway Switch
Const subwayTime=		1500

'---------------------------------------------------------------------

'Subway Switch Numbers

Const subUpper=		35
Const subLower=		36


'************************************************
'* Shoopity's variables
'************************************************
Dim CenterCounter:CenterCounter = 0
Dim Pi
Dim Sw16, Sw17, Sw18, Sw19, Sw20, Sw21, Sw22, Sw23, Sw24, Sw25, Sw26, Sw27, Sw28, Sw29, Sw30, Sw31, Sw32, Sw33, Sw34, Sw35, Sw36, Sw37, Sw38, Sw39, Sw40, Sw41, Sw42, Sw43, Sw44, Sw45, Sw46, Sw47, Sw48, Sw49
Dim Sw50, Sw51, Sw52, Sw53, Sw54, Sw55, Sw56, Sw57, Sw58, Sw59, Sw60, Sw61, Sw62, Sw63
Dim CustNumbersUL, CustNumbersUR, CustNumbersBL, CustNumbersBR			'EP- What to put in the upper left/right and lower left/right corners of the DMD
CustNumbersUL = ""
CustNumbersUR = ""
CustNumbersBL = ""
CustNumbersBR = ""
Dim SwLFlip, SwRFlip													'EP- Variables for indicating if the flipper switches are pushed (1) or not (-1)
Dim BallsInTrough:BallsInTrough = 4										'EP- Number of Balls in trough, used for virtual implementation of trough
Dim mMagnaSave
Dim AutoPlunger
Dim Coin
Dim OldDMDPrio:OldDMDPrio = 0											'EP- Currently playing DMD Scene priority
Dim EoBStep:EoBStep = 1													'EP- Used in Stepping through the End Of Ball animation
Dim rgbfadeamount:rgbfadeamount = 1										'EP- How fast the lights fade
Dim RGBTarget															'EP- What the lights are fading to
Dim SndPlaying, SndChannel												'EP- Keeps track of the name of the sound playing in which channel; Keeps track of the priority of the sound playing in the channel
SndChannel = Array(0, 0, 0, 0, 0, 0, 0, 0)
SndPlaying = Array("", "", "", "", "", "", "", "")
Dim PauseAdjuster:PauseAdjuster = 60									'EP- variable for the DMD pause; this may change as UltraDMD changes
Dim MusicRepeats:MusicRepeats = 0										'EP- 0 means No, 1 means yes
Dim BankDir:BankDir = -1												'EP- The speed at which the targets are moving; the direction they're moving
TargetTarget = TargetDown
Dim MusicRestart:MusicRestart = 0
Dim DoorDir:DoorDir = 1													'EP- Is the Door opening or closing
Dim GhostDir															'EP- Is the Ghost turning left or right
Dim ElevatorDir:ElevatorDir = 1											'EP- Is the Elevator going up or down
Dim BallElevator														'EP- Object for moving the ball in the elevator
Dim GhostFast, GhostSlow												'Ep- The fastest the ghost moves in real life, and the slowest it moves
GhostFast = 1.5
GhostSlow = 0.5
Dim ElevatorFast, ElevatorSlow											'EP- The fastest the elevator moves in real life, and the slowest
ElevatorFast = 1
ElevatorSlow = 0.1
Dim DoorFast, DoorSlow													'EP- Same as above, but for the door
DoorFast = 0.75
DoorSlow = 0.1
Dim TargetFast, TargetSlow
TargetSlow = 0.25
TargetFast = 1
Dim gfadespeed:gfadespeed = 1											'EP- Fading for the ghost
Dim GhostFadespeed:GhostFadespeed = 1									'EP- I put this here to make ghost fading speed easily adjustable
dim BallInElevator														'EP- is there a ball in the elevator
'******************************************************************** End Variables************************************************
'********************************************************************
'* Light Show
'********************************************************************
'Constant declarations for Light Shows
'animatePF(starting frame, number of frames, repeat);
'Start, # of Frames
'  0, 30			Original Attract Mode Lights
'  30, 14			GLIR whoosh left to right, short highlight on scoop
'  44, 30			Minion Kill animation!
'	74, 30			Ball Locked Vertical Wipe
' 	104, 15			Minion Hit!
'	119, 30			Psychic Scoring background
'	149, 30			Ball drain fade
'	179, 10			Center Ghost Burst Out
'  190, 10			Left Orbit Shot 0 Chase
'	200, 10			Door Shot 1 Chase
'	210, 10			Shot 2 Up Center
'	220, 10			Shot 3 Up Ramp Hotel
'	230, 10			Shot 4 Right Orbit Theater
'	240, 10			Scoop Explode!		
					
Dim lightFrame:lightFrame = 0						'Which lightshow frame we're showing
Dim lightShow(251)

LightShow(0) =   "0007777700000000000000000000000000000000700000007000000070000000"'Frame0
LightShow(1) =   "0005555577777777770000000000000011100000070000005707100057000000"'Frame1
LightShow(2) =   "0002222255555555070077770000000022200000000000002576210025700000"'Frame2	
LightShow(3) =   "0000000022222222700755550777777733300000700000000255321002570000"'Frame3	
LightShow(4) =   "0070000000000000000722220755555544407777070000000024432100257000"'Frame4	
LightShow(5) =   "0050000000000000007000000722222255505555007777770004543200025700"'Frame5
LightShow(6) =   "0020000000000000007000007000000066602222700555550003654300002570"'Frame6	
LightShow(7) =   "0000000000000000070000007000000077700000070222220002765400000257"'Frame7
LightShow(8) =   "0070000000000000070000007000000066600000000000000001676570000025"'Frame8	
LightShow(9) =   "0050000000000000007000000700000055500000700000000070567657000002"'Frame9
LightShow(10) =  "7027000070000000007070000770000044407000070700000750456725700000"'Frame10	
LightShow(11) =  "6007700077000000000777000777000033307700000770007520345602570000"'Frame11	
LightShow(12) =  "5007770077700000000777707077700022207770000777005200234500257000"'Frame12	
LightShow(13) =  "4007777077770000007077777077770011107777000777700000123400025700"'Frame13
LightShow(14) =  "3007777777777000007000007077777000000000000777777000012300002570"'Frame14	
LightShow(15) =  "2000000077777700070077770777777711107777000000005700101200000257"'Frame15	
LightShow(16) =  "1007777777777770070000000700000022200000000777772570210170000025"'Frame16	
LightShow(17) =  "0770000077777777000077770777777733307777000000000250321077000002"'Frame17
LightShow(18) =  "0657777700000000011100007000000044400000000777770020432177700000"'Frame18	
LightShow(19) =  "0520000077777777022277777077777755507777000000000000543277770000"'Frame19
LightShow(20) =  "0407777700000000033300007000000066600000000777770000654377777000"'Frame20
LightShow(21) =  "0370000077777777044477770077777777707777000000000000765477777700"'Frame21
LightShow(22) =  "0250000000000000055500000000000066600000000777770070676577777770"'Frame22	
LightShow(23) =  "0120000000000000766600000000000055500000000000000750567677777777"'Frame23
LightShow(24) =  "0000000000000000077700000000000044400000000000007520456700000000"'Frame24
LightShow(25) =  "0000000000000000766600000000000033300000000000005200345677777777"'Frame25
LightShow(26) =  "0000000000000000055500000000000022200000000000002000234500000000"'Frame25	
LightShow(27) =  "0000000000000000744400000000000011100000000000000000123477777777"'Frame26	
LightShow(28) =  "0000000000000000033300000000000000000000000000000000012300000000"'Frame27	
LightShow(29) =  "0000000000000000722200000000000000000000000000000000001277777777"'Frame28	
LightShow(30) =  "0000000000000000011100000000000000000000000000000000000100000000"'Frame29

LightShow(31) =  "0000007700000000000000000000000000000000700000000000700000000000"
LightShow(32) =  "7707777700007777000000000000000000000000700000000000770000000000"
LightShow(33) =  "7707777777777777070000000000000000000000700000000000770000000000"
LightShow(34) =  "7707777777777777777700000000000000000000700000007700770077777777"
LightShow(35) =  "6777666677777777777777777777700070000000500000007760560077777777"
LightShow(36) =  "4575444377666665777777777777777777700000300770007767340077777777"
LightShow(37) =  "2372222155444333767777777777777777707777100777776757127777777777"
LightShow(38) =  "0171000032221111545677777777777777707777070777774557007766666666"
LightShow(39) =  "0050000011000000312455556566666756707777070777772337007733333333"
LightShow(40) =  "0030000000000000101122333334444524606677070666770116007711111111"
LightShow(41) =  "0010000000000000000011111111222212304455060344550004005600000000"
LightShow(42) =  "0000000000000000000000000000000100102223040122230001003400000000"
LightShow(43) =  "0000000000000000000000000000000000000011020000110000001200000000"
LightShow(44) =  "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(45) =  "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(46) =  "0000000000000007070004400000000000000000000000000000000000000000"
LightShow(47) =  "0000000027630003040047202000000000000000000000000000000000000000"
LightShow(48) =  "0300531024773101020275107300003700000000000000000007000000000000"
LightShow(49) =  "0700367702477410011462017701377400000320000000000003000000000000"
LightShow(50) =  "0700024700257730312720143747774100003200010000000000000000000000"
LightShow(51) =  "0300001300125761615600371376431000000000470000000000000000000004"
LightShow(52) =  "0200000121102573736302670132100000000000130000000060000000000467"
LightShow(53) =  "0100000043101366565025761000000000000000000000000030000000025656"
LightShow(54) =  "3103321076421357262047743100001352000000000000000002240000000012"
LightShow(55) =  "4707777535666323111475427701367724702577000000000007000000000000"
LightShow(56) =  "0400023500014651615733353577753200003210770777640001007700000004"
LightShow(57) =  "0000000043210156677324660011000010000000020000002040000005777776"
LightShow(58) =  "7407743277764213153166524100023577500023000000000005760000000000"
LightShow(59) =  "0702347713357630112553236735777500207764130013570004000000000000"
LightShow(60) =  "0200000210013674726613570254332100000000370332100040001000013347"
LightShow(61) =  "2002110076311247374146752000001363000000000000000002140000000123"
LightShow(62) =  "4706777435675223021376427602347713603477000000010007100000000000"
LightShow(63) =  "0400023501125740404732353677775300004321770777740002007500000003"
LightShow(64) =  "0000000033200265767313640121100000000000000000000040000000057777"
LightShow(65) =  "7307432177752015262066205300013567300013000000000004000000000000"
LightShow(66) =  "0701367713477510211630036734777400007742000000060003000000000000"
LightShow(67) =  "0100000100013673737401570132100000000000040000000000000000000007"
LightShow(68) =  "0000000074331137173047731000000000000000000000000000000000000000"
LightShow(69) =  "0502775303776313010374107600016700000047000000000007000000000000"
LightShow(70) =  "0100000100004740204710140277520000000000000000000000000000000000"
LightShow(71) =  "0000000000000065365002750000000000000000000000000000000000000000"
LightShow(72) =  "0000000017720002020057202000000000000000000000000000000000000000"
LightShow(73) =  "0000000000017200000320001700000000000000000000000000000000000000"
LightShow(74) =  "0000000000000070007000070000000000000000000000000000000000000000"

LightShow(75) =  "0000000000000000000000000000000044400000700000000000000000000000"
LightShow(76) =  "0000000000000000000000000000000077700000700000000000000000000000"
LightShow(77) =  "0000000000000000000000000000000077700000710000000000000000000000"
LightShow(78) =  "0000000000000000000000000000000077700000420000000000000000000000"
LightShow(79) =  "0000000000000001000000110000000044400000240000000000000000000000"
LightShow(80) =  "0000000000000112000011220000000022200000170000000000000000000000"
LightShow(81) =  "0000000000011224011122341100000111100000070000000000000000000000"
LightShow(82) =  "0100001101122347112234572200011200000001060000000001000000000000"
LightShow(83) =  "0200111212234677223357774301122300000111030000000002000000000000"
LightShow(84) =  "0401123323457777345677767512234500001122020000000004000000000001"
LightShow(85) =  "1702335645777764677777537723457700002234010000010007000000000002"
LightShow(86) =  "1703567777777532777754327745777700003467000001120007000000000014"
LightShow(87) =  "2606777777754321765432214577777500006777000112230017000000000117"
LightShow(88) =  "5307776475432210533221102377754300007776000223450024111100001227"
LightShow(89) =  "7207643243221100322110001275432200007653000345770032222200012347"
LightShow(90) =  "7105322222110000211100000143221100005332000567770051333300123574"
LightShow(91) =  "7013221111100000100000000022111000003221000777761061555501235772"
LightShow(92) =  "4012110010000000000000000021100000002110000776531150777702357771"
LightShow(93) =  "2021100000000000000000000010000000001000000754322240777703577741"
LightShow(94) =  "1040000000000000000000000000000000000000000432214320777715777420"
LightShow(95) =  "1070000000000000000000000000000000000000000221107510444427774220"
LightShow(96) =  "0070000000000000000000000000000000000000000111007710222237642110"
LightShow(97) =  "0070000000000000000000000000000000000000000100007700221156421100"
LightShow(98) =  "0050000000000000000000000000000000000000000000005600111174211000"
LightShow(99) =	 "0030000000000000000000000000000000000000000000003300000072110000"
LightShow(100) = "0020000000000000000000000000000000000000000000002200000071100000"
LightShow(101) = "0010000000000000000000000000000000000000000000001100000041000000"
LightShow(102) = "0000000000000000000000000000000000000000000000000100000020000000"
LightShow(103) = "0000000000000000000000000000000000000000000000000000000010000000"
LightShow(104) = "0000000000000000000000000000000000000000000000000000000010000000"

LightShow(105) = "0000000000000000000000100000000000000000000000000000000000000000"
LightShow(106) = "0000000000003751012224761000000000000000000000000000000000000000"
LightShow(107) = "0300000011236643234445663200001100000000000000000000000000000000"
LightShow(108) = "0500003701124432123333442200000100000000000000000000000000000000"
LightShow(109) = "0300156400112224112222231100000000000000000000000000000000000000"
LightShow(110) = "0100244200000143001112570000000000000000000000000000000000000000"
LightShow(111) = "0000232000001310000164002000000200000000000000000002000000000000"
LightShow(112) = "3300100400022000012200001500013100000046000000000001000000000000"
LightShow(113) = "3100045200210000200000000004620000003520000000000000000000000000"
LightShow(114) = "2004520012000000000000000041000000002000000000050000310000000003"
LightShow(115) = "2002000000000000000000000000000000000000000001420000530000000001"
LightShow(116) = "0000000000000000000000000000000000000000000001100000230600000000"
LightShow(117) = "0000000000000000000000000000000000000000000000000000111500000000"
LightShow(118) = "0000000000000000000000000000000000000000000000000000000200000000"
LightShow(119) = "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(120) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(121) = "0000000000000000000000000000000000000000000000000000000010000000"
LightShow(122) = "0000000000000000000000000000000000000000000000000100000020000000"
LightShow(123) = "0010000000000000000000000000000000000000000000001100000031000000"
LightShow(124) = "0020000000000000000000000000000000000000000000002300000031100000"
LightShow(125) = "0030000000000000000000000000000000000000000000003400111123211000"
LightShow(126) = "0030000000000000000000000000000000000000000100003300111113321100"
LightShow(127) = "0020000000000000000000000000000000000000000211102110333302343210"
LightShow(128) = "1010000000000000000000000000000000000000000332111120444401234320"
LightShow(129) = "2001100000000000000000000011000000001100000343320030333300123331"
LightShow(130) = "3002111011000000100000000021110000002111000233430020111100011232"
LightShow(131) = "3103321121110000111100000133211100003322000112330011111100001123"
LightShow(132) = "2203433333211100321111001134332100003433000011120002000000000013"
LightShow(133) = "1302333434332211433321112323343300002333000000110003000000000002"
LightShow(134) = "0301123323343331333433214411233300001122000000000003000000000001"
LightShow(135) = "0200111211233343123334333301112300000011010000000002000000000000"
LightShow(136) = "0100000101112234111123342100011100000000020000000001000000000000"
LightShow(137) = "0000000000001113000111231000000000000000030000000000000000000000"
LightShow(138) = "0000000000000011000001110000000011100000030000000000000000000000"
LightShow(139) = "0000000000000000000000010000000011100000120000000000000000000000"
LightShow(140) = "0000000000000000000000000000000033300000110000000000000000000000"
LightShow(141) = "0000000000000000000000000000000044400000300000000000000000000000"
LightShow(142) = "0000000000000000000000000000000033300000400000000000000000000000"
LightShow(143) = "0000000000000000000000000000000011100000300000000000000000000000"
LightShow(144) = "0000000000000000000000000000000011100000100000000000000000000000"
LightShow(145) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(146) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(147) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(148) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(149) = "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(150) = "7777777777777666777776667777777733307777250777777767777777777777"
LightShow(151) = "7677777777766665777666656777777633207777240777777767777777777777"
LightShow(152) = "7677776676666655766666556677776622207776240777777766777777777777"
LightShow(153) = "7677666666665554666655556677666622207666130777777756777777777777"
LightShow(154) = "7576666566555544655555445566666511106666130777777755777777777776"
LightShow(155) = "6576655565554443555544435566655511106665120777767755777777777776"
LightShow(156) = "6476555455544433544444334465555411106555020776667754777777777776"
LightShow(157) = "6475554454443332444433334455544400005554010766667754777777777765"
LightShow(158) = "5375444344433322433333223354444300005444010666557753666677777665"
LightShow(159) = "5374433333333222333322223344433300004443010655557743666677776664"
LightShow(160) = "4274333333322221322222112243333200004333010555447743666677766554"
LightShow(161) = "4273332232222111222211112233322200003332000554447742555576665553"
LightShow(162) = "3263222222211111221111111132222100003222000444336632555576655443"
LightShow(163) = "3162221121111110111111101122211100002221000433336631444476554442"
LightShow(164) = "2152111111111100111110001121111100002111000333225621444465544332"
LightShow(165) = "2151111111110000111100000111111100001111000332225521333365443321"
LightShow(166) = "1041111011000000100000000011110000001111000222214520333364433221"
LightShow(167) = "1041100000000000000000000011000000001100000221114410222254332221"
LightShow(168) = "1030000000000000000000000000000000000000000111113410222253322210"
LightShow(169) = "1030000000000000000000000000000000000000000111103310111143221110"
LightShow(170) = "0020000000000000000000000000000000000000000110002310111142211110"
LightShow(171) = "0020000000000000000000000000000000000000000100002200111132111100"
LightShow(172) = "0010000000000000000000000000000000000000000000001200000031111000"
LightShow(173) = "0010000000000000000000000000000000000000000000001100000021100000"
LightShow(174) = "0010000000000000000000000000000000000000000000001100000021000000"
LightShow(175) = "0010000000000000000000000000000000000000000000001100000010000000"
LightShow(176) = "0000000000000000000000000000000000000000000000000000000010000000"
LightShow(177) = "0000000000000000000000000000000000000000000000000000000010000000"
LightShow(178) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(179) = "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(180) = "0000000000006556065554440000000000000000000000000000000000000000"
LightShow(181) = "0300766543211001210000001276554300000000000000000006000000000006"
LightShow(182) = "6103332211000000000000000032211156605444040000060052000000000062"
LightShow(183) = "3001111000000000000000000011100033402222420555440031666000065441"
LightShow(184) = "2060000000000000000000000000000012201111310333226620444506543320"
LightShow(185) = "1040000000000000000000000000000011100000100221114510323364332110"
LightShow(186) = "0030000000000000000000000000000000000000100111103300212243221100"
LightShow(187) = "0020000000000000000000000000000000000000000100002200111132111000"
LightShow(188) = "0010000000000000000000000000000000000000000000001200001121110000"
LightShow(189) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(190) = "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(191) = "7000000000000000000000000000000000000000000000005300030031000000"
LightShow(192) = "6007777043100000000000000000000000000000000000004200020021000000"
LightShow(193) = "4707777724566530000000000000000000000000000000003200010010000000"
LightShow(194) = "3606666611112357000000000000000000000000000000002100010000000000"
LightShow(195) = "2504555511112223000000000000000000000000000000001000000000000000"
LightShow(196) = "2403344411111122000000000000000000000000000000000000000000000000"
LightShow(197) = "1302223300111112000000000000000000000000400000000000000000000000"
LightShow(198) = "0201111200001111000000000000000000000000000000000000000000000000"
LightShow(199) = "0100000100000001000000000000000000000000000000000000000000000000"
LightShow(200) = "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(201) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(202) = "0001100021000000000000000000000000000000000000001100000011111111"
LightShow(203) = "0501112255555520153000000000000000000000000000002200000012222110"
LightShow(204) = "0401110066677777040000000000000000000000000000002200000012222210"
LightShow(205) = "0401100055556666030000000000000000000000000000001100000001111110"
LightShow(206) = "0301000034444555020000000000000000000000500000000000000000000000"
LightShow(207) = "0200000022233344010000000000000000000000700000000000000000000000"
LightShow(208) = "0100000011112223000000000000000000000000600000000000000000000000"
LightShow(209) = "0000000000001111000000000000000000000000500000000000000000000000"
LightShow(210) = "0000000000000000000000000000000000000000300000000000000000000000"

LightShow(211) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(212) = "0010000000000000203444324422222300000000000000000020000001111222"
LightShow(213) = "0020000000000000403677776655443200000000000000000130000001222334"
LightShow(214) = "0010000000000000302566665544333200000000000000000020000001112223"
LightShow(215) = "0000000000000000302455554433322277100000000000000010000000001112"
LightShow(216) = "0000000000000000201344443322221176100000000000000000000000000012"
LightShow(217) = "0000000000000000101233332211111163000000000000000000000000000001"
LightShow(218) = "0000000000000000000122221100000053000000000000000000000000000000"
LightShow(219) = "0000000000000000000011110000000042000000000000000000000000000000"
LightShow(220) = "0000000000000000000000000000000031000000000000000000000000000000"

LightShow(221) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(222) = "0000000000000000000000000044320000001000000000002220000002223333"
LightShow(223) = "0010000000000000200452007777777700000112000000003347000013445553"
LightShow(224) = "0000000000000000000045666566666700000000000000002234000002333442"
LightShow(225) = "0000000000000000000022114445555600000000070000001124000001122231"
LightShow(226) = "0000000000000000000011113333444500300000040000000013000000011111"
LightShow(227) = "0000000000000000000011102222333300400000030000000012000000000010"
LightShow(228) = "0000000000000000000000001111122200100000020000000001000000000000"
LightShow(229) = "0000000000000000000000001000011100100000020000000001000000000000"
LightShow(230) = "0000000000000000000000000000000000000000010000000000000000000000"

LightShow(231) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(232) = "0010000000000000000000000011110000004443000222331201000012221000"
LightShow(233) = "0020000000000000000000000010000000006777000334442317000013332100"
LightShow(234) = "0010000000000000000000000000000000005666000233331113000002221100"
LightShow(235) = "0000000000000000000000000000000000004455030222220002000001111000"
LightShow(236) = "0000000000000000000000000000000000003334000111110002000000000000"
LightShow(237) = "0000000000000000000000000000000000002222000000110001000000000000"
LightShow(238) = "0000000000000000000000000000000000001111000000000000000000000000"
LightShow(239) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(240) = "0000000000000000000000000000000000000000000000000000000000000000"

LightShow(241) = "0000000000000000000000000000000000000000000000000000000000000000"
LightShow(242) = "0000000000000000000000000075000000001000000006420000000000000000"
LightShow(243) = "0000000000000000000000000022000000000000000432100050005500000015"
LightShow(244) = "0000000010000000000000000000000000000000000210000030003200065442"
LightShow(245) = "5055430010000000000000000000000000000000000100006610061105543221"
LightShow(246) = "4033321000000000000000000000000000000000000000004410540064322110"
LightShow(247) = "2022210000000000000000000000000000000000000000003300430042211000"
LightShow(248) = "1011110000000000000000000000000000000000000000002200320031110000"
LightShow(249) = "1010100000000000000000000000000000000000000000001100210021000000"
LightShow(250) = "0000000000000000000000000000000000000000000000001100110020000000"
'********************************************************************************** End Light Show *********************************************************************
'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
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
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
End Sub


Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)
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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Dim LeftCount:LeftCount = 0
Sub wirerampleft_hit:playsound "WireRamp":LeftCount = LeftCount + 1:End Sub
Sub wirerampleftdrop_hit
	If LeftCount = 1 then
		playsound "BallDrop"
	End If
	LeftCount = 0
End Sub

Dim RightCount:RightCount = 0
Sub wirerampright_hit:playsound "WireRamp":RightCount = RightCount + 1:End Sub
Sub wireramprightdrop_hit
	If RightCount = 1 then
		playsound "BallDrop"
	End If
	RightCount = 0
End Sub

Sub TiDebug_Timer()
	tbdebug.text = HSCheck
End Sub