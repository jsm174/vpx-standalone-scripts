'
'       | | | |,^.        ,'.      | `.  | |
'       | | |  ,. `.    ,'   `.    | . `.| |
'       | | | .--`,'  ,'  ,^.  `.  | |`.   |
'       | | | |`. `. `. ,'___`. ,' | |  `| |
'       |_| |_|  `. `. `._____,'   |_|   | |
'                  `/                    `.|
'                                          `
'       __               _   _   __         ____  __     _
'       | `. ,^.       ,' | | | |  `.      |  __| | `.  | |
'       | . ' , |  /`,' . | | | | |`.`.    | |__  | . `.| |
'       | |`.'| |  `. ,'| | | | | |  `.`.  |  __| | |`.   |
'       | |   | | ,' . `. | | | | |____` / | |__  | |  `| |
'       |_|   | | \,' `.__| |_| |_______/   `.__| |_|   | |
'             `.|                                       `.|  
'               `                 IRON MAIDEN VIRTUAL TIME` by Franck Hollinger (Aka Mussinger) and Herweh on Codes | 2020
'                                                         
'
' Original Table
' Mod based on Laser War developed by Herweh (2019 for Visual Pinball 10)
'
' Very special thanks to Herweh to let me mod his table, for his special code scripting skills. 
' He added all the fun parts special for IMVT  I was wishing (Ball Save, Eddie Madness mode, Fear of the Dark Mode , sound track changer, and a lot more...)
' Thx for everything he helped and teached to me !!! Very pleased to work with you man !
'
' Special thanks to DCrosby for his work on 3D elements and lights integration help !
' I don't forget everybody who contributed to the Laser War table : Schreibi34, OldSkoolGamer, Francisco666 ICPjuggla, Flupper, Dark, nFozzy, DJRobX, RothbauerW, Mark70, Thalamus, digable.
' the VPX development team: For the continous improvement of VP.
' Vpinball.com For the great website/community.
'
' Big Up for my friends and beta-testers : Olivier S. , Rik L. and specialy to Ludovic D. from MTC who support me and my craziness Night and Day !!!!
' Also :
' - Carlos Andre, a big Iron Maiden Fan, who helped with the Music selection and explained me part of the band story :)
' - My friend David Maldonado for his daily support, graphic advises, brain storming ...
' - JPJ for his help and solutions to some issues.
' - Thanks to virtual pinball Facebook communities, specialy: the frenchy one "Monte Ton Cab" , Scott's "Orbital creators club", "Pinup Popper Artists Association", "VP Nation", "Visual Pinball Addicts", "Visual Pinball Junkies"
'... Sorry if I forget somebody  :)  
'
' >>>>>>>>  I have to thank my wife Vero for the hunderds of hours she let me pass in front of the computer! <<<<<<<<<
'
' Release notes:
' Version 1.0.0: Initial release
' Version 1.0.1: Some bugfixes and improvements
'				 - New voice sample for ball-1-locked
'				 - Voice sample changed Ball Save + Kick Save
'				 - New option to avoid the intro-f***-sample
'				 - Fixed a bug when during ball save mode the multiball was started
'				 - A bit of friction add on the rampe to slowdown ball return
'				 - Added FlexDMD support
'				 - Reworked the LEDs a bit
'
' FOR YOUR INFO:
' If you encounter performance issues, try to set some of the objects to "Static rendering". Open layer 7, select all screws and/or locknuts and select "Static Rendering" in the options window
' Or try to disable "Reflect Elements On Playfield" in the settings.
'

Option Explicit
Randomize

Dim GIColorMod, EnableGI, EnableFlasher, EnableReflectionsAtBall, LetTheBallJump, ShowBallShadow, ShadowOpacityGIOff, ShadowOpacityGIOn, RailsVisible, RampColorMod, ActivateDMD, GameStartSample, EddieMadnessMode, ChangeTrackTargetColor

' ****************************************************
' OPTIONS
' ****************************************************

' GI COLOR MOD  
'   0 = White GI (default)
'   1 = Colored GI
GIColorMod = 0

' SILVER OR BLACK RAMP MOD
'	0 = black
'	1 = silver
'   2 = random
RampColorMod = 1

' ENABLE/DISABLE GI (general illumination)
'	0 = GI is off
'	1 = GI is on (value is a multiplicator for GI intensity - decimal values like 0.7 or 1.33 are valid too)
EnableGI = 1

' ENABLE/DISABLE flasher
'	0 = Flashers are off
'	1 = Flashers are on
EnableFlasher = 1

' ENABLE/DISABLE insert reflections at the ball
'	0 = reflections are off
'	1 = reflections are on
EnableReflectionsAtBall = 1

' SHOW BALL SHADOWS
'	0 = no ball shadows
'	1 = ball shadows are visible
ShowBallShadow = 1

' LET THE BALL JUMP A BIT
'	0 = off
'	1 to 6 = ball jump intensity ( 3 = default, but I like 5 ;-P )
LetTheBallJump = 3

' PLAYFIELD SHADOW INTENSITY DURING GI OFF OR ON (adds additional visual depth) 
' usable range is 0 (lighter) - 100 (darker)  >  On and Off are separated file , not on same scale...
ShadowOpacityGIOff = 75
ShadowOpacityGIOn  = 90

' SIDE RAILS VISIBILITY
'   0 = hide side rails
'   1 = show side rails
'   2 = side rails visible just in desktop mode
RailsVisible = 2

' BALL RELEASE LOCATION FOR EDDIE MADNESS MULTIBALL
'   0 = additional balls will be released at the shooter lane and autoplunged
'   1 = additional balls will be released on the ramp behind Eddie's head (default)
EddieMadnessMode = 1

' VOICE SAMPLE WHEN STARTING A NEW GAME
'   0 = no sample used
'	1 = cheering crowd sample
'	2 = welcome warrior sample
'	3 = F-Bombs  sample (default)
GameStartSample = 3

' DE/ACTIVATE ULTRADMD OR FLEXDMD
'   0 = both deactivated
'   1 = activate UltraDMD (default)
'   2 = activate FlexDMD
ActivateDMD = 1

' COLOR FOR THE 7 CHANGE TRACK TARGETS
'	0 = Yellow targets (default)
'	1 = Different colors
ChangeTrackTargetColor = 0

'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************


' ****************************************************
' standard definitions
' ****************************************************
Const myVersion 	= "1.0.0"
Const UseSolenoids 	= 2
Const UseLamps 		= 0
Const UseSync 		= 0
Const HandleMech 	= 0
Const UseGI			= 1

'Standard Sounds
Const SSolenoidOn 	= "fx_Solenoid"
Const SSolenoidOff 	= ""
Const SFlipperOn 	= ""
Const SFlipperOff 	= ""
Const SCoin 		= "fx_coin"


'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************

Const cDMDRotation 	= -1 			'-1 for No change, 0 - DMD rotation of 0?, 1 - DMD rotation of 90?
Const cGameName 	= "lwar_a83"	'ROM name
Const ballsize 		= 50
Const ballmass 		= 1

If Version < 10600 Then
	MsgBox "This table requires Visual Pinball 10.6 or newer!" & vbNewLine & "Your version: " & Replace(Version/1000,",","."), , "IRON MAIDEN Virtual Time VPX"
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package."
On Error Goto 0

LoadVPM "01550000", "DE.VBS", 3.26

Dim DesktopMode: DesktopMode = IronMaiden.ShowDT
Dim i, bsTrough, mChangeTrack
Dim isGameOver : isGameOver = True
Dim isMultiballRunning : isMultiballRunning = False

If LetTheBallJump > 6 Then LetTheBallJump = 6 : If LetTheBallJump < 0 Then LetTheBallJump = 0
If RampColorMod = 2 Then RampColorMod = Int(Rnd()*2)

' *** Dynamic flipper friction mod ***
Const DynamicFlipperFriction = True
Const DynamicFlipperFrictionResting = 0.4
Const DynamicFlipperFrictionActive = 1.0


' ****************************************************
' MUSIC PLAYER
' ****************************************************
Const TrackStartsWith = "Iron Maiden - "

Dim musicTracks : musicTracks = GetTracks(TrackStartsWith)
Dim isMusicPlayin : isMusicPlayin = False
Dim musicNum : musicNum = 0
Dim isInAttractMode : isInAttractMode = True

Dim isSongFOTDUnlocked : isSongFOTDUnlocked = False
Dim isSongCIPWMUnlocked : isSongCIPWMUnlocked = False

Dim track2LEDs : track2LEDs = 3
Dim isTrack2LEDs : isTrack2LEDs = False
Dim trackFadeTime : trackFadeTime = 1
Sub MusicTimer_Timer()
	If isInAttractMode Then
		' play a randomly selected track
		If Not isMusicPlayin Then MusicStart 0
		' just do a short fade in and fade out
		If trackFadeTime <= 0 Then
			If isFadedIn Then 
				MusicFadeOut
			Else 
				MusicFadeIn
			End If
			trackFadeTime = 4
		End If
		trackFadeTime = trackFadeTime - 1
		If isTrack2LEDs Then RemoveText : isTrack2LEDs = False
	Else
		' play a randomly selected track
		If Not isMusicPlayin Then MusicStart 0
		If track2LEDs <= 0 Then
			track2LEDs = 6 : isTrack2LEDs = True
			If bsTrough.Balls < 3 Then AddText "PLAYIN " & CurrentTrack()
		Else
			track2LEDs = track2LEDs - 1
			If isTrack2LEDs Then RemoveText : isTrack2LEDs = False
		End If
	End If
End Sub

Sub MusicStart(step)
	MusicTimer.Enabled = False
	' maybe no track found so get out
	If Not IsArray(musicTracks) Then Exit Sub
	If UBound(musicTracks) <= -1 Then Exit Sub
	If UBound(musicTracks) = 0 And musicTracks(0) = "" Then Exit Sub
	' get the next track
	If step = 0 Then
		' a random track
		Do While True
			musicNum = RandomTrack()
			If Not ((Not isSongCIPWMUnlocked And UCase(CurrentTrackByID(musicNum)) = "CAN I PLAY WITH MADNESS") Or (Not isSongFOTDUnlocked And UCase(CurrentTrackByID(musicNum)) = "FEAR OF THE DARK")) Then
				Exit Do
			End If
		Loop
	Else
		noChangeTrackInfo = True
		' next or previous track
		Do While True
			musicNum = (musicNum + step) MOD (UBound(musicTracks)+1) : If musicNum < 0 Then musicNum = UBound(musicTracks)
			If hdpCanIPlayWithMadness Or Not (Not isSongCIPWMUnlocked And UCase(CurrentTrackByID(musicNum)) = "CAN I PLAY WITH MADNESS") Then
				Exit Do
			End If
		Loop
		If UCase(CurrentTrackByID(musicNum)) = "FEAR OF THE DARK" Then
			If Not isInBlinderUnlockMode And Not isSongFOTDUnlocked And Not isBlinderUnfolded Then UnfoldBlinder 0, changeTrackTime
		Else
			If isBlinderUnfolded Then ResetBlinder
		End If
	End If
	PlayMusic musicTracks(musicNum)
	isMusicPlayin = True
	If Not isInAttractMode Then 
		MusicOn
	ElseIf Not isFadedIn Then
		MusicOff
	End If
	If isChangeTrackActivated Then AddText CurrentTrack() ': vpmTimer.PulseSw 21
	MusicTimer.Enabled = True
End Sub
Sub MusicStartByTitle(title)
	Dim track, found
	found = False
	ResetPlayedTracks
	For track = 0 To UBound(musicTracks)
		If UCase(title) = UCase(CurrentTrackByID(track)) Then
			found = True
			Exit For
		End If
	Next
	If found Then musicNum = 0 : MusicStart track
End Sub
Sub MusicStop()
	EndMusic
	isMusicPlayin = False
End Sub
Sub MusicEnded()
	isMusicPlayin = False
End Sub
Sub MusicRestart(seconds2Wait)
	MusicRestartTimer.Interval = seconds2Wait * 1000
	MusicRestartTimer.Enabled = True
End Sub

Sub MusicRestartTimer_Timer()
	MusicRestartTimer.Enabled = False
	MusicStart 0
End Sub

Sub SetAttractMode(mode)
	Select Case mode
	Case -2
		' start game key is pressed
		If isInAttractMode Then MusicFadeOutAndStop
	Case -1
		' ball is released
		If Not isMusicPlayin And Not hdpFearOfTheDark And Not hdpCanIPlayWithMadness Then MusicStart 0
	Case 1
		' last ball has drained
		MusicSlowFadeOutAndRestart
	End Select
	isInAttractMode = (mode = 1)
End Sub

Dim musicVol : musicVol = 0
Dim isFadedIn : isFadedIn = False
Sub MusicOff()
	If isMusicPlayin Then MusicVolume = 0
	musicVol = 0
	isFadedIn = False
End Sub
Sub MusicOn()
	If isMusicPlayin Then MusicVolume = 1
	musicVol = 1
	isFadedIn = True
End Sub
Sub MusicFadeIn()
	If Not MusicFadeTimer.Enabled Then
		MusicFadeTimer.Interval = 99 : MusicFadeTimer.Enabled = True : isFadedIn = True
		If Not isSoundWelcomeUnderGlassPlayed Then IronMaidenSound "WelcomeUnderGlass" : isSoundWelcomeUnderGlassPlayed = True
	End If
End Sub
Sub MusicFadeOut()
	If Not MusicFadeTimer.Enabled Then
		MusicFadeTimer.Interval = 100 : MusicFadeTimer.Enabled = True : isFadedIn = False
	End If
End Sub
Sub MusicFadeOutAndStop()
	MusicTimer.Enabled = False
	MusicFadeTimer.Interval = 101 : MusicFadeTimer.Enabled = True : isFadedIn = False
End Sub
Sub MusicSlowFadeOutAndRestart()
	MusicTimer.Enabled = False
	trackFadeTime = 3
	MusicFadeTimer.Interval = 102 : MusicFadeTimer.Enabled = True : isFadedIn = False
End Sub

Sub MusicFadeTimer_Timer()
	If MusicFadeTimer.Interval = 99 Then
		If musicVol < 1 Then musicVol = musicVol + 0.05 Else MusicFadeTimer.Enabled = False : Exit Sub
	ElseIf MusicFadeTimer.Interval = 100 Then
		If musicVol > 0 Then musicVol = musicVol - 0.025 Else MusicFadeTimer.Enabled = False : Exit Sub
	ElseIf MusicFadeTimer.Interval = 101 Then
		If musicVol > 0 Then musicVol = musicVol - 0.04 Else MusicFadeTimer.Enabled = False : MusicStop : Exit Sub
	ElseIf MusicFadeTimer.Interval = 102 Then
		If musicVol > 0 Then musicVol = musicVol - 0.01 Else MusicFadeTimer.Enabled = False : MusicStop : MusicStart 0 : Exit Sub
	Else
		MusicFadeTimer.Enabled = False : Exit Sub
	End If
	If isMusicPlayin Then MusicVolume = musicVol
End Sub

Function CurrentTrack()
	CurrentTrack = Mid(Replace(Replace(musicTracks(musicNum),".mp3",""),TrackStartsWith,"") & Space(14),1,14)
End Function
Function CurrentTrackByID(trackID)
	CurrentTrackByID = Trim(Replace(Replace(musicTracks(trackID),".mp3",""),TrackStartsWith,""))
End Function

Dim isTrackPlayed, isTrackLocked, openTracks
Function RandomTrack()
	Dim ii, track, ret
	' init
	If Not IsArray(isTrackPlayed) Then 
		ReDim isTrackPlayed(UBound(musicTracks))
		ReDim isTrackLocked(UBound(musicTracks))
		ResetPlayedTracks
	ElseIf openTracks <= 0 Then
		ResetPlayedTracks
	End If
	' get next unplayed track
	track = Int(Rnd() * openTracks)
	For ii = 0 To UBound(isTrackPlayed)
		If Not isTrackPlayed(ii) Then
			If track <= 0 Then
				isTrackPlayed(ii) = True
				openTracks = openTracks - 1
				ret = ii
				Exit For
			ElseIf track > 0 Then 
				track = track - 1
			End If
		End If
	Next
	RandomTrack = ret
End Function
Sub ResetPlayedTracks()
	Dim ii
	For ii = 0 To UBound(isTrackPlayed)
		isTrackPlayed(ii) = False
		isTrackLocked(ii) = False
	Next
	openTracks = UBound(isTrackPlayed) + 1
End Sub

Function GetTracks(startsWith)
	Dim fso, f, path, ret(15), index
	' Set fso = CreateObject("Scripting.FileSystemObject")
	' path = UserDirectory & "..\Music"
	' If Not fso.FolderExists(path) Then path = fso.GetAbsolutePathName("..\Music")
	' If fso.FolderExists(path) Then
	' 	index = 0
	' 	For Each f In fso.GetFolder(path).Files
	' 		If UCase(startsWith) = UCase(Left(f.Name,Len(startsWith))) Then
	' 			ReDim Preserve ret(index)
	' 			ret(index) = f.Name
	' 			index = index + 1
	' 		End If
	' 	Next
	' End If
	' If index = 0 Then ReDim Preserve ret(0) : ret(0) = ""
	' Set fso = Nothing
	ret(0) = "Iron Maiden - 2 Minutes To Midnight.mp3"
	ret(1) = "Iron Maiden - 22 Acacia Avenue.mp3"
	ret(2) = "Iron Maiden - Aces High.mp3"
	ret(3) = "Iron Maiden - Can I Play With Madness.mp3"
	ret(4) = "Iron Maiden - Fear Of The Dark.mp3"
	ret(5) = "Iron Maiden - Flight Of Icarus.mp3"
	ret(6) = "Iron Maiden - Futureal.mp3"
	ret(7) = "Iron Maiden - Holy Smoke.mp3"
	ret(8) = "Iron Maiden - Invasion.mp3"
	ret(9) = "Iron Maiden - Lord Of The Flies.mp3"
	ret(10) = "Iron Maiden - Powerslave.mp3"
	ret(11) = "Iron Maiden - Speed Of Light.mp3"
	ret(12) = "Iron Maiden - The Final Frontier.mp3"
	ret(13) = "Iron Maiden - The Number Of The Beast.mp3"
	ret(14) = "Iron Maiden - Wasted Years.mp3"
	GetTracks = ret
End Function

' ****************************************************
' MUSIC PLAYER END
' ****************************************************


' ****************************************************
' table init
' ****************************************************
Sub IronMaiden_Init()
	vpmInit Me
'	NVOffset (2)
	With Controller
        .GameName 			= cGameName
        .SplashInfoLine 	= "IRON MAIDEN Virtual Time (2020)"
		.Games(cGameName).Settings.Value("sound") = 0
		.HandleMechanics 	= False
		.HandleKeyboard 	= False
		.ShowDMDOnly 		= False
		.ShowFrame 			= False
		.ShowTitle 			= False
		.Hidden 			= DesktopMode
		If cDMDRotation >= 0 Then .Games(cGameName).Settings.Value("rol") = cDMDRotation
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
	' initialize some table settings
	InitTable
End Sub

Sub IronMaiden_Paused() : Controller.Pause = True : End Sub
Sub IronMaiden_UnPaused() : Controller.Pause = False : End Sub
Sub IronMaiden_Exit()
	With Controller
		.Games(cGameName).Settings.Value("sound") = 1
		.Stop
	End With
End Sub

Sub IronMaiden_MusicDone()
	MusicEnded
End Sub

Sub InitTable()
	' tilt
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 1
	vpmNudge.TiltObj		= Array(Bumper43, Bumper44, Bumper45, LeftSlingshot, RightSlingshot)
	
	' ball trough
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 0, 12, 11, 10, 0, 0, 0, 0
		.InitKick BallRelease, 90, 10
		.Balls = 3
	End With

	' magnet for change track mode
	Set mChangeTrack = New cvpmMagnet
	With mChangeTrack
		.InitMagnet trChangeTrackMagnet, 10
		.CreateEvents "mChangeTrack"
	End With

	' init lights, flippers, bumpers, ...
	InitLights InsertLights
	InitRamp
	
	' init timers
	PinMAMETimer.Interval 				= PinMAMEInterval
    PinMAMETimer.Enabled  				= True
	LampTimer.Enabled 					= True
	GraphicsTimer.Interval				= 10
	GraphicsTimer.Enabled				= True
	RollingSoundTimer.Interval			= 10
	RollingSoundTimer.Enabled			= True
	GITimer.Interval					= 15
	FlasherTimer.Interval				= 15
	InsertFlasherTimer.Interval			= 15
	InsertLightsFlasherTimer.Interval	= 15
	MusicTimer.Enabled 					= True
	
	' initalise the DMD display
	DMDInit
	
	' rails visibility
	wLeftRail.Visible 	= RailsVisible = 1 Or (RailsVisible = 2 And DesktopMode)
	wRightRail.Visible 	= wLeftRail.Visible

	' display timer
	DisplayTimer.Interval = 25
	DisplayTimer.Enabled  = True
End Sub


' ****************************************************
' some special sounds and effects
' ****************************************************
Sub VolCtrl(volTimeInSeconds)
	If isInAttractMode Or Not isMusicPlayin Then Exit Sub
	MusicVolume = 0.6
	VolTimer.Interval = volTimeInSeconds * 1000
	VolTimer.Enabled = True
End Sub
Sub VolReset()
	If isInAttractMode Or Not isMusicPlayin Then Exit Sub
	MusicVolume = 1 ' music volume back to normal
End Sub
Sub VolTimer_Timer()
	VolReset
	VolTimer.Enabled = False
End Sub

Dim veryFirstBallIsLocked : veryFirstBallIsLocked = False
Sub Check4LockedBall(id)
	If Not isMultiballRunning Then
		If bsTrough.Balls = 1 And id <> 0 And Controller.Switch(id) Then
			Dim countSwitches
			countSwitches = IIF(Controller.Switch(25),1,0) + IIF(Controller.Switch(33),1,0) + IIF(Controller.Switch(38),1,0)
			If countSwitches > 0 Then
				If countSwitches > 1 And veryFirstBallIsLocked Then countSwitches = 1
				IronMaidenSound "Ball" & countSwitches & "IsLocked"
				veryFirstBallIsLocked = False
			End If
		ElseIf bsTrough.Balls = 2 And id = 0 Then
			If Controller.Switch(25) Or Controller.Switch(33) Or Controller.Switch(38) Then
				IronMaidenSound "Ball1IsLockedMale"
				veryFirstBallIsLocked = True
			End If
		End If
	End If
End Sub
Sub Check4RunningMultiball()
	If bsTrough.Balls <= 0 Then
		If Not Controller.Switch(25) And Not Controller.Switch(33) And Not Controller.Switch(38) Then
			IronMaidenSound "CanIPlayMadness"
		End If
	End If
End Sub
Sub Check4FOTDUnlockSong()
	If Not isSongFOTDUnlocked And UCase(CurrentTrackByID(musicNum)) = "FEAR OF THE DARK" Then
		isInBlinderUnlockMode = True
		UnfoldBlinder 0, blinderUnlockSongTimeInSecs
		watchEddieBlinder = True
		MusicStartByTitle "FEAR OF THE DARK"
		DMDFlushAndBlink "black.jpg", "FEAR OF THE DARK UNLOCKED", "CONGRATULATIONS!",  50, 25
	End If
End Sub

Dim lightCheckMode : lightCheckMode = 0
Dim isLightCheckMode1SoundPlayed : isLightCheckMode1SoundPlayed = False
Dim isLightCheckMode2SoundPlayed : isLightCheckMode2SoundPlayed = False
Sub CheckLockIsLit()
	If isLightCheckMode1SoundPlayed Then Exit Sub
	If Controller.Switch(25) Or Controller.Switch(33) Or Controller.Switch(38) Then Exit Sub
	CheckLightTimer.Enabled = False
	lightCheckMode = 1
	CheckLightTimer_Timer
End Sub
Sub CheckExtraBallIsLit()
	If isLightCheckMode2SoundPlayed Then Exit Sub
	CheckLightTimer.Enabled = False
	lightCheckMode = 2
	CheckLightTimer_Timer
End Sub
Const checkLightMaxTimerChecks = 20
Const checkLightMaxTimerInterval = 100
Dim checkLight : Set checkLight = Nothing
Dim checkLightCounter : checkLightCounter = 0
Dim checkLightIsLightOn : checkLightIsLightOn = 0
Sub CheckLightTimer_Timer()
	If lightCheckMode <= 0 Then Exit Sub
	If Not CheckLightTimer.Enabled Then
		checkLightCounter = 0
		checkLightIsLightOn = 0
		CheckLightTimer.Interval = checkLightMaxTimerInterval
		CheckLightTimer.Enabled = True
	End If
	If bsTrough.Balls < 3 And checkLightCounter >= 2*checkLightMaxTimerChecks Then
		If (lightCheckMode = 1 And (l20.State = LightStateOn Or l28.State = LightStateOn Or l36.State = LightStateOn)) Or _
			(lightCheckMode = 2 And l3.State = LightStateOn) Then
			checkLightIsLightOn = checkLightIsLightOn + 1
		End If
	Else
		checkLightIsLightOn = 0
	End If
	checkLightCounter = checkLightCounter + 1
	If checkLightCounter >= 3*checkLightMaxTimerChecks Then
		CheckLightTimer.Enabled = False
		If checkLightIsLightOn / checkLightMaxTimerChecks * 100 > 45 Then
			If lightCheckMode = 1 Then
				If Not isLightCheckMode1SoundPlayed Then IronMaidenSound "LockIsLit" : isLightCheckMode1SoundPlayed = True
			ElseIf lightCheckMode = 2 Then
				If Not isLightCheckMode2SoundPlayed Then IronMaidenSound "ShootAgain" : isLightCheckMode2SoundPlayed = True
			End If
		End If
		lightCheckMode = 0
	End If
End Sub

Dim isSoundWelcomeUnderGlassPlayed : isSoundWelcomeUnderGlassPlayed = False
Sub IronMaidenSound(soundName)
	Select Case soundName
	Case "WelcomeUnderGlass"
		' (one time) before music fade in during attract mode)
		PlaySound "_IMwelcomeUnderGlass"
	Case "Welcome"
		' when start a game, during attract music fade out
		If GameStartSample <> 0 Then PlaySound IIF(GameStartSample=1, "_IMVT-Crowd1", "_IMwelcome" & IIF(GameStartSample=2, "2", ""))
		DMDFlushAndBlink "black.jpg", "ARE YOU READY?", "",  50, 25
	Case "NoCredits"
		' no credits
		VolCtrl 1.5 : PlaySound "_IMNoCredits"
'		AddAutoText " GIMME  MONEY "
	Case "BallSave"
		' ball is saved
		VolCtrl 1.5 : PlaySound "_IMwatchout1"
		AddAutoText "KEEP ONROCKING"
		DMDFlushAndBlink "black.jpg", "KEEP ON ROCKING", "",  60, 30
	Case "KickBack"
		' when left lane kicker act
		VolCtrl 1.5 : PlaySound "_IMwatchout2"
		AddAutoText " KICK   SAVED "
		DMDFlushAndBlink "black.jpg", "SAVED by KICK", "",  60, 30
	Case "FearOfTheDarkBallLost"
		' when loose a ball during FotD
		VolCtrl 5 : PlaySound "_IMTooScared"
		DMDFlushAndBlink "black.jpg", "TOO DARK?", "",  20, 10
	Case "Multiball"
		' when multiball starts
		VolCtrl 2 : PlaySound "_IMmultiball2"
		DMDFlushAndBlink "black.jpg", "MULTIBALL", "",  50, 25
	Case "CanIPlayMadness"
		' when 3rd ball for multiball comes
		VolCtrl 4.5 : PlaySound "_IMcaniPlayMadnessss"
		AddAutoText "GO FOR MADNESS" 
		DMDFlushAndBlink "black.jpg", "SHOOT THE RAMP", "FOR EDDIE MADNESS",  50, 35
	Case "StartEddieMadness"
		' when EddieMadness mode begins > Fade out Music for 24 seconds -- Stop and fade in when out of the mode 
		MusicFadeOutAndStop
		MusicRestart 24.5
		PlaySound "_IMVTcanIplay-solo" : PlaySound "_IMVT-Crowd1" : PlaySound "_IMVT-Crowd2"
		AddAutoText "NEW TRKUNLOCKD"
		DMDFlushAndBlink "black.jpg", "TRACK UNLOCKED", "CONGRATULATIONS",  50, 35
	Case "YouLostMadness"
		' when 'Eddie Madness' mode ends
		VolCtrl 5.5 : PlaySound "_IMyouLostMadness"
		AddAutoText "MADNESS ENDED"
		DMDFlushAndBlink "black.jpg", "EDDIE MADNESS", "ENDED",  60, 25
	Case "LockIsLit"
		' when lock is lit
		VolCtrl 2 : PlaySound "_IMLockIsLite"
		DMDFlushAndBlink "black.jpg", "LOCK IS LIT", "",  50, 20
	Case "Ball1IsLockedMale"
		' when ball 1 is locked
		VolCtrl 2 : PlaySound "_IMLockOne"
		DMDFlushAndBlink "black.jpg", "FIRST BALL", "IS LOCKED",  50, 20
	Case "Ball1IsLocked"
		' when ball 1 is locked
		VolCtrl 2 : PlaySound "_IMLockOneFemal"
		DMDFlushAndBlink "black.jpg", "BALL ONE", "IS LOCKED",  50, 20
	Case "Ball2IsLocked"
		' when ball 2 is locked
		VolCtrl 2 : PlaySound "_IMLockTwoFemal"
		DMDFlushAndBlink "black.jpg", "BALL TWO", "IS LOCKED",  50, 20
	Case "ShootAgain"   ' TODO
		' shoot again
		VolCtrl 2 : PlaySound "_IMshootAgain"
	Case "ExtraTargetsOut"
		' when 7 track chg targets get out
		VolCtrl 2.5 : PlaySound "_IMShootTheTarget"
		AddAutoText " SHOOT TARGETS"
		DMDFlushAndBlink "black.jpg", "SHOOT THE DROPTARGETS", "TO LIT THE MAGNET",  60, 55
	Case "ExtraTargetsHit"
		' when 7 track chg targets hit
		VolCtrl 2.5 : PlaySound "_IMTargetsComplet"
		AddAutoText "GO FOR MAGNET "
		DMDFlushAndBlink "black.jpg", "SHOOT THE MAGNET", "TO CHANGE TRACK",  60, 55
	Case "SelectTrack"
		' in track selection
		VolCtrl 2.5 : PlaySound "_IMSelectTrack"
		DMDFlushAndBlink "black.jpg", "SELECT with FLIPPERS", "PLUNGER TO CONFIRM",  60, 55
	Case "BallLost"
		' random sound when ball is lost
		VolCtrl 3 : PlaySound RndSound(Array("_IMahLostBall","_IMShortFlipper","_IMShortFlipper2","_IMdesapointment","_IMlostAndFare","_IMminePlaying","_IMmusicToMyEars","_IMareYouDead","_IMareYouDead2"))
		DMDFlushAndBlink "black.jpg", "BALL LOST", "",  50, 35
	Case "GameOver"
		' random sound when game is over
		VolCtrl 3 : PlaySound RndSound(Array("_IMartOfFailure","_IMDiesWithBalls","_IMNextGame","_IMsmelsLikeBacon","_IMyouAreBeaten","_IMyouSuck","_IMyouSuck2","_IMyouDiedWithBoots","_IMyouAreWeak"))
		DMDFlushAndBlink "black.jpg", "THIS GAME FINISHED", "WANNA MORE?",  60, 55
	Case "GoodShot"
		' random sound for a good shot
		VolCtrl 3 : PlaySound RndSound(Array("_IMamazing","_IMwellDone","_IMwellDone2","_IMLetsGo","_IMLetsGo2","_IMgoodShot","_IMgreatShot","_IMhot","_IMCarefull","_IMimWaiting1","_IMimWaiting2"))
		DMDFlushAndBlink "black.jpg", "GOOD SHOT", "",  50, 20
	Case "Egg"
		' easter egg
		VolCtrl 4.5 : PlaySound "_IMeggcanIplay" : PlaySound "_IMVT-Crowd1" : PlaySound "_IMVT-Crowd2"
		DMDFlushAndBlink "black.jpg", "Franck and Friends", "say HELLO!",  50, 30
	End Select
End Sub

Function RndSound(sounds())
	RndSound = sounds( Int(Rnd() * (UBound(sounds)+1)) )
End Function


' When Extraball is Win
'		VolCtrl 4500 : PlaySound "_IMextraball" 

' When Bonus X top-lane done
'		VolCtrl 2200 : PlaySound "_IMBonusX" 			'***>>> OR _IMBonusX2 Random ? *** 

' When Extraball ready
'		VolCtrl 2000 : PlaySound "_IMgetExtraball1" 	'*** >>> OR _IMgetExtraball2 Random ? ***

' When Jackpot win when eddie madness active
'		PlaySound "_IMJackpooot" 						'*** >>> OR _IMJackpot2 Random ? *** 

' This one is dificult to put beacause is on the same time than EddyMadness
'_IMJackpoootIsReady


' ****************************************************
' keys
' ****************************************************
Sub IronMaiden_KeyDown(ByVal keycode)
	If keycode = StartGameKey Then CheckKey keycode
	If keycode = PlungerKey Then Plunger.PullBack : PlaySoundAt "plungerpull", Plunger : CheckKey keycode
	If keycode = LeftFlipperKey Then Controller.Switch(47) = True : CheckKey keycode
    If keycode = RightFlipperKey Then Controller.Switch(46) = True : CheckKey keycode
    If keycode = LeftTiltKey Then Nudge 90,2: Playsound SoundFX("fx_nudge",0)
    If keycode = RightTiltKey Then Nudge 270,2: Playsound SoundFX("fx_nudge",0)
    If keycode = CenterTiltKey Then Nudge 0,3: Playsound SoundFX("fx_nudge",0)
'	If keycode = LeftMagnaSave Then RampColorMod = (RampColorMod + 1) MOD 2 : ResetRamp
'	If keycode = RightMagnaSave Then GIColorMod = (GIColorMod + 1) MOD 2 : ResetGI
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub IronMaiden_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Fire : PlaySoundAt "plunger", Plunger
	If keycode = LeftFlipperKey Then Controller.Switch(47) = False
    If keycode = RightFlipperKey Then Controller.Switch(46) = False
    If vpmKeyUp(keycode) Then Exit Sub
End Sub


' ****************************************************
' *** solenoids
' ****************************************************
' flasher
SolCallback(1) 			= "SolFlasherInsert 1," 		' explosion
SolCallback(2) 			= "SolFlasherInsert 2," 		' red hotdog
SolCallback(3) 			= "SolFlasherInsert 3,"			' yellow hotdog"
SolCallback(4) 			= "SolFlasherInsert 4," 		' blue hotdog"
SolCallback(5) 			= "SolFlasherCannon"			' white flasher in cannon tower
SolCallback(6) 			= "SolFlasher 6,"				' yellow flasher
SolCallback(7) 			= "SolFlasher 7,"				' red flasher
SolCallback(8) 			= "SolFlasherMarsBlue"			' blue flasher and right backwall flasher
SolCallback(25) 		= "SolFlasherRampMultiplier"	' ramp multiplier flasher and middle backwall flasher
SolCallback(26) 		= "SolFlasherGreenShield " 		' green shield and left backwall flasher

' tools
SolCallback(9) 			= "SolBallRelease"
SolCallback(11) 		= "SolGI"
SolCallback(12) 		= "SolRedEject"
SolCallback(13) 		= "SolYellowEject"
SolCallback(14) 		= "SolBlueEject"
SolCallback(15) 		= "SolKickback"
SolCallback(16)         = "SolOuthole"
SolCallback(29) 		= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"

' flipper
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"


'************************  Shake Scripting  *************************
'* Code adapted from Cirqus Voltaire from Dozer, Ninuzzu and Nfozzy *
'* Big thank you for this fabulous method, and thx to JPJ to show me*             
'********************************************************************

 Dim mMagnet, cBall, pMod, rmmod

 Set mMagnet = new cvpmMagnet
 With mMagnet
	.InitMagnet WobbleMagnet, 1.5
	.Size = 100
	.CreateEvents mMagnet
	.MagnetOn = True
 End With
 WobbleInit

 Sub RMShake
	cball.velx = cball.velx + rmball.velx*pMod
	cball.vely = cball.vely + rmball.vely*pMod
 End Sub

Sub RMShake2
	cball.velx = cball.velx + activeball.velx*.05
	cball.vely = cball.vely + activeball.vely*.05
 End Sub

Sub RM_Make_Hit()
RMShake2
End Sub

'Includes stripped down version of my reverse slope scripting for a single ball
 Dim ngrav, ngravmod, pslope, nslope, slopemod
 Sub WobbleInit
	pslope = IronMaiden.SlopeMin +((IronMaiden.SlopeMax - IronMaiden.SlopeMin) * IronMaiden.GlobalDifficulty)
	nslope = pslope
	slopemod = pslope + nslope
	ngravmod = 60/WobbleTimer.interval
	ngrav = slopemod * .0905 * IronMaiden.Gravity / ngravmod
	pMod = .1					'percentage of hit power transfered to captive wobble ball
	Set cBall = ckicker.createball:cball.image = "blank":ckicker.Kick 0,0:mMagnet.addball cball
	WobbleTimer.Enabled = True
 End Sub

 Sub WobbleTimer_Timer()
	'BallShake.Enabled = RMBallInMagnet
	cBall.Vely = cBall.VelY-ngrav					'modifier for slope reversal/cancellation
	rmmod = (pEddieGIOn.z+265.5)/265*.3				'.4 is a 40% modifier for ratio of ball movement to head movement
	pEddieGIOn.RotX = 95 + (ckicker.y - cball.y)*rmmod
	pEddieGIOn.RotZ = 5+(cball.x - ckicker.x)*rmmod
 	pEddieGIOff.RotX = pEddieGIOn.RotX
	pEddieGIOff.RotZ = pEddieGIOn.RotZ
 End Sub

' *********************************************************************
' Eddie's rotating head
' *********************************************************************
Dim startingRotZ : startingRotZ = pEddieGIOn.ObjRotZ
Dim startingAngle : startingAngle = 26
Dim rotations : rotations = 2
Sub EddieRotationTimer_Timer()
	If Not EddieRotationTimer.Enabled Then EddieRotationTimer.Enabled = True : PlaySoundAt "fx_loosemetalplate", pEddieGIOff
	If rotations > 0 Then
		If (pEddieGIOn.ObjRotZ + 30) MOD 360 = startingRotZ Then
			rotations = rotations - 1
		End If
		pEddieGIOn.ObjRotZ = pEddieGIOn.ObjRotZ + 30
	ElseIf startingAngle > 1 Then
		pEddieGIOn.ObjRotZ = (pEddieGIOn.ObjRotZ + startingAngle)
		startingAngle = startingAngle - 1
	ElseIf pEddieGIOn.ObjRotZ MOD 360 <> startingRotZ Then
		If Abs(pEddieGIOn.ObjRotZ MOD 360 - startingRotZ) >= 3 Then
			pEddieGIOn.ObjRotZ = pEddieGIOn.ObjRotZ + 2
		Else
			pEddieGIOn.ObjRotZ = pEddieGIOn.ObjRotZ + 1
		End If
	Else
		pEddieGIOn.ObjRotZ = startingRotZ
		startingAngle = 26
		EddieRotationTimer.Enabled = False
	End If
	pEddieGIOff.ObjRotZ = pEddieGIOn.ObjRotZ
End Sub

Sub trEddieRamp_Hit()
	If Not sw29.Enabled Then Exit Sub
	IronMaidenSound "GoodShot"
	ballSaveActive = 0
	' maybe rotate the head
	If Rnd() <= 0.3 Then rotations = 0 : EddieRotationTimer_Timer
End Sub


' ******************************************************
' A lot more 'Iron Maiden mod' code changes
' ******************************************************
Dim ballSaveActive : ballSaveActive = 0
Dim isballSaveActive : isballSaveActive = True
Const ballSaveDuration = 15
Dim ballSaveFlashFreq : ballSaveFlashFreq = 0
Dim ballSaved : Set ballSaved = Nothing
Dim noTopLaneScore : noTopLaneScore = False

Dim startAutoPlunger : startAutoPlunger = False

Sub StartANewGame()
	If credits > 0 Then
		If Not isGameOver Then Exit Sub
		SetAttractMode -2
		isGameOver = False
		Glasss.Visible = False 
		IronMaidenSound "Welcome"
		isSoundWelcomeUnderGlassPlayed = False
		isSongFOTDUnlocked = False
		isSongCIPWMUnlocked = False
	Else
		IronMaidenSound "NoCredits"
	End If
End Sub
Sub StartANewBall()
	RestartCTTimer
	isLightCheckMode1SoundPlayed = False
	isLightCheckMode2SoundPlayed = False
	isInBlinderUnlockMode = False
	noTopLaneScore = False
	isMultiballRunning = False
End Sub
Sub BallIsOver()
	StopCTTimer
	If Not isBlinderUnfolded Then IronMaidenSound "BallLost"
End Sub
Sub GameIsOver()
	If isGameOver Then Exit Sub
	isGameOver = True
	IronMaidenSound "GameOver"
	StopCTMode
	RemoveText
	SetAttractMode 1
	unfoldEddieBlinderTimeInSecs = 0
End Sub

Private Sub CheckKey(keycode)
	If keycode = StartGameKey Then
		StartANewGame
	ElseIf keycode = PlungerKey Then
		If isChangeTrackActivated Then MusicSelectTrack
	ElseIf keycode = LeftFlipperKey Then
		If isChangeTrackActivated Then MusicStart -1
		If isInAttractMode Then CheckHdP "L"
	ElseIf keycode = RightFlipperKey Then
		If isChangeTrackActivated Then MusicStart 1
		If isInAttractMode Then CheckHdP "R"
	End If
End Sub

Dim inputHdP : inputHdP = ""
Dim hdpFearOfTheDark : hdpFearOfTheDark = False
Dim hdpCanIPlayWithMadness : hdpCanIPlayWithMadness = False
Sub CheckHdP(input)
	If musicVol < 0.5 Then Exit Sub
	If Not EddieHDPTimer.Enabled Then inputHdP = "" : EddieHDPTimer.Enabled = True
	inputHdP = inputHdP & input
End Sub
Sub EddieHDPTimer_Timer()
	EddieHDPTimer.Enabled = False
	Select Case inputHdP
	Case "RLRRLRLL"
		IronMaiden_KeyDown StartGameKey
		UnfoldBlinder 0,15
		hdpFearOfTheDark = True
		DMDFlushAndBlink "black.jpg", "IMMEDIATE MODE", "FEAR OF THE DARK",  60, 35
	Case "RLRRLRLLRLRRLRLL"
		IronMaiden_KeyDown StartGameKey
		hdpCanIPlayWithMadness = True
		DMDFlushAndBlink "black.jpg", "IMMEDIATE MODE", "EDDIE MADNESS",  60, 45
	Case "RRRLRRRL"
		rotations = Int(Rnd()*3)+1 : EddieRotationTimer_Timer : HeliSpin.Enabled = True : IronMaidenSound "Egg" : Glasss.Visible = False
	Case "RRLLRRLLRL"
		Glasss.Visible = True : PlaySound "GlassBrkScream"
		DMDFlushAndBlink "black.jpg", "repair LRLRLR", "OH SHIT",  50, 25
	Case "LRLRLR"
		Glasss.Visible = False 
		DMDFlushAndBlink "black.jpg", "OKAY", "COOL",  50, 10
	Case "RLRLRLRRRR"
		ChangeTrackTargetColor = IIF(ChangeTrackTargetColor=1,0,1) : RecolorChangeTrackTargets
		DMDFlushAndBlink "black.jpg", IIF(ChangeTrackTargetColor=1,"COLORED TARGETS","YELLOW TARGETS"), "NICE",  50, 10
	End Select
	inputHdP = ""
End Sub

Sub trHDP_Hit()
	If hdpFearOfTheDark Then
		hdpFearOfTheDark = False
		isInBlinderUnlockMode = True
		UnfoldBlinder 0, blinderUnlockSongTimeInSecs
		watchEddieBlinder = True
		MusicStartByTitle "FEAR OF THE DARK"
	ElseIf hdpCanIPlayWithMadness Then
		trBallSaveStart.TimerInterval = 1 : trBallSaveStart.TimerEnabled = True
		MusicStartByTitle "CAN I PLAY WITH MADNESS"
	End If
End Sub

Dim isEddieMadnessReady2Go : isEddieMadnessReady2Go = False
Dim eddieMadnessReady2GoFlashFreq : eddieMadnessReady2GoFlashFreq = 0
Dim isEddieMadnessActive : isEddieMadnessActive = False
Dim eddieMadnessFlashFreq : eddieMadnessFlashFreq = 0
Dim startEddieMadness : startEddieMadness = False
Dim startEddieMadnessBall2 : startEddieMadnessBall2 = False
Dim isEddieMadnessBall1Created : isEddieMadnessBall1Created = False
Dim isEddieMadnessBall2Created : isEddieMadnessBall2Created = False
Dim stopEddieMadnessReady2Go : stopEddieMadnessReady2Go = True
Dim stopEddieMadness : stopEddieMadness = False

Dim isEddieBlinderReady2Go : isEddieBlinderReady2Go = False
Dim startEddieBlinder : startEddieBlinder = False
Dim waitForEddieBlinderInSecs : waitForEddieBlinderInSecs = 0
Dim unfoldEddieBlinderTimeInSecs : unfoldEddieBlinderTimeInSecs = 0
Dim watchEddieBlinder : watchEddieBlinder = False
Dim eddieBlinderFlashFreq : eddieBlinderFlashFreq = 0

AutoPlunger.PullBack

Dim isBallSaveStartHit : isBallSaveStartHit = False
Sub trBallSaveStart_Hit()
	isBallSaveStartHit = True
	ballSaveActive = 0
	CheckEddieMadnessReady2Go
	If isEddieMadnessBall1Created Or isEddieMadnessBall2Created Then
		Plunger.Pullback
		trBallSaveStart.TimerInterval = 751 : trBallSaveStart.TimerEnabled = True
	ElseIf bsTrough.Balls <= 1 Or hdpFearOfTheDark Or hdpCanIPlayWithMadness Then
		startAutoPlunger = True
	End If
End Sub
Sub trBallSaveStart_Unhit()
	isBallSaveStartHit = False
	ballSaveActive = IIF(bsTrough.Balls>=2 And Not isEddieMadnessBall1Created And Not isEddieMadnessBall2Created,ballSaveDuration,0)
	lBallSave.State = IIf(ballSaveActive>0, LightStateOn, LightStateOff)
End Sub

Sub trEddieMultiballNextBall_Hit()
	If isEddieMadnessBall1Created And Not isEddieMadnessBall2Created Then
		startEddieMadnessBall2 = True
	End If
	If Not isMultiballRunning Then
		If bsTrough.Balls = 0 Then
			isMultiballRunning = True
			IronMaidenSound "Multiball"
		Else
			CheckLockIsLit
		End If
	End If
End Sub
Sub trEddieMultiballNextBall2_Hit()
	If hdpCanIPlayWithMadness Then
		If UBound(GetBalls) < 5 Then
			trBallSaveStart.TimerInterval = 1 : trBallSaveStart.TimerEnabled = True
		End If
	ElseIf isEddieMadnessBall1Created And Not isEddieMadnessBall2Created Then
		startEddieMadnessBall2 = True
	End If
	sw29.Enabled = True
	CheckExtraBallIsLit
End Sub

Sub IronMaidenTimer_Timer()
	' ball save stuff
	If ballSaveActive > 0 Then
		isBallSaveActive = True
		If ballSaveFlashFreq <= 0 Then
			lBallSave.State = IIF(lBallSave.State = LightStateOn, LightStateOff, LightStateOn)
			ballSaveFlashFreq = GetBallSaveFlashFreq
		End If
		ballSaveFlashFreq = ballSaveFlashFreq - 1
		ballSaveActive = ballSaveActive - IronMaidenTimer.Interval / 1000
	Else
		If isBallSaveActive Then lBallSave.State = LightStateOff : isBallSaveActive = False
	End If
	' change track lights
	If wait4ChangeTrackMode <= 5 And Not isChangeTrackModeOn Then
		If changeTrackModeFlashFreq <= 0 Then
			lMagnet.State = IIF(lMagnet.State = LightStateOn, LightStateOff, LightStateOn)
			lChangeTrackDown.State = lMagnet.State
			lChangeTrackUp.State = lMagnet.State
			lChangeTrack1.State = lMagnet.State
			lChangeTrack2.State = lMagnet.State
			lChangeTrack3.State = lMagnet.State
			lChangeTrack4.State = lMagnet.State
			lChangeTrack5.State = lMagnet.State
			lChangeTrack6.State = lMagnet.State
			lChangeTrack7.State = lMagnet.State
			changeTrackModeFlashFreq = GetChangeTrackFlashFreq(wait4ChangeTrackMode)
		End If
		changeTrackModeFlashFreq = changeTrackModeFlashFreq - 1
	End If
	If isChangeTrackModeOn Then
		If changeTrackModeFlashFreq <= 0 Then
			lMagnet.State = LightStateOff
			lChangeTrackDown.State = LightStateOff
			lChangeTrackUp.State = LightStateOff
			areCTLightsOn = Not areCTLightsOn
			SetCTLights IIF(areCTLightsOn, LightStateOff, LightStateOn)
			changeTrackModeFlashFreq = GetChangeTrackFlashFreq(-1)
		End If
		changeTrackModeFlashFreq = changeTrackModeFlashFreq - 1
	End If
	If isChangeTrackMagnetOn Then
		If isChangeTrackActivated Then
			lMagnet.State = LightStateOn
		Else
			If changeTrackModeFlashFreq <= 0 Then
				lMagnet.State = IIF(lMagnet.State = LightStateOn, LightStateOff, LightStateOn)
				changeTrackModeFlashFreq = Int(GetChangeTrackFlashFreq(-1) / 1.5)
			End If
			changeTrackModeFlashFreq = changeTrackModeFlashFreq - 1
		End If
	End If
	If isChangeTrackActivated Then
		If writeChangeTrackActivated Then
			writeChangeTrackActivated = False
			IronMaidenSound "SelectTrack"
			AddText "CHOOSE  TRACK "
		End If
		If changeTrackModeFlashFreq <= 0 Then
			lChangeTrackDown.State = IIF(lChangeTrackDown.State = LightStateOn, LightStateOff, LightStateOn)
			lChangeTrackUp.State = lChangeTrackDown.State
			changeTrackModeFlashFreq = GetChangeTrackFlashFreq(-1)
		End If
		changeTrackModeFlashFreq = changeTrackModeFlashFreq - 1
	End If
	' autoplunge ball
	If startAutoPlunger Then
		startAutoPlunger = False
		trBallSaveStart.TimerInterval = 2 : trBallSaveStart.TimerEnabled = True
	End If
	' Eddie madness multiball stuff
	If startEddieMadness Then
		sw29.Enabled = False
		isEddieMadnessActive = True
		startEddieMadness = False
		trBallSaveStart.TimerInterval = IIF(EddieMadnessMode = 1, 1101, 1) : trBallSaveStart.TimerEnabled = True
		IronMaidenSound "StartEddieMadness"
	End If
	If startEddieMadnessBall2 Then
		sw29.Enabled = False
		startEddieMadnessBall2 = False
		trBallSaveStart.TimerInterval = 1001 : trBallSaveStart.TimerEnabled = True
	End If
	If isEddieMadnessReady2Go Then
		If eddieMadnessReady2GoFlashFreq <= 0 Then
			lEddieMadnessReady2Go.State = IIF(lEddieMadnessReady2Go.State = LightStateOn, LightStateOff, LightStateOn)
			eddieMadnessReady2GoFlashFreq = 15
		End If
		eddieMadnessReady2GoFlashFreq = eddieMadnessReady2GoFlashFreq - 1
	End If
	If isEddieMadnessActive Then
		If eddieMadnessFlashFreq <= 0 Then
			lEddieMadness.State = IIF(lEddieMadness.State = LightStateOn, LightStateOff, LightStateOn)
			eddieMadnessFlashFreq = 10
		End If
		eddieMadnessFlashFreq = eddieMadnessFlashFreq - 1
	End If
	If stopEddieMadnessReady2Go Then
		stopEddieMadnessReady2Go = False
		isEddieMadnessReady2Go = False
		lEddieMadnessReady2Go.State = LightStateOff
	End If
	If stopEddieMadness Then
		stopEddieMadness = False
		isEddieMadnessActive = False
		lEddieMadness.State = LightStateOff
		IronMaidenSound "YouLostMadness"
	End If
	' Eddie's blinder over the flippers
	If isEddieBlinderReady2Go Then
		CheckEddieBlinderReady2Go
	End If
	If startEddieBlinder Then
		startEddieBlinder = False
		If Not isBlinderUnfolded Then
			watchEddieBlinder = True
			waitForEddieBlinderInSecs = 10
			unfoldEddieBlinderTimeInSecs = unfoldEddieBlinderTimeInSecs + 5
			UnfoldBlinder waitForEddieBlinderInSecs, unfoldEddieBlinderTimeInSecs
			If isLeftFlipperUp Or isRightFlipperUp Then RestartBlinderFlipperTimer
		End If
	End If
	If watchEddieBlinder Then
		CheckEddieBlinder
		CheckEddieBlinderLight
	End If
End Sub

Sub trBallSaveStart_Timer()
	' ball save stuff
	If trBallSaveStart.TimerInterval = 1000 Then
		trBallSaveStart.TimerEnabled = False
		PlaySoundAt SoundFX(IIF(bsTrough.Balls>0,"fx_ballrel","fx_solenoid"), DOFContactors), BallRelease
		With ballSaved
			.X = BallRelease.X : .Y = BallRelease.Y : .VelX = 0 : .VelY = 0 : .VelZ = 0
		End With
		Drain.Kick 90, 10
		PlaySoundAt "plungerpull", Plunger
		Plunger.Pullback
		IronMaidenSound "BallSave"
		trBallSaveStart.TimerInterval = 750 : trBallSaveStart.TimerEnabled = True
	ElseIf trBallSaveStart.TimerInterval = 750 Then
		trBallSaveStart.TimerEnabled = False
		StopSound "plungerpull"
		PlaySoundAt "plunger", Plunger
		AutoPlunger.Fire
		Plunger.Fire
		trBallSaveStart.TimerInterval = 100 : trBallSaveStart.TimerEnabled = True
	ElseIf trBallSaveStart.TimerInterval = 100 Then
		trBallSaveStart.TimerEnabled = False
		AutoPlunger.Pullback
		ballSaveActive = 0
	' Eddie madness multiball stuff
	ElseIf trBallSaveStart.TimerInterval = 1 Or trBallSaveStart.TimerInterval = 1001 Or trBallSaveStart.TimerInterval = 1101 Then
		trBallSaveStart.TimerEnabled = False
		If Not isEddieMadnessBall1Created Or Not isEddieMadnessBall2Created Or hdpCanIPlayWithMadness Then
			If hdpCanIPlayWithMadness Then
				PlaySoundAt SoundFX("ferris_ball_drop", DOFContactors), EddieBallRelease
				EddieBallRelease.CreateSizedballWithMass Ballsize/2, Ballmass
				EddieBallRelease.Kick 90, 10
				Exit Sub
			ElseIf EddieMadnessMode = 1 Then
				PlaySoundAt SoundFX("ferris_ball_drop", DOFContactors), EddieBallRelease
				EddieBallRelease.CreateSizedballWithMass Ballsize/2, Ballmass
				EddieBallRelease.Kick 90, 10
			Else
				PlaySoundAt SoundFX("fx_ballrel", DOFContactors), BallRelease
				BallRelease.CreateSizedballWithMass Ballsize/2, Ballmass
				BallRelease.Kick 90, 10
				PlaySoundAt "plungerpull", Plunger
				Plunger.Pullback
			End If
			If Not isEddieMadnessBall1Created Then isEddieMadnessBall1Created = True Else isEddieMadnessBall2Created = True
			trBallSaveStart.TimerInterval = 751 : trBallSaveStart.TimerEnabled = True
		End If
	ElseIf trBallSaveStart.TimerInterval = 751 Then
		trBallSaveStart.TimerEnabled = False
		If EddieMadnessMode <> 1 Then
			StopSound "plungerpull"
			PlaySoundAt "plunger", Plunger
			AutoPlunger.Fire
			Plunger.Fire
		End If
		trBallSaveStart.TimerInterval = 101 : trBallSaveStart.TimerEnabled = True
	ElseIf trBallSaveStart.TimerInterval = 101 Then
		trBallSaveStart.TimerEnabled = False
		If EddieMadnessMode <> 1 Then AutoPlunger.Pullback
		ballSaveActive = 0
	' autoplunge balls
	ElseIf trBallSaveStart.TimerInterval = 2 Then
		PlaySoundAt "plungerpull", Plunger
		Plunger.Pullback
		trBallSaveStart.TimerInterval = 752 : trBallSaveStart.TimerEnabled = True
	ElseIf trBallSaveStart.TimerInterval = 752 Then
		trBallSaveStart.TimerEnabled = False
		PlaySoundAt "plunger", Plunger
		AutoPlunger.Fire
		Plunger.Fire
		trBallSaveStart.TimerInterval = 102 : trBallSaveStart.TimerEnabled = True
	ElseIf trBallSaveStart.TimerInterval = 102 Then
		trBallSaveStart.TimerEnabled = False
		AutoPlunger.Pullback
	End If
End Sub

Sub CheckEddieMadnessReady2Go()
	' activate Eddie Blinder when two or more balls are on the playfield
	isEddieBlinderReady2Go = (bsTrough.Balls <= 1)
	' activate Eddie Madness shot when all three balls are on the playfield
	If Not isEddieMadnessReady2Go Then isEddieMadnessReady2Go = (bsTrough.Balls <= 0)
	' deactivate Eddie madness shot when just one of the starting three balls is on the playfield
	stopEddieMadnessReady2Go = (bsTrough.Balls >= 2) 'Not isEddieMadnessReady2Go
End Sub
Sub CheckEddieMadness()
	If isEddieMadnessReady2Go Then startEddieMadness = True
	If startEddieMadness Then stopEddieMadnessReady2Go = True
End Sub
Sub CheckEddieBlinderReady2Go()
	startEddieBlinder = (bsTrough.Balls >= 2)
	isEddieBlinderReady2Go = Not startEddieBlinder
End Sub
Sub CheckEddieBlinder()
	If bsTrough.Balls >= 3 Then
		watchEddieBlinder = False
		ResetBlinder
		If Not isSongFOTDUnlocked And UCase(CurrentTrackByID(musicNum)) = "FEAR OF THE DARK" Then
			MusicStop
		End If
	ElseIf Not BlinderUnfoldTimer.Enabled Then
		watchEddieBlinder = False
	End If
End Sub
Sub CheckEddieBlinderLight()
	If Not watchEddieBlinder Then 
		lFearOfTheDark.State = LightStateOff
	ElseIf blinderWaitingTime > 0 Or blinderUnfoldTime > 0 Then
		If blinderWaitingTime > 0 Then
			If eddieBlinderFlashFreq <= 0 Then
				lFearOfTheDark.State = IIF(lFearOfTheDark.State = LightStateOn, LightStateOff, LightStateOn)
				eddieBlinderFlashFreq = GetEddieBlinderFlashFreq
			End If
			eddieBlinderFlashFreq = eddieBlinderFlashFreq - 1
		Else
			If lFearOfTheDark.State <> LightStateOn Then lFearOfTheDark.State = LightStateOn
		End If
	End If
End Sub

Function GetBallSaveFlashFreq()
	Dim ret : ret = 15
	Select Case Int(ballSaveActive)+1
	Case 1 : ret = 2
	Case 2 : ret = 4
	Case 3 : ret = 6
	Case 4 : ret = 10
	Case 5 : ret = 12
	End Select
	GetBallSaveFlashFreq = ret
End Function
Function GetEddieBlinderFlashFreq()
	Dim ret : ret = 20
	Select Case Int(blinderWaitingTime)+1
	Case 1 : ret = 2
	Case 2 : ret = 5
	Case 3 : ret = 8
	Case 4 : ret = 12
	Case 5 : ret = 15
	End Select
	GetEddieBlinderFlashFreq = ret
End Function
Function GetChangeTrackFlashFreq(mode)
	Dim ret : ret = 25
	Select Case mode
	Case 0 : ret = 2
	Case 1 : ret = 4
	Case 2 : ret = 7
	Case 3 : ret = 10
	Case 4 : ret = 14
	Case 5 : ret = 20
	End Select
	GetChangeTrackFlashFreq = ret
End Function


' ******************************************************
' change music track
' ******************************************************
Dim isChangeTrackModeOn : isChangeTrackModeOn = False
Dim isChangeTrackMagnetOn : isChangeTrackMagnetOn = False
Dim isChangeTrackMagnetFirstCheck : isChangeTrackMagnetFirstCheck = False
Dim isChangeTrackActivated : isChangeTrackActivated = False
Dim writeChangeTrackActivated : writeChangeTrackActivated = False
Const changeTrackTime = 35
Dim timeChangeTrackActivated : timeChangeTrackActivated = changeTrackTime
Const changeTrackStart = 30
Const changeTrackStartInGame = 180
Dim wait4ChangeTrackMode : wait4ChangeTrackMode = changeTrackStart
Dim changeTrackModeFlashFreq : changeTrackModeFlashFreq = 0
Dim areCTLightsOn : areCTLightsOn = False

RecolorChangeTrackTargets
Sub RecolorChangeTrackTargets()
	swChangeTrack1.Image 	= "Drop-Target" & IIF(ChangeTrackTargetColor = 1, "-P", "")
	lChangeTrack1.Color 	= IIF(ChangeTrackTargetColor = 1, l16.Color, lMagnet.Color)
	lChangeTrack1.ColorFull = IIF(ChangeTrackTargetColor = 1, l16.ColorFull, lMagnet.ColorFull)
	swChangeTrack2.Image 	= swChangeTrack1.Image
	lChangeTrack2.Color 	= lChangeTrack1.Color
	lChangeTrack2.ColorFull = lChangeTrack1.ColorFull
	swChangeTrack3.Image 	= "Drop-Target" & IIF(ChangeTrackTargetColor = 1, "-R", "")
	lChangeTrack3.Color 	= IIF(ChangeTrackTargetColor = 1, l12.Color, lMagnet.Color)
	lChangeTrack3.ColorFull = IIF(ChangeTrackTargetColor = 1, l12.ColorFull, lMagnet.ColorFull)
	swChangeTrack4.Image 	= swChangeTrack3.Image
	lChangeTrack4.Color 	= lChangeTrack3.Color
	lChangeTrack4.ColorFull = lChangeTrack3.ColorFull
	swChangeTrack5.Image 	= "Drop-Target" & IIF(ChangeTrackTargetColor = 1, "-B", "")
	lChangeTrack5.Color 	= IIF(ChangeTrackTargetColor = 1, l24.Color, lMagnet.Color)
	lChangeTrack5.ColorFull = IIF(ChangeTrackTargetColor = 1, l24.ColorFull, lMagnet.ColorFull)
	swChangeTrack6.Image 	= swChangeTrack5.Image
	lChangeTrack6.Color 	= lChangeTrack5.Color
	lChangeTrack6.ColorFull = lChangeTrack5.ColorFull
	swChangeTrack7.Image 	= "Drop-Target" & IIF(ChangeTrackTargetColor = 1, "-T", "")
	lChangeTrack7.Color 	= IIF(ChangeTrackTargetColor = 1, l34.Color, lMagnet.Color)
	lChangeTrack7.ColorFull = IIF(ChangeTrackTargetColor = 1, l34.ColorFull, lMagnet.ColorFull)
End Sub

Sub swChangeTrack1_Hit() : swChangeTrack1.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub
Sub swChangeTrack2_Hit() : swChangeTrack2.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub
Sub swChangeTrack3_Hit() : swChangeTrack3.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub
Sub swChangeTrack4_Hit() : swChangeTrack4.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub
Sub swChangeTrack5_Hit() : swChangeTrack5.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub
Sub swChangeTrack6_Hit() : swChangeTrack6.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub
Sub swChangeTrack7_Hit() : swChangeTrack7.IsDropped = True : vpmTimer.PulseSw 21 : DropTargetHit : End Sub

Sub StartCTMode()
	if bsTrough.Balls = 2 Then
		StopCTMode
		ChangeTrackTimer.Enabled = True
	End If
End Sub

Dim wasCTTimerOn, wasCTWait, wasCTModeOn, wasCTMagnetOn, wasCTTargetDropped(6)
Sub PauseCTMode()
	wasCTTimerOn = ChangeTrackTimer.Enabled
	ChangeTrackTimer.Enabled = False
	wasCTWait = wait4ChangeTrackMode
	wasCTModeOn = isChangeTrackModeOn
	wasCTMagnetOn = isChangeTrackMagnetOn
	wasCTTargetDropped(0) = swChangeTrack1.IsDropped
	wasCTTargetDropped(1) = swChangeTrack2.IsDropped
	wasCTTargetDropped(2) = swChangeTrack3.IsDropped
	wasCTTargetDropped(3) = swChangeTrack4.IsDropped
	wasCTTargetDropped(4) = swChangeTrack5.IsDropped
	wasCTTargetDropped(5) = swChangeTrack6.IsDropped
	wasCTTargetDropped(6) = swChangeTrack7.IsDropped
	StopCTMode
End Sub
Sub UnpauseCTMode()
	If wasCTTimerOn Then
		wait4ChangeTrackMode = wasCTWait
		isChangeTrackModeOn = wasCTModeOn
		isChangeTrackMagnetOn = wasCTMagnetOn
		mChangeTrack.MagnetOn = wasCTMagnetOn
		PlaySoundAt "fx_resetdroptargets", swChangeTrack5
		swChangeTrack1.IsDropped = wasCTTargetDropped(0)
		swChangeTrack2.IsDropped = wasCTTargetDropped(1)
		swChangeTrack3.IsDropped = wasCTTargetDropped(2)
		swChangeTrack4.IsDropped = wasCTTargetDropped(3)
		swChangeTrack5.IsDropped = wasCTTargetDropped(4)
		swChangeTrack6.IsDropped = wasCTTargetDropped(5)
		swChangeTrack7.IsDropped = wasCTTargetDropped(6)
		ChangeTrackTimer.Enabled = wasCTTimerOn
	Else
		StartCTMode
	End If
End Sub

Sub StopCTTimer()
	ChangeTrackTimer.Enabled = False
End Sub
Sub RestartCTTimer()
	ChangeTrackTimer.Enabled = True
End Sub

Sub StopCTMode()
	ChangeTrackTimer.Enabled = False
	' get all off
	wait4ChangeTrackMode = changeTrackStart
	isChangeTrackModeOn = False
	isChangeTrackMagnetOn = False
	isChangeTrackMagnetFirstCheck = False
	isChangeTrackActivated = False
	writeChangeTrackActivated = False
	areCTLightsOn = False
	MoveAllCTTargets False
	SetCTLights LightStateOff
	lMagnet.State = LightStateOff
	lChangeTrackDown.State = LightStateOff
	lChangeTrackUp.State = LightStateOff
	' switch magnet off
	mChangeTrack.MagnetOn = False
End Sub

Sub RestartCTMode()
	StopCTMode
	wait4ChangeTrackMode = changeTrackStartInGame
	ChangeTrackTimer.Enabled = True
End Sub

Sub MusicSelectTrack()
	Controller.Switch(52) = False
	RemoveText
	RestartCTMode
	Check4FOTDUnlockSong
End Sub

Sub MoveAllCTTargets(moveUp)
	Dim obj, areAllDropped
	areAllDropped = True
	For Each obj In CTDropTargets
		If Not obj.IsDropped Then areAllDropped = False : Exit For
	Next
	If (Not areAllDropped And Not moveUp) Or moveUp Then PlaySoundAt "fx_resetdroptargets", swChangeTrack5
	For Each obj In CTDropTargets : obj.IsDropped = Not moveUp : Next
End Sub

Function AreAllCTTargetsDown()
	Dim ret, obj
	ret = True
	For Each obj In CTDropTargets
		If Not obj.IsDropped Then ret = False : Exit For
	Next
	AreAllCTTargetsDown = ret
End Function

Function IsBallInCTMagnet()
	IsBallInCTMagnet = (UBound(mChangeTrack.Balls) > -1)
End Function

Sub SetCTLights(lightState)
	If isChangeTrackModeOn Then
		If Not swChangeTrack1.IsDropped Then lChangeTrack1.State = lightState Else lChangeTrack1.State = LightStateOn
		If Not swChangeTrack2.IsDropped Then lChangeTrack2.State = lightState Else lChangeTrack2.State = LightStateOn
		If Not swChangeTrack3.IsDropped Then lChangeTrack3.State = lightState Else lChangeTrack3.State = LightStateOn
		If Not swChangeTrack4.IsDropped Then lChangeTrack4.State = lightState Else lChangeTrack4.State = LightStateOn
		If Not swChangeTrack5.IsDropped Then lChangeTrack5.State = lightState Else lChangeTrack5.State = LightStateOn
		If Not swChangeTrack6.IsDropped Then lChangeTrack6.State = lightState Else lChangeTrack6.State = LightStateOn
		If Not swChangeTrack7.IsDropped Then lChangeTrack7.State = lightState Else lChangeTrack7.State = LightStateOn
	Else
		Dim obj
		For Each obj In CTLights : obj.State = LightStateOff : Next
	End If
End Sub

Sub ChangeTrackTimer_Timer()
	If Not ChangeTrackTimer.Enabled Then ChangeTrackTimer.Enabled = True
	If wait4ChangeTrackMode > 0 Then
		If Not isBallSaveStartHit Then wait4ChangeTrackMode = wait4ChangeTrackMode - 1
	Else
		If Not isChangeTrackModeOn Then
			isChangeTrackModeOn = True
			MoveAllCTTargets True
			IronMaidenSound "ExtraTargetsOut"
		ElseIf Not isChangeTrackMagnetOn Then
			If AreAllCTTargetsDown() Then
				isChangeTrackMagnetOn = True
				mChangeTrack.MagnetOn = True
				IronMaidenSound "ExtraTargetsHit"
			End If
		ElseIf Not isChangeTrackActivated Then
			If Not IsBallInCTMagnet() Then
				isChangeTrackMagnetFirstCheck = False
			Else
				If Not isChangeTrackMagnetFirstCheck Then
					isChangeTrackMagnetFirstCheck = True
				ElseIf Not isChangeTrackActivated Then
					isChangeTrackMagnetFirstCheck = False
					isChangeTrackActivated = True
					noChangeTrackInfo = False
					timeChangeTrackActivated = changeTrackTime
					writeChangeTrackActivated = True
				End If
			End If
		End If
		If isChangeTrackActivated Then
			If timeChangeTrackActivated > 0 Then
				ChangeTrackAnimationTimer.Enabled = True
				Controller.Switch(52) = True
				If Not noChangeTrackInfo And timeChangeTrackActivated MOD 5 = 4 Then
					AddText "FLIPPERUP/DOWN"
				ElseIf Not noChangeTrackInfo And timeChangeTrackActivated MOD 5 = 3 Then
					AddText "PLUNGERCONFIRM"
				Else
					AddText Left(CurrentTrack(),11+IIF(timeChangeTrackActivated<=9,1,0)) & " " & timeChangeTrackActivated
				End If
				timeChangeTrackActivated = timeChangeTrackActivated - 1
			Else
				MusicSelectTrack
			End If
		End If
	End If
End Sub

Dim noChangeTrackInfo : noChangeTrackInfo = False
Dim changeTrackAnimation : changeTrackAnimation = 0
Sub ChangeTrackAnimationTimer_Timer()
	If Not isChangeTrackActivated Then ChangeTrackAnimationTimer.Enabled = False : newLEDText2 = "" : Exit Sub
	Select Case changeTrackAnimation
	Case 0
		newLEDText2 = "_"
	Case 1
		newLEDText2 = "-"
	Case 2
		newLEDText2 = "^"
	End Select
	refreshLEDs = True
	changeTrackAnimation = changeTrackAnimation + 1 : If changeTrackAnimation > 2 Then changeTrackAnimation = 0
End Sub

Sub trChangeTrackStart_Hit()
	If Not ChangeTrackTimer.Enabled Then StartCTMode
End Sub


' ******************************************************
' outhole, drain and ball release
' ******************************************************
Sub SolOuthole(Enabled)
	If Enabled Then 
		bsTrough.SolIn True
	End If
End Sub
Sub SolBallRelease(Enabled)
	If Enabled Then
		SetAttractMode -1
		PlaySoundAt SoundFX(IIF(bsTrough.Balls>0,"fx_ballrel","fx_solenoid"), DOFContactors), BallRelease
		If Not isballSaveActive Or Controller.Switch(25) Or Controller.Switch(33) Or Controller.Switch(38) Then
			If bsTrough.Balls >= 3 Then 
				StartANewBall
			Elseif bsTrough.Balls = 2 Then
				PauseCTMode
			End If
		End If
		If bsTrough.Balls > 0 Then Check4LockedBall 0 : bsTrough.SolOut True
	End If
End Sub

Sub Drain_Hit()
	BallSearch
	PlaySoundAtVol "drain", Drain, 0.01
	If isEddieMadnessBall2Created Then
		Drain.DestroyBall
		isEddieMadnessBall2Created = False
	ElseIf isEddieMadnessBall1Created Then
		Drain.DestroyBall
		isEddieMadnessBall1Created = False
		stopEddieMadness = True
	ElseIf hdpCanIPlayWithMadness And UBound(GetBalls) > 1 Then
		Drain.DestroyBall
	ElseIf isBallSaveActive Then
		Set ballSaved = ActiveBall
		trBallSaveStart.TimerInterval = 1000 : trBallSaveStart.TimerEnabled = True
		noTopLaneScore = True
	Else
		vpmTimer.PulseSw 13
		bsTrough.AddBall Me
		If isBlinderUnfolded And bsTrough.Balls >= 3 Then IronMaidenSound "FearOfTheDarkBallLost"
		hdpCanIPlayWithMadness = False
		CheckEddieBlinderReady2Go
		CheckEddieMadnessReady2Go
		If bsTrough.Balls >= 3 Then 
			BallIsOver
		ELseIf bsTrough.Balls = 2 Then
			isMultiballRunning = False
			isLightCheckMode1SoundPlayed = False
			UnpauseCTMode
		End If
	End If
End Sub

' find balls that have fallen off the table
Sub BallSearch()
	Dim b
	For Each b In GetBalls
		If b.Y > 2200 Then 
			b.X = 155 : b.Y = 550 : b.VelX = 0 : b.VelY = 0
		End If
	Next 
End Sub


' ****************************************************
' flipper subs
' ****************************************************
Dim isLeftFlipperUp, isRightFlipperUp : isLeftFlipperUp = False : isRightFlipperUp = False

Sub SolLFlipper(Enabled)
	If Enabled Then
		LeftFlipperUp
		isLeftFlipperUp = True : RestartBlinderFlipperTimer
    Else
		LeftFlipperDown
		isLeftFlipperUp = False : StopBlinderFlipperTimer
    End If
End Sub
Sub LeftFlipperUp()
	PlaySoundAtVol SoundFX("fx_flipperup",DOFContactors), LeftFlipper, 2
	If DynamicFlipperFriction Then LeftFlipper.Friction = DynamicFlipperFrictionActive
	LeftFlipper.RotateToEnd
End Sub
Sub LeftFlipperDown()
	PlaySoundAt SoundFX("fx_flipperdown",DOFContactors), LeftFlipper
	If DynamicFlipperFriction Then LeftFlipper.Friction = DynamicFlipperFrictionResting
	LeftFlipper.RotateToStart
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RightFlipperUp
		isRightFlipperUp = True : RestartBlinderFlipperTimer
    Else 
		RightFlipperDown
		isRightFlipperUp = False : StopBlinderFlipperTimer
    End If
End Sub
Sub RightFlipperUp()
	PlaySoundAtVol SoundFX("fx_flipperup",DOFContactors),RightFlipper, 2
	If DynamicFlipperFriction Then RightFlipper.Friction = DynamicFlipperFrictionActive
	RightFlipper.RotateToEnd
End Sub
Sub RightFlipperDown()
	PlaySoundAt SoundFX("fx_flipperdown",DOFContactors),RightFlipper
	If DynamicFlipperFriction Then RightFlipper.Friction = DynamicFlipperFrictionResting
	RightFlipper.RotateToStart
End Sub

' flipper hit sounds
Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 1
End Sub
Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 1
End Sub


' ****************************************************
' sling shots and animations
' ****************************************************
Dim LeftStep, RightStep

Sub LeftSlingShot_Slingshot()
	vpmTimer.PulseSw 15
	PlaySoundAt SoundFX("fx_left_slingshot", DOFContactors), pLeftSlingHammer
    LeftStep = 0
	LeftSlingShot.TimerInterval = 15
	LeftSlingShot_Timer
End Sub
Sub LeftSlingShot_Timer()
    Select Case LeftStep
		Case 0: LeftSling1.Visible = False : LeftSling3.Visible = True : pLeftSlingHammer.TransZ = -28 : LeftSlingShot.TimerEnabled = True
        Case 1: LeftSling3.Visible = False : LeftSling2.Visible = True : pLeftSlingHammer.TransZ = -10
        Case 2: LeftSling2.Visible = False : LeftSling1.Visible = True : pLeftSlingHammer.TransZ = 0 : LeftSlingShot.TimerEnabled = False
    End Select
    LeftStep = LeftStep + 1
End Sub

Sub RightSlingShot_Slingshot()
	vpmTimer.PulseSw 14
	PlaySoundAt SoundFX("fx_right_slingshot", DOFContactors), pRightSlingHammer
    RightStep = 0
	RightSlingShot.TimerInterval = 15
	RightSlingShot_Timer
End Sub
Sub RightSlingShot_Timer()
    Select Case RightStep
		Case 0: RightSling1.Visible = False : RightSling3.Visible = True : pRightSlingHammer.TransZ = -28 : RightSlingShot.TimerEnabled = True
        Case 1: RightSling3.Visible = False : RightSling2.Visible = True : pRightSlingHammer.TransZ = -10
        Case 2: RightSling2.Visible = False : RightSling1.Visible = True : pRightSlingHammer.TransZ = 0 : RightSlingShot.TimerEnabled = False
    End Select
    RightStep = RightStep + 1
End Sub


' ****************************************************
' bumpers
' ****************************************************
Sub Bumper43_Hit() : vpmTimer.PulseSw 43 : PlaySoundAt SoundFX("fx_bumper1",DOFContactors), Bumper43 : HeliSpin.Enabled = True : End Sub
Sub Bumper44_Hit() : vpmTimer.PulseSw 44 : PlaySoundAt SoundFX("fx_bumper2",DOFContactors), Bumper44 : HeliSpin.Enabled = True : End Sub
Sub Bumper45_Hit() : vpmTimer.PulseSw 45 : PlaySoundAt SoundFX("fx_bumper3",DOFContactors), Bumper45 : HeliSpin.Enabled = True : End Sub


' ****************************************************
' switches
' ****************************************************
' stand-up targets
Sub sw22_Hit()   : vpmTimer.PulseSw 22  : StandUpTargetHit : CheckLockIsLit : End Sub
Sub sw23_Hit()   : vpmTimer.PulseSw 23  : StandUpTargetHit : CheckLockIsLit : End Sub
Sub sw24_Hit()   : vpmTimer.PulseSw 24  : StandUpTargetHit : CheckLockIsLit : End Sub
Sub sw30_Hit()   : vpmTimer.PulseSw 30  : StandUpTargetHit : CheckLockIsLit : SlowDownKickback ActiveBall : End Sub
Sub sw31_Hit()   : vpmTimer.PulseSw 31  : StandUpTargetHit : CheckLockIsLit : SlowDownKickback ActiveBall : End Sub
Sub sw32_Hit()   : vpmTimer.PulseSw 32  : StandUpTargetHit : CheckLockIsLit : SlowDownKickback ActiveBall : End Sub
Sub sw35_Hit()   : vpmTimer.PulseSw 35  : StandUpTargetHit : CheckLockIsLit : End Sub
Sub sw36_Hit()   : vpmTimer.PulseSw 36  : StandUpTargetHit : CheckLockIsLit : End Sub
Sub sw37_Hit()   : vpmTimer.PulseSw 37  : StandUpTargetHit : CheckLockIsLit : End Sub

' top lanes
Sub sw40_Hit()
	If Not noTopLaneScore Then Controller.Switch(40) = True
	RollOverSound
	noTopLaneScore = False
End Sub
Sub sw40_Unhit() : Controller.Switch(40) = False : End Sub
Sub sw41_Hit()
	If Not noTopLaneScore Then Controller.Switch(41) = True
	RollOverSound
	noTopLaneScore = False
End Sub
Sub sw41_Unhit() : Controller.Switch(41) = False : End Sub
Sub sw42_Hit()
	If Not noTopLaneScore Then Controller.Switch(42) = True
	RollOverSound
	noTopLaneScore = False
End Sub
Sub sw42_Unhit() : Controller.Switch(42) = False : End Sub

' rollover stars
Sub sw26_Hit()   : Controller.Switch(26) = True  : RollOverSound : End Sub
Sub sw26_Unhit() : Controller.Switch(26) = False : End Sub
Sub sw34_Hit()   : Controller.Switch(34) = True  : RollOverSound : End Sub
Sub sw34_Unhit() : Controller.Switch(34) = False : End Sub
Sub sw39_Hit()   : Controller.Switch(39) = True  : RollOverSound : End Sub
Sub sw39_Unhit() : Controller.Switch(39) = False : End Sub
Sub sw48_Hit()   : Controller.Switch(48) = True  : RollOverSound : End Sub
Sub sw48_Unhit() : Controller.Switch(48) = False : End Sub
Sub sw49_Hit()   : Controller.Switch(49) = True  : RollOverSound : End Sub
Sub sw49_Unhit() : Controller.Switch(49) = False : End Sub
Sub sw50_Hit()   : Controller.Switch(50) = True  : RollOverSound : End Sub
Sub sw50_Unhit() : Controller.Switch(50) = False : End Sub

' kickback at left outlane
Sub sw17_Hit()   : Controller.Switch(17) = True  : RollOverSound : HeliSpin.Enabled = True : End Sub
Sub sw17_Unhit() : Controller.Switch(17) = False : End Sub

' inlanes and outlanes
Sub sw18_Hit()   : Controller.Switch(18) = True  : RollOverSound : End Sub
Sub sw18_Unhit() : Controller.Switch(18) = False : End Sub
Sub sw19_Hit()   : Controller.Switch(19) = True  : RollOverSound : End Sub
Sub sw19_Unhit() : Controller.Switch(19) = False : End Sub
Sub sw20_Hit()   : Controller.Switch(20) = True  : RollOverSound : End Sub
Sub sw20_Unhit() : Controller.Switch(20) = False : End Sub

' shooter lane
Sub sw52_Hit()   : Controller.Switch(52) = True  : RollOverSound : End Sub   
Sub sw52_UnHit() : Controller.Switch(52) = False : isBallMovedStraightUp = False : End Sub

' ramp trigger
Sub sw29_Hit()   : Controller.Switch(29) = True  : RollOverSound : CheckEddieMadness : End Sub
Sub sw29_Unhit() : Controller.Switch(29) = False : End Sub

' spinners
Sub sw27_Spin()  : vpmTimer.PulseSw 27 : PlaySoundAt "fx_spinner", sw27 : HeliSpin.Enabled = True : End Sub
Sub sw51_Spin()  : vpmTimer.PulseSw 51 : PlaySoundAt "fx_spinner", sw51 : HeliSpin.Enabled = True : End Sub

Sub rRubberBand5_Hit() : vpmTimer.PulseSw 21 : rRubberBand5_Timer : End Sub
Sub rRubberBand5_Timer()
	If Not rRubberBand5.TimerEnabled Then rRubberBand5.TimerEnabled = 20 : rRubberBand5.TimerEnabled = True
	If rRubberBand5.Visible Then
		rRubberBand5.Visible  		= False
		rRubberBand5a.Visible 		= True
		SwitchSound sw50
	Else
		rRubberBand5.Visible  		= True
		rRubberBand5a.Visible 		= False
		rRubberBand5.TimerEnabled 	= False
	End If
End Sub
Sub rRubberBand6_Hit() : vpmTimer.PulseSw 21 : rRubberBand6_Timer : End Sub
Sub rRubberBand6_Timer()
	If Not rRubberBand6.TimerEnabled Then rRubberBand6.TimerEnabled = 20 : rRubberBand6.TimerEnabled = True
	If rRubberBand6.Visible Then
		rRubberBand6.Visible  		= False
		rRubberBand6a.Visible 		= True
		SwitchSound sw49
	Else
		rRubberBand6.Visible  		= True
		rRubberBand6a.Visible 		= False
		rRubberBand6.TimerEnabled 	= False
	End If
End Sub

' add some variation to the ball direction under the top lanes
Dim isBallMovedStraightUp : isBallMovedStraightUp = False
Sub sw40Dir_Hit(): MoveBallUnderTopLane ActiveBall : End Sub
Sub sw41Dir_Hit(): MoveBallUnderTopLane ActiveBall : End Sub
Sub sw42Dir_Hit(): MoveBallUnderTopLane ActiveBall : End Sub
Sub MoveBallUnderTopLane(movingBall)
	With movingBall
		Do While True
			.VelX = .VelX + Rnd() * 4 - 2
			If Abs(.VelX) > 0.1 Then Exit Do
			If Not isBallMovedStraightUp Then isBallMovedStraightUp = True : Exit Do
		Loop
	End With
End Sub


' ****************************************************
' rotating spinner
' ****************************************************
Sub SpinnerTimer_Timer()
	pSpinner27.RotX 		= 360 - sw27.CurrentAngle
    pSpinnerRod27.TransZ 	= -Sin(sw27.CurrentAngle * 2 * 3.14 / 360) * 5
    pSpinnerRod27.TransX 	= Sin((sw27.CurrentAngle - 90) * 2 * 3.14 / 360) * -5
	pSpinner51.RotX 		= 360 - sw51.CurrentAngle
    pSpinnerRod51.TransZ 	= -Sin(sw51.CurrentAngle * 2 * 3.14 / 360) * 5
    pSpinnerRod51.TransX 	= Sin((sw51.CurrentAngle - 90) * 2 * 3.14 / 360) * -5
End Sub


' ****************************************************
' saucer
' ****************************************************
Dim sw25Step : sw25Step = 0
Dim sw33Step : sw33Step = 0
Dim sw38Step : sw38Step = 0
Dim sw25Ball : Set sw25Ball = Nothing
Dim sw33Ball : Set sw33Ball = Nothing
Dim sw38Ball : Set sw38Ball = Nothing

Sub sw25_Hit()   : PlaySoundAt "fx_saucer_enter", sw25 : CheckSwitch sw25 : SlowDownBall ActiveBall : End Sub
Sub sw25_Unhit() : Controller.Switch(25) = False : End Sub
Sub sw33_Hit()   : PlaySoundAt "fx_saucer_enter", sw33 : CheckSwitch sw33 : SlowDownBall ActiveBall : End Sub
Sub sw33_Unhit() : Controller.Switch(33) = False : End Sub
Sub sw38_Hit()   : PlaySoundAt "fx_saucer_enter", sw38 : CheckSwitch sw38 : SlowDownBall ActiveBall : End Sub
Sub sw38_Unhit() : Controller.Switch(38) = False : End Sub

Sub SolRedEject(Enabled)
	If Enabled Then
		ballSaveActive = 0
		MoveHammer pSaucer25Hammer, 0
		sw25Step 			= 0
		sw25.TimerInterval 	= 11
		sw25.TimerEnabled 	= True
	End If
End Sub
Sub SolYellowEject(Enabled)
	If Enabled Then
		ballSaveActive = 0
		MoveHammer pSaucer33Hammer, 0
		sw33Step 			= 0
		sw33.TimerInterval 	= 11
		sw33.TimerEnabled 	= True
	End If
End Sub
Sub SolBlueEject(Enabled)
	If Enabled Then
		ballSaveActive = 0
		MoveHammer pSaucer38Hammer, 0
		sw38Step 			= 0
		sw38.TimerInterval 	= 11
		sw38.TimerEnabled 	= True
	End If
End Sub

Sub sw25_Timer()
	If sw25.TimerInterval = 500 Then
		sw25.TimerEnabled = False
		Check4LockedBall 25
	Else
		SaucerAction sw25Step, pSaucer25Hammer, sw25Ball, 25, sw25, 180, 10
		sw25Step = sw25Step + 1
	End If
End Sub
Sub sw33_Timer()
	If sw33.TimerInterval = 500 Then
		sw33.TimerEnabled = False
		Check4LockedBall 33
	Else
		SaucerAction sw33Step, pSaucer33Hammer, sw33Ball, 33, sw33, 80, 18
		sw33Step = sw33Step + 1
	End If
End Sub
Sub sw38_Timer()
	If sw38.TimerInterval = 500 Then
		sw38.TimerEnabled = False
		Check4LockedBall 38
	Else
		SaucerAction sw38Step, pSaucer38Hammer, sw38Ball, 38, sw38, 180, 14
		sw38Step = sw38Step + 1
	End If
End Sub

Sub SaucerAction(step, pHammer, kBall, id, switch, kickangle, kickpower)
	Select Case step
		Case 0		: MoveHammer pHammer, -1 : PlaySoundAt SoundFX("fx_vuk_exit", DOFContactors), switch
		Case 1		: MoveHammer pHammer, 16 : If Controller.Switch(id) Then KickBall kBall, kickangle, 10, kickpower, 5, 30
		Case 13		: Controller.Switch(id) = False : Check4RunningMultiball : CheckLockIsLit
		Case 25,26,27,28,29 : MoveHammer pHammer, -2
		Case 30 	: MoveHammer pHammer,  0 : switch.TimerEnabled = False
		Case Else 	: ' nothing to do
	End Select
End Sub

Sub KickBall(kBall, kAngle, kAngleVar, kVel, kVelZ, kLiftZ)
	Dim rAngle
	rAngle 	= 3.14159265 * (kAngle + (kAngleVar / 2 - kAngleVar * Rnd()) - 90) / 180
	kVel	= kVel + (kVel/20 - kVel/10 * Rnd())
	kVelZ	= kVelZ + (kVelZ/20 - kVelZ/10 * Rnd())
	With kBall
		.Z = .Z + kLiftZ
		.VelZ = kVelZ
		.VelX = cos(rAngle) * kVel
		.VelY = sin(rAngle) * kVel
	End With
End Sub

Sub MoveHammer(hammer, rotZ)
	If rotZ = 0 Then
		hammer.RotZ = 0
	Else
		hammer.RotZ = hammer.RotZ + rotZ
	End If
End Sub

Sub CheckSwitch(switch)
	switch.TimerInterval = 500
	switch.TimerEnabled  = True
End Sub
Sub SlowDownBall(actBall)
	With actBall
		If .VelY < 0 Then .VelY = .VelY / 2 : .VelX = .VelX / 2
	End With
End Sub


' ****************************************************
' kick back
' ****************************************************
Sub SolKickback(Enabled)
   	Kickback.Enabled = Enabled
End Sub 

Dim kickbackBallVel : kickbackBallVel = 1
Dim kickbackBall    : Set kickbackBall = Nothing
Sub Kickback_Hit()
	Kickback.TimerEnabled  = False
	kickbackBallVel  = BallVel(ActiveBall)
	Set kickbackBall = ActiveBall
	PlaySoundAt SoundFX("fx_solenoid", DOFContactors), Kickback
	Kickback.TimerInterval = 40
   	Kickback.TimerEnabled  = True
	ballSaveActive = 0
	IronMaidenSound "KickBack"
End Sub
Sub Kickback_Timer()
	If Kickback.TimerInterval = 40 Then
		Kickback.Kick 0, Int(35 + kickbackBallVel*2/3 + Rnd()*20)
		kickbackBall.VelX = 0
		PlaySoundAt SoundFX("plunger", DOFContactors), Kickback
		pKickback.TransY = 0
		Kickback.TimerInterval = 20
	ElseIf Kickback.TimerInterval = 20 Then
		pKickback.TransY = pKickback.TransY + 10
		If pKickback.TransY = 50 Then Kickback.TimerInterval = 500
	ElseIf Kickback.TimerInterval = 500 Then
		Kickback.TimerInterval = 50
	ElseIf Kickback.TimerInterval = 50 Then
		pKickback.TransY = pKickback.TransY - 5
		If pKickback.TransY = 0 Then Kickback.TimerInterval = 1000
	ElseIf Kickback.TimerInterval = 1000 Then
		Kickback.TimerEnabled = False
	End If
End Sub

Sub SlowDownKickback(kickedBall)
	If Kickback.TimerEnabled Then
		kickedBall.VelX = kickedBall.VelX / Int(9+Rnd()*4)
	End If
End Sub


' *********************************************************************
' lamps and illumination
' *********************************************************************
' inserts
Dim PFLights(200,3), PFLightsCount(200), isGIOn, resetAddLights
resetAddLights = False

Sub InitLights(aColl)
	' init inserts
	Dim obj, idx
	For Each obj In aColl
		idx = obj.TimerInterval
		Set PFLights(idx, PFLightsCount(idx)) = obj
		PFLightsCount(idx) = PFLightsCount(idx) + 1
		If EnableReflectionsAtBall = 0 And Right(obj.Name,1) = "a" Then obj.IntensityScale = 0
	Next
	' init flasher
	InitFlasher
	' init GI
	InitGI True
End Sub
Sub LampTimer_Timer()
	Dim chgLamp, num, chg, ii, nr, xxx, obj
	xxx = -1
    chgLamp = Controller.ChangedLamps
	If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
			Select Case chglamp(ii,0)
			Case 9
				SetTowerLight 1, (chgLamp(ii,1) <> 0)
			Case 10
				SetTowerLight 2, (chgLamp(ii,1) <> 0)
			Case 11
				SetTowerLight 3, (chgLamp(ii,1) <> 0)
			Case 49
				SetSomeTools chgLamp(ii,1)
			Case Else
				For nr = 1 to PFLightsCount(chgLamp(ii,0)) : PFLights(chgLamp(ii,0),nr - 1).State = chgLamp(ii,1) : Next
				For Each obj In InsertLightsFlasher
					If obj.TimerInterval = chgLamp(ii,0) Then
						If chgLamp(ii,1) = 0 Then
							obj.IntensityScale = 0.93 : InsertLightsFlasherTimer.Enabled = True
						ElseIf chgLamp(ii,1) = 1 Then
							obj.IntensityScale = 0.1 : InsertLightsFlasherTimer.Enabled = True
						End If
					End If
				Next
				If bsTrough.Balls >= 3 Then
					resetAddLights = True
					Select Case chglamp(ii,0) 
					Case  3 : lBallSave.State = l3.State
					Case  7 : lEddieMadness.State = l7.State : lFearOfTheDark.State = l7.State
					Case 12 : lEddieMadnessReady2Go.State = l12.State
					Case 16 : lChangeTrack1.State = l16.State
					Case 18 : lChangeTrack2.State = l18.State
					Case 23 : lChangeTrack3.State = l23.State
					Case 31 : lChangeTrack4.State = l31.State
					Case 24 : lChangeTrack5.State = l24.State
					Case 26 : lChangeTrack6.State = l26.State : lChangeTrackDown.State = l26.State
					Case 34 : lChangeTrack7.State = l34.State
					Case 32 : lChangeTrackUp.State = l32.State
					End Select
					lMagnet.State = (lChangeTrackDown.State Or lChangeTrackUp.State)
				ElseIf resetAddLights Then
					resetAddLights = False
					lBallSave.State = LightStateOff
					lEddieMadness.State = LightStateOff
					lEddieMadnessReady2Go.State = LightStateOff
					lFearOfTheDark.State = LightStateOff
					lChangeTrack1.State = LightStateOff
					lChangeTrack2.State = LightStateOff
					lChangeTrack3.State = LightStateOff
					lChangeTrack4.State = LightStateOff
					lChangeTrack5.State = LightStateOff
					lChangeTrack6.State = LightStateOff
					lChangeTrack7.State = LightStateOff
					lMagnet.State = LightStateOff
					lChangeTrackDown.State = LightStateOff
					lChangeTrackUp.State = LightStateOff
				End If
			End Select
        Next
	End If
End Sub

Sub InsertLightsFlasherTimer_Timer()
	Dim obj, setTimerOff
	setTimerOff = True
	For Each obj In InsertLightsFlasher
		If obj.IntensityScale = 0.93 Or obj.IntensityScale = 0.62 Or obj.IntensityScale = 0.31 Then
			obj.IntensityScale = obj.IntensityScale - 0.31
			If obj.IntensityScale > 0 Then setTimerOff = False
		End If
		If obj.IntensityScale = 0.1 Or obj.IntensityScale = 0.4 Or obj.IntensityScale = 0.7 Then
			obj.IntensityScale = obj.IntensityScale + 0.3
			If obj.IntensityScale < 1 Then setTimerOff = False
		End If
	Next
	If setTimerOff Then InsertLightsFlasherTimer.Enabled = False
End Sub

Dim isWhiteFlasherOn : isWhiteFlasherOn = False
Sub SetTowerLight(mode, isLightOn)
	Dim obj, coll
	Select Case mode
	Case 1
		If Not isWhiteFlasherOn Then
			For Each obj In GIRedTowerLights : obj.State = isLightOn : Next
		End If
	Case 2
		If Not isWhiteFlasherOn Then
			For Each obj In GIYellowTowerLights : obj.State = isLightOn : Next
		End If
	Case 3
		If Not isWhiteFlasherOn Then
			For Each obj In GIBlueTowerLights : obj.State = isLightOn : Next
		End If
	Case 4
		isWhiteFlasherOn = isLightOn
		If isLightOn Then
			For Each coll In Array(GIRedTowerLights,GIYellowTowerLights,GIBlueTowerLights)
				For Each obj In coll : obj.State = LightStateOff : Next
			Next
		End If
		For Each obj In GIWhiteTowerLights : obj.State = isLightOn : Next
	End Select
End Sub

' flasher
Const minFlasherNo = 1
Const maxFlasherNo = 13
ReDim fValue(maxFlasherNo,6) : For i = minFlasherNo To maxFlasherNo : fValue(i,0) = 0 : Next

Sub InitFlasher()
	' playfield
	fValue(1,5) 		= Array(fLight1a,fLight1b)
	fValue(1,6) 		= InsertFlasher
	fValue(2,5) 		= Array(fLight2a,fLight2b,fLight2c,fLight2d)
	fValue(2,6) 		= InsertFlasher
	fValue(3,5) 		= Array(fLight3a,fLight3b,fLight3c,fLight3d)
	fValue(3,6) 		= InsertFlasher
	fValue(4,5) 		= Array(fLight4a,fLight4b,fLight4c,fLight4d)
	fValue(4,6) 		= InsertFlasher
	fValue(6,1) 		= Array(fFlash6A,fFlash6B)
	fValue(6,2) 		= Array(fLit6A,fLit6B)
	fValue(6,3) 		= Array(fBase6A,fBase6B)
	fValue(6,4) 		= Array(fLight6A,fLight6Aa,fLight6B,fLight6Ba)
	fValue(6,6)			= YellowFlasher
	fValue(7,1) 		= Array(fFlash7A,fFlash7B)
	fValue(7,2) 		= Array(fLit7A,fLit7B)
	fValue(7,3) 		= Array(fBase7A,fBase7B)
	fValue(7,4) 		= Array(fLight7A,fLight7Aa,fLight7B,fLight7Ba)
	fValue(7,6)			= RedFlasher
	fValue(8,1) 		= Array(fFlash8A)
	fValue(8,2) 		= Array(fLit8A)
	fValue(8,3) 		= Array(fBase8A)
	fValue(8,4) 		= Array(fLight8A,fLight8Aa)
	fValue(8,6)			= BlueFlasher
	fValue(9,1) 		= Array(fFlash8B)
	fValue(9,2) 		= Array(fLit8B)
	fValue(9,3) 		= Array(fBase8B)
	fValue(9,4) 		= Array(fLight8B,fLight8Ba,fLight8Bb)
	fValue(9,6)			= WhiteFlasher
	fValue(10,5) 		= Array(fLight9Aa,fLight9Ab,fLight9Ac)
	fValue(10,6) 		= InsertFlasher
	fValue(11,1) 		= Array(fFlash9B)
	fValue(11,2) 		= Array(fLit9B)
	fValue(11,3) 		= Array(fBase9B)
	fValue(11,4) 		= Array(fLight9B,fLight9Ba,fLight9Bb)
	fValue(11,6)		= WhiteFlasher
	fValue(12,5) 		= Array(fLight10Aa,fLight10Ab,fLight10Ac)
	fValue(12,6) 		= InsertFlasher
	fValue(13,1) 		= Array(fFlash10B)
	fValue(13,2) 		= Array(fLit10B)
	fValue(13,3) 		= Array(fBase10B)
	fValue(13,4) 		= Array(fLight10B,fLight10Ba,fLight10Bb)
	fValue(13,6)		= WhiteFlasher
	' start flasher timer
	FlasherTimer.Interval 		= 15
	InsertFlasherTimer.Interval = 15
	If Not FlasherTimer.Enabled Then FlasherTimer_Timer
End Sub

Sub SolFlasher(flasherNo, flasherValue)
	If EnableFlasher = 0 Then Exit Sub
	' set value
	fValue(flasherNo,0) = 100
	' start flasher timer
	If Not FlasherTimer.Enabled Then FlasherTimer_Timer
End Sub
Sub SolFlasherInsert(flasherNo, flasherValue)
	If EnableFlasher = 0 Then Exit Sub
	' set value
	fValue(flasherNo,0) = 100
	' start flasher timer
	If Not InsertFlasherTimer.Enabled Then InsertFlasherTimer_Timer
End Sub
Sub SolFlasherMarsBlue(flasherValue)
	SolFlasherInsert 8, flasherValue
	SolFlasher 9, flasherValue
End Sub
Sub SolFlasherRampMultiplier(flasherValue)
	SolFlasherInsert 10, flasherValue
	SolFlasher 11, flasherValue
End Sub
Sub SolFlasherGreenShield(flasherValue)
	SolFlasherInsert 12, flasherValue
	SolFlasher 13, flasherValue
End Sub
Sub SolFlasherCannon(flasherValue)
	SetTowerLight 4, flasherValue
End Sub

Sub FlasherTimer_Timer()
	Dim ii, allZero, flashx3, matdim, obj
	allZero = True
	If Not FlasherTimer.Enabled Then FlasherTimer.Enabled = True
	For ii = minFlasherNo To maxFlasherNo
		If (IsObject(fValue(ii,1)) Or IsArray(fValue(ii,1)) Or IsObject(fValue(ii,4)) Or IsArray(fValue(ii,4))) And fValue(ii,0) >= 0 Then
			allZero = False
			' decrease flasher value
			If fValue(ii,0) = 99 Then 
				fValue(ii,0) = 1
				allZero= True
			Else
				If fValue(ii,0) = 100 Then fValue(ii,0) = 1 Else fValue(ii,0) = fValue(ii,0) * fDecrease(fValue(ii,6)) - 0.01
			End If
			' maybe show some flasher objects
			If IsObject(fValue(ii,1)) Then
				If Not fValue(ii,1).Visible Then fValue(ii,1).Visible = True
			ElseIf IsArray(fValue(ii,1)) Then
				For Each obj In fValue(ii,1) : If Not obj.Visible Then obj.Visible = True : End If : Next
			End If
			If IsObject(fValue(ii,2)) Then 
				fValue(ii,2).Visible = True
			ElseIf IsArray(fValue(ii,2)) Then
				For Each obj In fValue(ii,2) : obj.Visible = True : Next
			End If
			' calc values
			flashx3 = fValue(ii,0) ^ 3
			matdim 	= Round(10 * fValue(ii,0))
			' set flasher object values
			If IsObject(fValue(ii,1)) Then 
				fValue(ii,1).Opacity = fOpacity(fValue(ii,6)) * flashx3
			ElseIf IsArray(fValue(ii,1)) Then
				For Each obj In fValue(ii,1) : obj.Opacity = fOpacity(fValue(ii,6)) * flashx3 : Next
			End If
			If IsObject(fValue(ii,2)) Then 
				fValue(ii,2).BlendDisableLighting = 10 * flashx3 : fValue(ii,2).Material = "domelit" & matdim : fValue(ii,2).Visible = Not (fValue(ii,0) < 0.15)
			ElseIf IsArray(fValue(ii,2)) Then
				For Each obj In fValue(ii,2) : obj.BlendDisableLighting = 10 * flashx3 : obj.Material = "domelit" & matdim : obj.Visible = Not (fValue(ii,0) < 0.15) : Next
			End If
			If IsObject(fValue(ii,3)) Then 
				fValue(ii,3).BlendDisableLighting =  flashx3
			ElseIf IsArray(fValue(ii,3)) Then
				For Each obj In fValue(ii,3) : obj.BlendDisableLighting =  flashx3 : Next
			End If
			If IsObject(fValue(ii,4)) Then 
				fValue(ii,4).State = IIF(flashx3<=0.02,LightStateOff,LightStateOn) : fValue(ii,4).IntensityScale = IIF(flashx3<=0,0,flashx3)
			ElseIf IsArray(fValue(ii,4)) Then
				For Each obj In fValue(ii,4) : obj.State = IIF(flashx3<=0.02,LightStateOff,LightStateOn) : obj.IntensityScale = IIF(flashx3<=0,0,flashx3) : Next
			End If
		End If
	Next
	If allZero Then FlasherTimer.Enabled = False
End Sub
Sub InsertFlasherTimer_Timer()
	Dim ii, allZero, fScale, obj
	allZero = True
	If Not InsertFlasherTimer.Enabled Then InsertFlasherTimer.Enabled = True
	For ii = minFlasherNo To maxFlasherNo
		If (IsObject(fValue(ii,5)) Or IsArray(fValue(ii,5))) And fValue(ii,0) >= 0 Then
			allZero = False
			' decrease flasher value
			If fValue(ii,0) = 100 Then fValue(ii,0) = 1 Else fValue(ii,0) = fValue(ii,0) - fDecrease(fValue(ii,6))
			' calc values
			fScale = fValue(ii,0)
			' set flasher object values
			If IsObject(fValue(ii,5)) Then fValue(ii,5).State = LightStateOn : fValue(ii,5).IntensityScale = IIF(fScale<=0,0,fScale)
			If IsArray(fValue(ii,5)) Then
				For Each obj In fValue(ii,5) : obj.State = LightStateOn : obj.IntensityScale = IIF(fScale<=0,0,fScale) : Next
			End If
		End If
	Next
	If allZero Then InsertFlasherTimer.Enabled = False
End Sub
Function fOpacity(fColor)
	If fColor = RedFlasher Then
		fOpacity = RedFlasherOpacity
	ElseIf fColor = BlueFlasher Then
		fOpacity = BlueFlasherOpacity
	ElseIf fColor = YellowFlasher Then
		fOpacity = YellowFlasherOpacity
	Else
		fOpacity = WhiteFlasherOpacity
	End If
End Function
Function fDecrease(fColor)
	If fColor = RedFlasher Then
		fDecrease = RedFlasherDecrease
	ElseIf fColor = BlueFlasher Then
		fDecrease = BlueFlasherDecrease
	ElseIf fColor = YellowFlasher Then
		fDecrease = YellowFlasherDecrease
	ElseIf fColor = InsertFlasher Then
		fDecrease = InsertFlasherDec
	Else
		fDecrease = WhiteFlasherDecrease
	End If
End Function

Dim WhiteFlasher, WhiteFlasherOpacity, WhiteFlasherDecrease
WhiteFlasher			= 1
WhiteFlasherOpacity		= 300
WhiteFlasherDecrease	= 0.9

Dim RedFlasher, RedFlasherOpacity, RedFlasherDecrease
RedFlasher				= 2
RedFlasherOpacity		= 1500
RedFlasherDecrease		= 0.85

Dim BlueFlasher, BlueFlasherOpacity, BlueFlasherDecrease
BlueFlasher				= 3
BlueFlasherOpacity		= 8000
BlueFlasherDecrease		= 0.85

Dim YellowFlasher, YellowFlasherOpacity, YellowFlasherDecrease
YellowFlasher			= 4
YellowFlasherOpacity	= 1000
YellowFlasherDecrease	= 0.9

Dim InsertFlasher, InsertFlasherDec
InsertFlasher			= 5
InsertFlasherDec		= 0.2


' general illumination
Sub ResetGI()
	InitGI False
End Sub
Sub InitGI(startUp)
	If startUp Then
		isGIOn = False
		SolGI False
	End If
	Dim coll, obj
	' init GI overhead
	For Each obj In Array(GIOverhead,GIOverheadRed,GIOverheadYellow,GIOverheadBlue)
		With obj
			.IntensityScale 	= IIF(isGIOn,1,0)
			.State				= LightStateOn
			If GIColorMod = 1 And Right(obj.Name,6) = "Yellow" Then
				.Color			= YellowOverhead
				.ColorFull 		= YellowOverheadFull
				.Intensity 		= YellowOverheadI
			ElseIf GIColorMod = 1 And Right(obj.Name,3) = "Red"  Then
				.Color			= RedOverhead
				.ColorFull 		= RedOverheadFull
				.Intensity 		= RedOverheadI
			ElseIf GIColorMod = 1 And Right(obj.Name,4) = "Blue" Then
				.Color			= BlueOverhead
				.ColorFull 		= BlueOverheadFull
				.Intensity 		= BlueOverheadI
			Else
				.Color			= WhiteOverhead
				.ColorFull 		= WhiteOverheadFull
				.Intensity 		= WhiteOverheadI
			End If
		End With
	Next
	' init GI bulbs
	For Each obj In GIBulbs
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 And Right(obj.Name,1) = "y" Then
			obj.Color		= YellowBulbs
			obj.ColorFull	= YellowBulbsFull
			obj.Intensity 	= YellowBulbsI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "r" Then
			obj.Color		= RedBulbs
			obj.ColorFull	= RedBulbsFull
			obj.Intensity 	= RedBulbsI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "b" Then
			obj.Color		= BlueBulbs
			obj.ColorFull	= BlueBulbsFull
			obj.Intensity 	= BlueBulbsI * EnableGI
		Else
			obj.Color		= WhiteBulbs
			obj.ColorFull 	= WhiteBulbsFull
			obj.Intensity 	= WhiteBulbsI * EnableGI
		End If
	Next
	GIBulb011y.Intensity = GIBulb011y.Intensity / 10
	GIBulb018y.Intensity = GIBulb018y.Intensity / 1000
	GIBulb019y.Intensity = GIBulb019y.Intensity / 1000
	' init GI lights
	For Each obj in GI
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 And Right(obj.Name,1) = "y" Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "r" Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "b" Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BlueI * EnableGI
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhiteI * EnableGI
		End If
	Next
	For Each obj In GIPlastics
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 And Right(obj.Name,1) = "y" Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowPlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "r" Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedPlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "b" Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BluePlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhitePlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		End If
		' reduce the intensity a bit for second level plastics
		If obj.Surface = "105h" Then obj.Intensity = obj.Intensity / 5
	Next
	GIPlasticsLight013y.Intensity = GIPlasticsLight013y.Intensity / 2
	GIPlasticsLight014y.Intensity = GIPlasticsLight014y.Intensity / 2
	GIPlasticsLight015y.Intensity = GIPlasticsLight015y.Intensity / 2
	GIPlasticsLight016y.Intensity = GIPlasticsLight016y.Intensity / 2
	' init tower lights
	If startUp Then
		For Each obj In GIRedTowerLights
			obj.Intensity = obj.Intensity / 2
		Next
		For Each obj In GIYellowTowerLights
			obj.Intensity = obj.Intensity / 2
		Next
		For Each obj In GIBlueTowerLights
			obj.Intensity = obj.Intensity / 1.25
		Next
		For Each obj In GIWhiteTowerLights
			obj.Intensity = obj.Intensity * 2
		Next
	End If
	' init additional flasher for lights
	For Each obj In InsertLightsFlasher
		obj.IntensityScale 	= 0
	Next
End Sub

Dim GIDir : GIDir = 0
Dim GIStep : GIStep = 0
Sub SolGI(IsOff)
	If EnableGI = 0 And Not isGIOn Then Exit Sub
	If isGIOn <> (Not IsOff) Then
		isGIOn = Not IsOff
		If isGIOn Then
			' GI goes on
			PlaySoundAtVol "fx_relay_on", fLight6A, 1
			GIDir = 1 : GITimer_Timer
			DOF 101, DOFOn
		Else
			' GI goes off
			PlaySoundAtVol "fx_relay_off", fLight6A, 1
			GIDir = -1 : GITimer_Timer
			DOF 101, DOFOff
		End If
	End If
End Sub
Sub GITimer_Timer()
	If Not GITimer.Enabled Then GITimer.Enabled = True
	GIStep = GIStep + GIDir
	' set opacity of the shadow overlays and overhead GI illumination
	SetShadowOpacityAndGIOverhead
	' set GI illumination
	SetGIIllumination
	' set bumper lights
	SetBumperLights
	' set the light of some tools
	SetSomeTools -1
	' set material of targets, posts, pegs, ramps etc
	SetMaterials
	' set ramps
	SetRamps
	' GI on/off goes in 4 steps so maybe stop timer
	If (GIDir = 1 And GIStep = 4) Or (GIDir = -1 And GIStep = 0) Then
		GITimer.Enabled = False
	End If
End Sub
Sub SetShadowOpacityAndGIOverhead()
	fGIOff.Opacity 	= (ShadowOpacityGIOff / 4) * (4 - GIStep)
	fGIOn.Opacity 	= (ShadowOpacityGIOn / 4) * GIStep
	' set GI overhead illumination
	GIOverhead.IntensityScale 		= GIStep/4
	GIOverheadRed.IntensityScale 	= IIF(GIColorMod = 1,GIStep/4,0)
	GIOverheadYellow.IntensityScale	= 0'IIF(GIColorMod = 1,GIStep/4,0)
	GIOverheadBlue.IntensityScale 	= IIF(GIColorMod = 1,GIStep/4,0)
End Sub
Sub SetGIIllumination()
	' set GI illumination
	Dim coll, obj
	For Each coll In Array(GI,GIPlastics,GIBulbs)
		For Each obj In coll
			If obj.TimerInterval <> -1 Then obj.IntensityScale = GIStep/4
		Next
	Next
End Sub
'Sub SetBumperLights()
'	Dim obj
'	For Each obj In GIBumperLights
'		obj.IntensityScale 	= GIStep/4
'		obj.State 			= IIF(GIStep <= 0, LightstateOff, LightStateOn)
'	Next
'End Sub
Sub SetMaterials()
	UseGIMaterial GIRubbers, "Rubber White", GIStep
	UseGIMaterial GIWireTrigger, "Metal0.8", GIStep
	'UseGIMaterial GILocknuts, "Metal Chrome S34", GIStep
	'UseGIMaterial GIScrews, "Metal0.8", GIStep
	'UseGIMaterial GIPlasticScrews, "Metal0.8", GIStep
	UseGIMaterial Array(pMetalWalls), "Metal Light", GIStep
	UseGIMaterial GIMetalChromes, "Metal Chrome S34", GIStep
	UseGIMaterial GIMetalPrims, "Metal S34", GIStep
	UseGIMaterial GIYellowPosts, "Plastic White", GIStep
	UseGIMaterial GIBlackPosts, "Plastic White", GIStep
	UseGIMaterial GIRedTargets, "Plastic with opaque image", GIStep
	UseGIMaterial GIDropTargets, "Plastic with opaque image", GIStep
	UseGIMaterial GIYellowTargets, "Plastic with opaque image", GIStep
	UseGIMaterial GIBlueTargets, "Plastic with opaque image", GIStep
	UseGIMaterial GITargetsParts, "Metal S34", GIStep
	UseGIMaterial GIRedPlasticPegs, "TransparentPlasticRed", GIStep
	UseGIMaterial GIYellowPlasticPegs, "TransparentPlasticYellow", GIStep
	UseGIMaterial GIBluePlasticPegs, "TransparentPlasticBlue", GIStep
	UseGIMaterial GIGates, "Metal Wires", GIStep
	UseGIMaterial4Bumper GIBumpers, "Plastic White", GIStep
	UseGIMaterial GIBulbGlasses, "Lamps Glass", GIStep
	UseGIMaterial Array(pSpinner27,pSpinner51), "Plastic with an image V", GIStep
	'UseGIMaterial Array(pToplaneW,pToplaneWA,pToplaneAR,pToplaneR), "TransparentPlasticYellow", GIStep
	UseGIMaterial Array(pLevelPlate,pLevelPlate2), "Metal0.8", GIStep
	UseGIMaterial Array(pLeftFlipper,pRightFlipper), "Plastic with an image", GIStep
	UseGIMaterial Array(pApron,rApronPlunger), "Apron", GIStep
	UseGIMaterial Array(Plunger), "Plunger", GIStep
	UseGIMaterial Array(rSaucer38StopOnTop), "Metal Wires", GIStep
	UseGIMaterial Array(pBackboard), "Plastics Light", GIStep
End Sub
Sub SetRamps()
	pPlasticRamp.Material 	= CreateMaterialName("Prims platt" & IIF(RampColorMod=0, " Black", ""), GIStep)
	pSpitfire.Material 		= CreateMaterialName("aa3dMat", GIStep)
	pHeli.Material 			= CreateMaterialName("aa3dMat", GIStep)
	pWireRamp.Material 		= CreateMaterialName("wireramp", GIStep)
	pSpotlight1.Material 	= CreateMaterialName("Metal Chrome S34", GIStep)
	pSpotlight2.Material 	= CreateMaterialName("Metal Chrome S34", GIStep)
	pSpotlight3.Material 	= CreateMaterialName("Red Rubber", GIStep)
End Sub

Sub SetSomeTools(ledEyesOn)
	' just change Eddie's LED eyes
	If ledEyesOn >= 0 Then
		pEddieGIOn.Image = "Eddie_GIOn_Eyes" & IIF((ledEyesOn <> 0),"On","Off")
		pEddieGIOff.Image = "Eddie_GIOff_Eyes" & IIF((ledEyesOn <> 0),"On","Off")
	Else
		' set Eddie
		pEddieGIOn.Material = "EddieMat" & IIF(GIStep = 4, "", 4-GIStep)
		pEddieGIOn.Visible = (GIStep <> 0)
		pEddieGIOff.Material = "EddieMat" & IIF(GIStep = 0, "", GIStep)
		pEddieGIOff.Visible = (GIStep <> 4)

		' set the Spitfire
		If GIStep < 2 Then
			pSpitfire.blenddisablelighting = 0
			pHeli.blenddisablelighting = 0
		Else
			pSpitfire.blenddisablelighting = 0.3
			pHeli.blenddisablelighting = 0.2
		End If
	End If	
End Sub

' ************* Bumpers hooking on GI Steps *******************************
Sub SetBumperLights()
	If GIStep < 2 Then
		pBumperTop43.image = "BumperTop-off"
		pBumperTop44.image = "BumperTop-off"
		pBumperTop45.image = "BumperTop-off"
		pMetalWalls.image = "MetalWalls_GI_Off"
	Else
		pBumperTop43.image = "BumperTop"
		pBumperTop44.image = "BumperTop"
		pBumperTop45.image = "BumperTop"
		pMetalWalls.image = "MetalWalls_GI_On"
	End If
End Sub
' **************************************************************************

Sub UseGIMaterial(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.Material = mat : Next
End Sub
Sub UseGIMaterial4Bumper(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.SkirtMaterial = mat : Next
End Sub
Function CreateMaterialName(matName, currentGIStep)
	CreateMaterialName = matName & IIF(currentGIStep=0, " Dark", IIF(currentGIStep<4, " Dark" & currentGIStep,""))
End Function


' *********************************************************************
' colors
' *********************************************************************
Dim White, WhiteFull, WhiteI, WhiteP, WhitePlastic, WhitePlasticFull, WhitePlasticI, WhiteBumper, WhiteBumperFull, WhiteBumperI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI, WhiteOverheadFull, WhiteOverhead, WhiteOverheadI
WhiteFull = rgb(255,255,255) 
White = rgb(255,196,64) 'rgb(255,255,180)
WhiteI = 20
WhitePlasticFull = rgb(255,255,180) 
WhitePlastic = rgb(255,255,180)
WhitePlasticI = 25
WhiteBumperFull = rgb(255,255,180) 
WhiteBumper = rgb(255,255,180)
WhiteBumperI = 25
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 10 * ShadowOpacityGIOff
WhiteOverheadFull = rgb(255,255,180)
WhiteOverhead = rgb(255,255,180)
WhiteOverheadI = .15

Dim Yellow, YellowFull, YellowI, YellowPlastic, YellowPlasticFull, YellowPlasticI, YellowBumper, YellowBumperFull, YellowBumperI, YellowBulbs, YellowBulbsFull, YellowBulbsI, YellowOverheadFull, YellowOverhead, YellowOverheadI
YellowFull = rgb(255,255,0)
Yellow = rgb(255,255,0)
YellowI = 5
YellowPlasticFull = rgb(255,255,0)
YellowPlastic = rgb(255,255,0)
YellowPlasticI = 40
YellowBumperFull = rgb(0,0,255)
YellowBumper = rgb(0,0,255)
YellowBumperI = 50
YellowBulbsFull = rgb(255,255,0)
YellowBulbs = rgb(255,255,0)
YellowBulbsI = 250 * ShadowOpacityGIOff
YellowOverheadFull = rgb(255,255,10)
YellowOverhead = rgb(255,255,10)
YellowOverheadI = 0.25

Dim Red, RedFull, RedI, RedPlastic, RedPlasticFull, RedPlasticI, RedBumper, RedBumperFull, RedBumperI, RedBulbs, RedBulbsFull, RedBulbsI, RedOverheadFull, RedOverhead, RedOverheadI
RedFull = rgb(255,75,75)
Red = rgb(255,75,75)
RedI = 50
RedPlasticFull = rgb(255,75,75)
RedPlastic = rgb(255,75,75)
RedPlasticI = 35
RedBumperFull = rgb(255,75,75)
RedBumper = rgb(255,128,32)
RedBumperI = 75
RedBulbsFull = rgb(255,75,75)
RedBulbs = rgb(255,75,75)
RedBulbsI = 250 * ShadowOpacityGIOff
RedOverheadFull = rgb(255,10,10)
RedOverhead = rgb(255,10,10)
RedOverheadI = 0.25

Dim Blue, BlueFull, BlueI, BluePlastic, BluePlasticFull, BluePlasticI, BlueBumper, BlueBumperFull, BlueBumperI, BlueBulbs, BlueBulbsFull, BlueBulbsI,  BlueOverheadFull, BlueOverhead, BlueOverheadI
BlueFull = rgb(75,75,255)
Blue = rgb(75,75,255)
BlueI = 100
BluePlasticFull = rgb(75,75,255)
BluePlastic = rgb(75,75,255)
BluePlasticI = 35
BlueBumperFull = rgb(160,115,0)
BlueBumper = rgb(160,115,0)
BlueBumperI = 50
BlueBulbsFull = rgb(75,75,255)
BlueBulbs = rgb(75,75,255)
BlueBulbsI = 125 * ShadowOpacityGIOff
BlueOverheadFull = rgb(10,10,255)
BlueOverhead = rgb(10,10,255)
BlueOverheadI = .8


' *********************************************************************
' digital display
' *********************************************************************
Dim Digits(31)
Digits(0)  = Array(a_00, a_01, a_02, a_03, a_04, a_05, a_06, a_07, a_08, a_09, a_0a, a_0b, a_0c, a_0d, a_0e, a_0f)
Digits(1)  = Array(a_10, a_11, a_12, a_13, a_14, a_15, a_16, a_17, a_18, a_19, a_1a, a_1b, a_1c, a_1d, a_1e, a_1f)
Digits(2)  = Array(a_20, a_21, a_22, a_23, a_24, a_25, a_26, a_27, a_28, a_29, a_2a, a_2b, a_2c, a_2d, a_2e, a_2f)
Digits(3)  = Array(a_30, a_31, a_32, a_33, a_34, a_35, a_36, a_37, a_38, a_39, a_3a, a_3b, a_3c, a_3d, a_3e, a_3f)
Digits(4)  = Array(a_40, a_41, a_42, a_43, a_44, a_45, a_46, a_47, a_48, a_49, a_4a, a_4b, a_4c, a_4d, a_4e, a_4f)
Digits(5)  = Array(a_50, a_51, a_52, a_53, a_54, a_55, a_56, a_57, a_58, a_59, a_5a, a_5b, a_5c, a_5d, a_5e, a_5f)
Digits(6)  = Array(a_60, a_61, a_62, a_63, a_64, a_65, a_66, a_67, a_68, a_69, a_6a, a_6b, a_6c, a_6d, a_6e, a_6f)

Digits(7)  = Array(b_00, b_01, b_02, b_03, b_04, b_05, b_06, b_07, b_08, b_09, b_0a, b_0b, b_0c, b_0d, b_0e, b_0f)
Digits(8)  = Array(b_10, b_11, b_12, b_13, b_14, b_15, b_16, b_17, b_18, b_19, b_1a, b_1b, b_1c, b_1d, b_1e, b_1f)
Digits(9)  = Array(b_20, b_21, b_22, b_23, b_24, b_25, b_26, b_27, b_28, b_29, b_2a, b_2b, b_2c, b_2d, b_2e, b_2f)
Digits(10) = Array(b_30, b_31, b_32, b_33, b_34, b_35, b_36, b_37, b_38, b_39, b_3a, b_3b, b_3c, b_3d, b_3e, b_3f)
Digits(11) = Array(b_40, b_41, b_42, b_43, b_44, b_45, b_46, b_47, b_48, b_49, b_4a, b_4b, b_4c, b_4d, b_4e, b_4f)
Digits(12) = Array(b_50, b_51, b_52, b_53, b_54, b_55, b_56, b_57, b_58, b_59, b_5a, b_5b, b_5c, b_5d, b_5e, b_5f)
Digits(13) = Array(b_60, b_61, b_62, b_63, b_64, b_65, b_66, b_67, b_68, b_69, b_6a, b_6b, b_6c, b_6d, b_6e, b_6f)

Digits(14) = Array(c_00, c_01, c_02, c_03, c_04, c_05, c_06, c_07)
Digits(15) = Array(c_10, c_11, c_12, c_13, c_14, c_15, c_16)
Digits(16) = Array(c_20, c_21, c_22, c_23, c_24, c_25, c_26)
Digits(17) = Array(c_30, c_31, c_32, c_33, c_34, c_35, c_36, c_37)
Digits(18) = Array(c_40, c_41, c_42, c_43, c_44, c_45, c_46)
Digits(19) = Array(c_50, c_51, c_52, c_53, c_54, c_55, c_56)
Digits(20) = Array(c_60, c_61, c_62, c_63, c_64, c_65, c_66)

Digits(21) = Array(d_00, d_01, d_02, d_03, d_04, d_05, d_06, d_07)
Digits(22) = Array(d_10, d_11, d_12, d_13, d_14, d_15, d_16)
Digits(23) = Array(d_20, d_21, d_22, d_23, d_24, d_25, d_26)
Digits(24) = Array(d_30, d_31, d_32, d_33, d_34, d_35, d_36, d_37)
Digits(25) = Array(d_40, d_41, d_42, d_43, d_44, d_45, d_46)
Digits(26) = Array(d_50, d_51, d_52, d_53, d_54, d_55, d_56)
Digits(27) = Array(d_60, d_61, d_62, d_63, d_64, d_65, d_66)

Digits(28) = Array(e_00, e_01, e_02, e_03, e_04, e_05, e_06)
Digits(29) = Array(e_10, e_11, e_12, e_13, e_14, e_15, e_16)

Digits(30) = Array(f_00, f_01, f_02, f_03, f_04, f_05, f_06)
Digits(31) = Array(f_10, f_11, f_12, f_13, f_14, f_15, f_16)

Dim currentLED(31), replacedLED(31), refreshLEDs
For i = 0 To 31 : currentLED(i) = 0 : replacedLED(i) = 0 : Next
refreshLEDs = False
Dim ledChar(25), ledNo(9), ledNoX(9), ledSign(0)
ledChar(0)  = 1+2+4+16+32+64+2048		' A
ledChar(1)  = 1+2+4+8+512+2048+8192
ledChar(2)  = 1+8+16+32
ledChar(3)  = 1+2+4+8+512+8192
ledChar(4)  = 1+8+16+32+64
ledChar(5)  = 1+16+32+64				' F
ledChar(6)  = 1+4+8+16+32+2048
ledChar(7)  = 2+4+16+32+64+2048
ledChar(8)  = 1+8+512+8192
ledChar(9)  = 2+4+8+16
ledChar(10) = 16+32+64+1024+4096		' K
ledChar(11) = 8+16+32					' L
ledChar(12) = 2+4+16+32+256+1024
ledChar(13) = 2+4+16+32+256+4096
ledChar(14) = 1+2+4+8+16+32
ledChar(15) = 1+2+16+32+64+2048			' P
ledChar(16) = 1+2+4+8+16+32+4096
ledChar(17) = 1+2+16+32+64+2048+4096
ledChar(18) = 1+4+8+32+64+2048			' S
ledChar(19) = 1+512+8192
ledChar(20) = 2+4+8+16+32				' U
ledChar(21) = 16+32+1024+16384
ledChar(22) = 2+4+16+32+4096+16384
ledChar(23) = 256+1024+4096+16384
ledChar(24) = 256+1024+8192
ledChar(25) = 1+8+1024+16384
ledNo(0) 	= 1+2+4+8+16+32
ledNo(1) 	= 2+4
ledNo(2) 	= 1+2+8+16+64+2048
ledNo(3) 	= 1+2+4+8+64+2048
ledNo(4) 	= 2+4+32+64+2048
ledNo(5) 	= 1+4+8+32+64+2048
ledNo(6) 	= 1+4+8+16+32+64+2048
ledNo(7) 	= 1+2+4
ledNo(8) 	= 1+2+4+8+16+32+64+2048
ledNo(9) 	= 1+2+4+32+64+2048
ledNoX(0) 	= 1+2+4+8+16+32
ledNoX(1) 	= 2+4
ledNoX(2) 	= 1+2+8+16+64
ledNoX(3) 	= 1+2+4+8+64
ledNoX(4) 	= 2+4+32+64
ledNoX(5) 	= 1+4+8+32+64
ledNoX(6) 	= 1+4+8+16+32+64
ledNoX(7) 	= 1+2+4
ledNoX(8) 	= 1+2+4+8+16+32+64
ledNoX(9) 	= 1+2+4+32+64
ledSign(0)  = 1024+16384				' /

Sub DisplayTimer_Timer()
	DisplayTimer.Enabled = False
    Dim chgLED : chgLED = Empty
	If Not refreshLEDs Then chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If DesktopMode Or B2SOn Then
		If Not IsEmpty(chgLED) Or refreshLEDs Then
			refreshLEDs = False
			If Not IsEmpty(chgLED) Then GatherLEDs(chgLED)
'			GetScores
			GetCredits
			ReplaceLEDText
			ShowNewText
			ShowLEDs
		End If
    End If
	DisplayTimer.Enabled = True
End Sub

Sub GatherLEDs(chgLED)
	Dim ii, jj, num, chg, stat, digit
	For ii = 0 To UBound(chgLED)
		num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
		If (num < 32) Then
			digit = 1
			For jj = 0 To UBound(Digits(num))
				If (chg And 1) <> 0 Then
					IF (stat And 1) <> 0 Then
						If (currentLED(num) And digit) = 0 Then currentLED(num) = currentLED(num) + digit
					Else
						If (currentLED(num) And digit) <> 0 Then currentLED(num) = currentLED(num) - digit
					End If
				End If
				chg = chg\2 : stat = stat\2 : digit = digit*2
			Next
		End If
	Next
End Sub

Dim score(3), credits
ResetScores
credits = 0
Sub ResetScores()
	Dim ii
	For ii = 0 to 3 : score(ii) = 0 : Next
End Sub
Sub GetScores()
	Dim ii
	For ii = 0 To 3 : score(ii) = GetNoFromDigits(ii*7, 7, score(ii)) : Next
End Sub
Sub GetCredits()
	credits = GetNoFromDigits(28, 2, credits)
End Sub
Function GetNoFromDigits(startingDigit, digits, oldScore)
	Dim cLED, ii, jj, isNumeric, isRealNumeric, ret
	isNumeric = False
	isRealNumeric = False
	ret = 0
	For ii = 0 To digits-1
		cLED = currentLED(startingDigit + ii)
		If (cLED And 128) <> 0 Then cLED = cLED - 128
		If (cLED And 32768) <> 0 Then cLED = cLED - 32768
		If (cLED And 2048) <> 0 Then cLED = cLED - 2048
		isNumeric = (cLED = 0)
		jj = 0
		Do While jj <= 9 And Not isNumeric
			If cLED = ledNoX(jj) Then isNumeric = True : isRealNumeric = True : Exit Do
			jj = jj + 1
		Loop
		If Not isNumeric Then Exit For
		ret = ret + (10 ^ (digits-1-ii)) * jj
	Next
	GetNoFromDigits = IIF(isRealNumeric, ret, oldScore)
End Function

Sub ReplaceLEDText()
	Dim ii
	For ii = 0 To 31
		replacedLED(ii) = currentLED(ii)
	Next
	ReplaceLEDWord Array("A LASER", "A LA   ", "LASER", " WAR  ", "DESTROY", "ION GUN", "WARRIOR", "  DATA ", " EAST  ", "BLAST", "RETURNTO BASE", "RETURNTO", "RETURN", "SHOOTBASE", "  RED", "YELLOW", " BLUE"), _
				   Array("AN IRON", "AN I   ", " IRON", "MAIDEN", " GO FOR", " EDDIE ", "TROOPER", "FRK AND", "FRIENDS", "ROCK ", " SHOOTLOCK   ", " SHOOT  ", " SHOOT", " OPENLOCK", "SENTI", " BEAST", "MUMMY")
	' look for ' MATCH '
	If replacedLED(0) = 0 And replacedLED(1) = ledChar(12) And replacedLED(2) = ledChar(0) And replacedLED(3) = ledChar(19) And replacedLED(4) = ledChar(2) And replacedLED(5) = ledChar(7) And replacedLED(6) = 0 Then
		GameIsOver
	End If
End Sub
Sub ReplaceLEDWord(oldLEDWords, newLEDWords)
	Dim ii, jj, kk, found, oldLEDWord, newLEDWord
	For ii = 0 To 13
		For jj = 0 To UBound(oldLEDWords)
			oldLEDWord = UCase(oldLEDWords(jj))
			newLEDWord = UCase(newLEDWords(jj))
			' fill up with blanks
			Do While Len(newLEDWord) < Len(oldLEDWord)
				newLEDWord = newLEDWord & " "
			Loop
			' replace
			found = False
			If Len(oldLEDWord) > 0 Then
				If LED2Char(ii) = Mid(oldLEDWord,1,1) Then
					found = True
					For kk = ii + 1 To 13
						If LED2Char(kk) <> Mid(oldLEDWord,kk-ii+1,1) Then
							found = False
							Exit For
						End If
						If kk-ii+1 >= Len(oldLEDWord) Then Exit For
					Next
				End If
				If found And ii <= 11 Then
					For kk = ii To 13
						replacedLED(kk) = Char2LED(Mid(newLEDWord,kk-ii+1,1))
						If kk-ii+1 >= Len(newLEDWord) Then Exit For
					Next
				End If
			End If
		Next
	Next
End Sub
Function LED2Char(digit)
	Dim ii, ret
	ret = ""
	If replacedLED(digit) = 0 Then
		ret = " "
	Else
		For ii = 0 To 25
			If replacedLED(digit) = ledChar(ii) Then
				ret = Chr(65+ii)
				Exit For
			End If
		Next
		If ret = "" Then
			For ii = 0 To 9
				If replacedLED(digit) = ledNo(ii) Then
					ret = Chr(48+ii)
					Exit For
				End If
			Next
		End If
		If ret = "" Then
			If replacedLED(digit) = ledSign(0) Then ret = "/"
		End If
	End If
	LED2Char = ret
End Function
Function Char2LED(char)
	Dim ret
	ret = 0
	If Asc(char) >= 65 And Asc(char) <= 90 Then
		ret = ledChar(Asc(char)-65)
	ElseIf Asc(char) >= 48 And Asc(char) <= 57 Then
		ret = ledChar(Asc(char)-48)
	ElseIf char = "/" Then
		ret = ledSign(0)
	End If
	Char2LED = ret
End Function

Dim newLEDText : newLEDText = ""
Dim newLEDText2 : newLEDText2 = ""

Sub AddAutoText(aLEDText) ' *** Auto removed LEDtext
	newLEDText = UCase(aLEDText)
	newLEDText2 = UCase("TODO")
	refreshLEDs = True
	LedTimer.Enabled = True
End Sub
Sub LedTimer_Timer()
	newLEDText = ""
	newLEDText2 = ""
	refreshLEDs = True
	LedTimer.Enabled = False
End Sub
Sub AddText(aLEDText) ' *** Magna on/off  LEDtext
	newLEDText = UCase(aLEDText)
	refreshLEDs = True
End Sub
Sub RemoveText()
	newLEDText = ""
	refreshLEDs = True
	LedTimer.Enabled = False
End Sub
Sub ShowNewText()
	If Len(newLEDText) > 0 Then
		Dim ii, char
		For ii = 1 To Len(newLEDText)
			If ii >= 15 Then Exit For
			char = Mid(newLEDText,ii,1)
			If Asc(char) >= 65 And Asc(char) <= 90 Then
				replacedLED(ii-1) = ledChar(Asc(char)-65)
			ElseIf Asc(char) >= 48 And Asc(char) <= 57 Then
				replacedLED(ii-1) = ledNo(Asc(char)-48)
			ElseIf char = "/" Then
				replacedLED(ii-1) = ledSign(0)
			ElseIf char = " " Then
				replacedLED(ii-1) = 0
			End If
		Next
		If newLEDText2 <> "" Then
			For ii = 1 To 14
				If newLEDText2 = "-" Then
					replacedLED(14+ii-1) = 64
				ElseIf newLEDText2 = "^" Then
					replacedLED(14+ii-1) = 1
				ElseIf newLEDText2 = "_" Then
					replacedLED(14+ii-1) = 8
				ElseIf newLEDText2 = "|" Then
					replacedLED(14+ii-1) = 20
				ElseIf newLEDText2 = "[" Then
					replacedLED(14+ii-1) = 34
				End If
			Next
		End If
	End If
End Sub

Sub ShowLEDs()
	Dim ii, obj, stat
	For ii = 0 To 31
		If DesktopMode Then
			stat = replacedLED(ii)
			For Each obj In Digits(ii)
				obj.State = (stat And 1)
				stat = stat\2
			Next
		End If
		If B2SOn Then
			Controller.B2SSetLED ii+1, replacedLED(ii)
		End If
	Next
End Sub

' *********************************************************************
' some special physics behaviour
' *********************************************************************
' target or rubber post is hit so let the ball jump a bit
Sub DropTargetHit()
	DropTargetSound
	TargetHit
End Sub

Sub StandupTargetHit()
	StandUpTargetSound
	TargetHit
End Sub

Sub TargetHit()
    ActiveBall.VelZ = ActiveBall.VelZ * (0.5 + (Rnd()*LetTheBallJump + Rnd()*LetTheBallJump + 1) / 6)
End Sub

Sub RubberPostHit()
	ActiveBall.VelZ = ActiveBall.VelZ * (0.9 + (Rnd()*(LetTheBallJump-1) + Rnd()*(LetTheBallJump-1) + 1) / 6)
End Sub

Sub RubberRingHit()
	ActiveBall.VelZ = ActiveBall.VelZ * (0.8 + (Rnd()*(LetTheBallJump-1) + 1) / 6)
End Sub

Sub trShooterLaneSlowDown_Hit()
	If BallVel(ActiveBall) > 5 Then
		Dim var : var = 1 + RND() * 0.3
		With ActiveBall
			.VelX = .VelX / 2 * var : .VelY = .VelY / 2 * var
		End With
	End If
End Sub


' *********************************************************************
' sound stuff
' *********************************************************************
Sub RollOverSound()
	PlaySoundAtVolPitch SoundFX("fx_rollover",DOFContactors), ActiveBall, 0.02, .25
End Sub  
Sub DropTargetSound()
	PlaySoundAtVolPitch SoundFX("fx_droptarget",DOFTargets), ActiveBall, 2, .25
End Sub
Sub StandUpTargetSound()
	PlaySoundAtVolPitch SoundFX("fx_target",DOFTargets), ActiveBall, 2, .25
End Sub
Sub GateSound()
	PlaySoundAtVolPitch SoundFX("fx_gate",DOFContactors), ActiveBall, 0.02, .25
End Sub

Sub SwitchSound(switch)
	PlaySoundAt SoundFX("fx_sensor",DOFContactors), switch
End Sub

Sub trShooterLaneRampBeforeStart_Hit()
	isBallOnWireRamp(GetBallID(ActiveBall)) = False
	isBallOnRamp = False
End Sub
Sub trShooterLaneRampStart_Hit()
	isBallOnWireRamp(GetBallID(ActiveBall)) = True
	isBallOnRamp = True
End Sub
Sub trShooterLaneRampEnd_Hit()
	isBallOnWireRamp(GetBallID(ActiveBall)) = False
	isBallOnRamp = False
End Sub

Sub trOrbitLaneRampBeforeStart_Hit()
	isBallOnRamp = False
End Sub
Sub trOrbitLaneRampStart_Hit()
	isBallOnRamp = True
End Sub
Sub trOrbitWireRampStart_Hit()
	isBallOnWireRamp(GetBallID(ActiveBall)) = True
End Sub
Sub trOrbitRampEnd_Hit()
	isBallOnWireRamp(GetBallID(ActiveBall)) = False
	isBallOnRamp = False
End Sub


' *********************************************************************
' supporting Surround Sound Feedback (SSF) functions
' *********************************************************************
' set position as table object (Use object or light but NOT wall) and Vol to 1
Sub PlaySoundAt(sound, tableobj)
	PlaySound sound, 1, 1, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub
' set position as table object and Vol manually.
Sub PlaySoundAtVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub
' set position as table object and Vol + RndPitch manually 
Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
	PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

' set all as per ball position & speed.
Sub PlaySoundAtBall(sound)
	PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
' set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
Sub PlaySoundAtBallAbsVol(sound, VolMult)
	PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

' requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
	PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0, 0, 1, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
' Supporting Ball & Sound Functions
' *********************************************************************
Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 200)
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 2000)
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table.
	Dim tmp : tmp = tableobj.x * 2 / IronMaiden.Width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball)
    Dim tmp : tmp = ball.y * 2 / IronMaiden.Height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function


' *************************************************************
' ball shadow and ramp look
' *************************************************************
Const tnob = 6 ' total number of balls

Dim BallShadow : BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

Sub GraphicsTimer_Timer()
	Dim ii

	' maybe show ball shadows
	If ShowBallShadow <> 0 Then
		Dim BOT
		BOT = GetBalls
		' hide shadow of deleted balls
		If UBound(BOT) < tnob - 1 Then
			For ii = UBound(BOT) + 1 To tnob - 1
				If BallShadow(ii).Visible Then BallShadow(ii).Visible = False
			Next
		End If
		' render the shadow for each ball
		For ii = 0 to UBound(BOT)
			If BOT(ii).X < IronMaiden.Width/2 Then
				BallShadow(ii).X = ((BOT(ii).X) - (Ballsize/6) + ((BOT(ii).X - (IronMaiden.Width/2))/7)) + 6
			Else
				BallShadow(ii).X = ((BOT(ii).X) + (Ballsize/6) + ((BOT(ii).X - (IronMaiden.Width/2))/7)) - 6
			End If
			BallShadow(ii).Y = BOT(ii).Y + 12
			BallShadow(ii).Visible = (BOT(ii).Z > 20)
		Next
	End If

	' maybe move primitive flippers
	pLeftFlipper.ObjRotZ  = LeftFlipper.CurrentAngle
	pRightFlipper.ObjRotZ = RightFlipper.CurrentAngle
	pLeftFlipperShadow.ObjRotZ  = LeftFlipper.CurrentAngle + 1
	pRightFlipperShadow.ObjRotZ = RightFlipper.CurrentAngle + 1
End Sub

Sub ResetRamp()
	InitRamp
End Sub
Sub InitRamp()
	If RampColorMod = 1 Then
		pPlasticRamp.Image	= "RampTextureSilver"
		'pPlasticTower.Image = "RampTextureSilver"
	Else
		pPlasticRamp.Image	= "RampTextureBlack"
		'pPlasticTower.Image = "RampTextureBlack"
	End If
	SetMaterials
End Sub


' *************************************************************
'      rolling and realtime sounds
' *************************************************************
ReDim rolling(tnob)
ReDim isBallOnWireRamp(tnob)
For i = 0 to tnob : rolling(i) = False : isBallOnWireRamp(i) = False : Next
Dim isBallOnRamp : isBallOnRamp = False

Sub RollingSoundTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound "fx_ballrolling" & b
        StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If isBallOnWireRamp(b) Then
				' ball on wire ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				PlaySound "fx_metalrolling" & b, -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			ElseIf BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
                rolling(b) = False
            End If
		End If
		
		'   ball drop sounds matching the adjusted height params
		If BOT(b).VelZ < -2 And BOT(b).Z < 55 And BOT(b).Z > 27 And Not isBallOnRamp Then
			PlaySound "fx_balldrop" & Int(Rnd()*3), 0, ABS(BOT(b).VelZ)/17*5, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

		' saucers sw25, sw33 and sw38
		If InCircle(BOT(b).X, BOT(b).Y, sw25.X, sw25.Y, 20) Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(25) Then
					Controller.Switch(25) 	= True
					Set sw25Ball 			= BOT(b)
					If hdpCanIPlayWithMadness Then hdpCanIPlayWithMadness = (UBound(BOT)>1)
					If hdpCanIPlayWithMadness Then SolRedEject True
				End If
			End If
		End If
		If InCircle(BOT(b).X, BOT(b).Y, sw33.X, sw33.Y, 20) Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(33) Then
					Controller.Switch(33) 	= True
					Set sw33Ball 			= BOT(b)
					If hdpCanIPlayWithMadness Then hdpCanIPlayWithMadness = (UBound(BOT)>1)
					If hdpCanIPlayWithMadness Then SolYellowEject True
				End If
			End If
		End If
		If InCircle(BOT(b).X, BOT(b).Y, sw38.X, sw38.Y, 20) Then
			If Abs(BOT(b).VelX) < 3 And Abs(BOT(b).VelY) < 3 Then
				BOT(b).VelX = BOT(b).VelX / 3
				BOT(b).VelY = BOT(b).VelY / 3
				If Not Controller.Switch(38) Then
					Controller.Switch(38) 	= True
					Set sw38Ball 			= BOT(b)
					If hdpCanIPlayWithMadness Then hdpCanIPlayWithMadness = (UBound(BOT)>1)
					If hdpCanIPlayWithMadness Then SolBlueEject True
				End If
			End If
		End If

    Next
End Sub


' *********************************************************************
' more realtime sounds
' *********************************************************************
' ball collision sound
Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' rubber hit sounds
Sub RubberWalls_Hit(idx)
    PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
	RubberRingHit
End Sub
Sub RubberPosts_Hit(idx)
    PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
	RubberPostHit
End Sub

' metal hit sounds
Sub MetalWalls_Hit(idx)
	PlaySoundAtBallAbsVol "fx_metalhit" & Int(Rnd*3), Minimum(Vol(ActiveBall),0.5)
End Sub

' plastics hit sounds
Sub Plastics_Hit(idx)
	PlaySoundAtBallAbsVol "fx_ball_hitting_plastic", Minimum(Vol(ActiveBall),0.5)
End Sub

' gates sound
Sub Gates_Hit(idx)
	GateSound
End Sub

' *********************************************************************
' some more general methods
' *********************************************************************
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

Function InCircle(pX, pY, centerX, centerY, radius)
	Dim route
	route = Sqr((pX - centerX) ^ 2 + (pY - centerY) ^ 2)
	InCircle = (route < radius)
End Function

Function Minimum(val1, val2)
	Minimum = IIF(val1<val2,val2,val1)
End Function

Const fileName = "C:\Games\Visual Pinball\User\Log.txt"
Sub WriteLine(text)
	Dim fso, fil
	Set fso = CreateObject("Scripting.FileSystemObject")
	Set fil = fso.OpenTextFile(fileName, 8, true)
	fil.WriteLine Date & " " & Time & ": " & text
	fil.Close
	Set fso = Nothing
End Sub

Function IIF(bool, obj1, obj2)
	If bool Then
		IIF = obj1
	Else
		IIF = obj2
	End If
End Function

Function Maximum(obj1, obj2)
	If obj1 > obj2 Then
		Maximum = obj1
	Else
		Maximum = obj2
	End If
End Function

Function GetBallID(actBall)
	Dim b, BOT, ret
	ret = -1
	BOT = GetBalls
	For b = 0 to UBound(BOT)
		If actBall Is BOT(b) Then
			ret = b : Exit For
		End If
	Next
	GetBallId = ret
End Function

'******* Heli Spinner **********************
Dim Spins
Spins = 40
HeliSpin.interval = 30

Sub HeliSpin_Timer
	if Spins > 1 then
	pHeli.RotY = (pHeli.RotY + Spins)
	Spins = Spins -1
	else Spins = 40
	HeliSpin.Enabled = False
	end if
End Sub
'******* End Heli Spinner *********


'**********************************************************
'* BLINDER ANIMATION ********** TOMMY's table inspiration *
'**********************************************************
Dim isBlinderUnfolded : isBlinderUnfolded = False
Dim blinderWaitingTime : blinderWaitingTime = 0
Dim blinderUnfoldTime : blinderUnfoldTime = 0
Const blinderUnlockSongTimeInSecs = 100
Dim isInBlinderUnlockMode : isInBlinderUnlockMode = False

Sub UnfoldBlinder(waitingTime, unfoldTime)
	BlinderUnfoldTimer.Enabled = False
	blinderWaitingTime = waitingTime
	blinderUnfoldTime = unfoldTime
	BlinderUnfoldTimer_Timer
End Sub
Sub ResetBlinder()
	BlinderUnfoldTimer.Enabled = False
	blinderWaitingTime = 0
	blinderUnfoldTime = 0
	MoveBlinder False
End Sub

Sub BlinderUnfoldTimer_Timer()
	If Not BlinderUnfoldTimer.Enabled Then BlinderUnfoldTimer.Enabled = True
	If blinderWaitingTime > 0 Then
		blinderWaitingTime = blinderWaitingTime - 1
	ElseIf blinderUnfoldTime > 0 Then
		blinderUnfoldTime = blinderUnfoldTime - 1
		If Not isBlinderUnfolded Then MoveBlinder True
	Else
		BlinderUnfoldTimer.Enabled = False
		MoveBlinder False
		If isInBlinderUnlockMode Then
			isInBlinderUnlockMode = False
			isSongFOTDUnlocked = True
			AddAutoText "NEW TRKUNLOCKD"
		End If
	End If
End Sub

Sub RestartBlinderFlipperTimer()
	If watchEddieBlinder Then 
		BlinderFlipperTimer.Enabled = False
		BlinderFlipperTimer.Interval = 1500
		BlinderFlipperTimer.Enabled = True
	End If
End Sub
Sub StopBlinderFlipperTimer()
	BlinderFlipperTimer.Enabled = False
	BlinderFlipperTimer.Interval = 1500
End Sub
Sub BlinderFlipperTimer_Timer()
	BlinderFlipperTimer.Enabled = False
	If BlinderFlipperTimer.Interval = 1500 Then
		If isLeftFlipperUp Or isRightFlipperUp Then
			If isLeftFlipperUp Then LeftFlipperDown
			If isRightFlipperUp Then RightFlipperDown
			BlinderFlipperTimer.Interval = 200 : BlinderFlipperTimer.Enabled = True
		End If
	ElseIf BlinderFlipperTimer.Interval = 200 Then
		If isLeftFlipperUp Then LeftFlipperUp
		If isRightFlipperUp Then RightFlipperUp
		BlinderFlipperTimer.Interval = 250 : BlinderFlipperTimer.Enabled = True
	ElseIf BlinderFlipperTimer.Interval = 250 Then
		If Not isLeftFlipperUp Then LeftFlipperDown
		If Not isRightFlipperUp Then RightFlipperDown
		BlinderFlipperTimer.Interval = 1500 : BlinderFlipperTimer.Enabled = True
	End If
End Sub

Sub MoveBlinder(unfold)
	If isBlinderUnfolded <> unfold Then
		BlinderTimer.Interval = IIF(unfold,10,9)
		BlinderTimer.Enabled  = True
		PlaySoundAtVol "fx_blinder", pBlinder1, 0.2
		If unfold then PlaySound "_IMVT-Crowd1"
	End If
End Sub

Sub BlinderTimer_Timer()
	If BlinderTimer.Interval = 10 Then
		If pBlinder1.RotY <= -48 Then BlinderTimer.Enabled = False : isBlinderUnfolded = True : Exit Sub
		pBlinder2.RotY = pBlinder2.RotY - 3
		If pBlinder2.RotY <= -51 Then pBlinder1.RotY = pBlinder1.RotY - 3
	Else
		If pBlinder1.RotY >= 0   Then pBlinder2.RotY =0 : BlinderTimer.Enabled = False : isBlinderUnfolded = False : Exit Sub
		pBlinder2.RotY = pBlinder2.RotY + 3
		If pBlinder2.RotY >= -51 Then pBlinder1.RotY = pBlinder1.RotY + 3
	End If
End Sub

'* END BLINDER ANIMATION **********************************



'***********************************************************************************
' DMD routines using UltraDMD or FlexDMD
'***********************************************************************************

Dim UltraDMD : Set UltraDMD = Nothing
Dim FlexDMD  : Set FlexDMD = Nothing

' DMD using UltraDMD or FlexDMD calls

Sub DMD(background, toptext, bottomtext, duration)
    If ActivateDMD = 0 Then Exit Sub
    UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
End Sub

Sub DMDBlink(background, toptext, bottomtext, duration, nblinks) 'blinks the lower text nblinks times
    If ActivateDMD = 0 Then Exit Sub
    Dim i
    For i = 1 to nblinks
        UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
    Next
	DMD "IMLOGO.jpg", "", "", 20000
End Sub

Sub DMDFlush()
    If ActivateDMD = 0 Then Exit Sub
    UltraDMDTimer.Enabled = False
    UltraDMD.CancelRendering
    UltraDMD.Clear
End Sub

Sub DMDFlushAndBlink(background, toptext, bottomtext, duration, nblinks)
	DMDFlush : DMDBlink background, toptext, bottomtext, duration, nblinks
End Sub

' UltraDMD or FlexDMD init
Sub DMDInit()
	If ActivateDMD = 0 Then Exit Sub

	UltraDMDTimer.Enabled = True

	' try to activate UltraDMD or FlexDMD
	If ActivateDMD = 1 Then
		On Error Resume Next
		Set UltraDMD = CreateObject("UltraDMD.DMDObject")
		On Error GoTo 0
		If UltraDMD is Nothing Then MsgBox "No UltraDMD found. This table MAY run without it." : ActivateDMD = 0 : Exit Sub
		UltraDMD.Init
		If Not UltraDMD.GetMajorVersion = 1 Then MsgBox "Incompatible version of UltraDMD found." : Exit Sub
		If UltraDMD.GetMinorVersion <1 Then MsgBox "Incompatible version of UltraDMD found. Please update to version 1.1 or newer." : Exit Sub
	ElseIf ActivateDMD = 2 Then
		On Error Resume Next
		Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
		On Error GoTo 0
		If FlexDMD is Nothing Then MsgBox "No FlexDMD found.  This table MAY run without it." : ActivateDMD = 0 : Exit Sub
		FlexDMD.GameName = cGameName
		Set UltraDMD = FlexDMD.NewUltraDMD()
		FlexDMD.RenderMode = 2     'Uncomment to use full colorDMD
		UltraDMD.Init
	End If
    
    Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir:curDir = fso.GetAbsolutePathName(".")

    Dim DirName
    DirName = curDir & "\" & cGameName & ".UltraDMD"

    If Not fso.FolderExists(DirName) Then Msgbox "UltraDMD userfiles directory '" & DirName & "' does not exist." & CHR(13) & "No graphic images will be displayed on the DMD"
    UltraDMD.SetProjectFolder DirName

    ' wait for the animation to end
    Do While UltraDMD.IsRendering : Loop

    ' start
    UltraDMD.ScrollingCredits "", "|||IMVT 2020 vpx|||||Original idea||by||Mussinger|||||Builders team|||Herweh|on|code|||DCrosby|3d+GI|||Mussinger|on|design|and|project||||||keep|cool|and|play|virtual|pinball", 15, 10, 1, 1
	DMD "IM03.gif", "", "", 20000
End Sub