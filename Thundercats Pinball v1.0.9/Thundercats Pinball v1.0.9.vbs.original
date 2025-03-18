' ****************************************************************
'                       VISUAL PINBALL X
'                 JPSalas Diablo Pinball Script
'                 MerlinRTP Thundercats Script modifications
'                         Version 1.0.9-DOF-SCORBIT-VR
' ****************************************************************



Option Explicit
Randomize
SetLocale 1033

Const BallSize = 50 ' 50 is the normal size
Const cGameName = "Thundercats"
Const maxvel = 40
Const BallMass = 1

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

'//////////////////////////////////////////////////////////////////////
dim ScorbitActive
ScorbitActive					= 0 	' Is Scorbit Active	
Const     ScorbitShowClaimQR	= 1 	' If Scorbit is active this will show a QR Code  on ball 1 that allows player to claim the active player from the app
Const     ScorbitUploadLog		= 0 	' Store local log and upload after the game is over 
Const     ScorbitAlternateUUID  = 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)
'/////////////////////////////////////////////////////////////////////


'♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥ PLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥



Const bFreePlay = True ' set it to False if you want to use coins


' Difficulty Modes Expained
'  Easy - +4 Mutants, Boss Health 70%, REGEN RATE - ZERO
' Medium - +6 Mutants, Boss Health 70%, REGEN RATE + 100%
' Hard - +10 Mutants, Boss Health 100%, REGEN RATE + 200%
' 1 - EASY, 2 - MEDIUM, 3-HARD
Const StartingDifficulty = 2    ' 2 - MEDIUM

'♥♡♥♡♥♡♥♡♥♡♥♡♥ End of pLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥


' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' DO NOT CHANGE ANYTHING IN THIS SECTION
Const     ScorbitClaimSmall		= 1 	' Make Claim QR Code smaller for high res backglass 

Dim PuPlayer: Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 

Dim pupPackScreenFile: pupPackScreenFile = PuPlayer.GetRoot & "Thundercats\ScreenType.txt"
Dim ObjFso: Set ObjFso = CreateObject("Scripting.FileSystemObject")
Dim ObjFile: Set ObjFile = ObjFso.OpenTextFile(pupPackScreenFile)
Dim DMDType: DMDType = ObjFile.ReadLine

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
debug "DMD: "&DMDType
'DMDType = "1"

' Define any Constants
Const TableName = "Thundercats"
Const myVersion = "1.0.9 DOF"
Const AudioExpiry = 4000			' Duration of Queue Expiration 
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game
Const TrustPostEnabled = 1 ' 0 removes
Const EnableFlexDMD = False
Const EnablePuPDMD = True


'Font colors:
	Const cWhite = 	16777215
	Const cRed = 	397512
	Const cGold = 	1604786
	Const cGreen = 32768
	Const cGrey = 	8421504
	Const cYellow = 65535
	Const cOrange = 33023
	Const cPurple = 16711808
	Const cBlue = 16711680
	Const cLightBlue = 16744448
	Const cBoltYellow = 2148582
	Const cLightGreen = 9747818




'Define Variables related to initial difficulty
Dim MaxMultiballs(4)  ' max number of balls during multiballs
Dim MummRAMode 'Options:  0 = easy,   1 = hard,   2 = insane
Dim MultiballMod
Dim EBLCycle(4)   ' Extra Ball Lit Cycle
Dim BallsPerGame   ' Determined by difficulty
Dim TrialAwardCounter
Dim TrialReq(4)  ' Amount of cycles needed to be awarded bonus
Dim LevelTracker(4)
Dim AwardPoints, TotalBonus
Dim ModeColor



' Define Global Variables
'Dim TmpDMDColor
'Dim toppervideo
Dim BallHandlingQueue : Set BallHandlingQueue = New vpwQueueManager
Dim EOBQueue : Set EOBQueue = New vpwQueueManager
Dim DMDQueue : Set DMDQueue = New vpwQueueManager
Dim HSQueue : Set HSQueue = New vpwQueueManager
Dim AudioQueue : Set AudioQueue = New vpwQueueManager
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
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
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim ao, bsnr         'Stats B2S use it
Dim ActiveDOFCol
Dim x,tmpx

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole
Dim MB(4)   ' Monster Base 
Dim BossHealth(4) ' Boss Health Base
Dim DiffMod(4) ' Regeneration multiplier
Dim NotInBossFight
Dim NotInMutantFight
Dim TmpHitCount
Dim JCoolDown, TCoolDown,EOBTracker,EBAwarded,SSLight


' Define Game Flags
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bOnTheFirstBallScorbit
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bSkipBossVideo
Dim bSkipGameOverVideo
'Dim bMummRADefeated
Dim LaneAwardEB

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger
Dim LMagnet, RMagnet
Dim FlexDMD




'===============================   DEBUG CODE ===========================================
Dim CurrTime,objIEDebugWindow
CurrTime = Timer
Sub Debug( myDebugText )

' Uncomment the next line to turn off debugging
Exit Sub

DebugTimer.enabled = 1

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 2200
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>Thundercats Debug Window -TimeStamp: " & CurrTime & "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & CurrTime & "</b><br>" & vbCrLf
End Sub
'===============================================================================================
'===============================   DEBUG CODE ===========================================
'Dim CurrTime,objIEDebugWindow
CurrTime = Timer
Sub Debug4( myDebugText )

' Uncomment the next line to turn off debugging
Exit Sub

DebugTimer.enabled = 1

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 2000
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>Thundercats Debug Window -TimeStamp: " & CurrTime & "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & CurrTime & "</b><br>" & vbCrLf
End Sub
'===============================================================================================

'**************************
'   PinUp Player USER Config
'**************************

dim PuPDMDDriverType: PuPDMDDriverType=2   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
dim useRealDMDScale : useRealDMDScale=1    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
Dim pGameName       : pGameName="Thundercats"  'pupvideos foldername, probably set to cGameName in realworld

Const pTopper=0
Const pDMD=5
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
'Const pFullDMD=5
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10
Const pPuPOverlay=11
Const pBGVideos=12
'Const pLCD=13
Const pTopperVideo=14
Const pLeftApron=18
Const pRightApron=19

Const TygraMB = 3		' If value was larger than 2 ran into intermittent issues - original value was 3
Const MummraMB = 3		' If value was larger than 2 ran into intermittent issues - original value was 4


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

'Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pBGCurPage: pBGCurPage=0
Dim pInAttract : pInAttract=false   'pAttract mode

'****** PuP Variables ******
Const HasPuP = True   'dont set to false as it will break pup

Dim usePUP: Dim cPuPPack:  Dim PUPStatus: PUPStatus=false ' dont edit this line!!!


'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "Thundercats"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup 
'************ PuP-Pack Startup **************

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
		
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub


' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i,c
    Randomize
    
    ' Disable Dev Walls
	DevWall1.visible = False
	DevWall1.isdropped = True
	DevWall2.visible = False
	DevWall2.isdropped = True
	DevWall3.visible = False
	DevWall3.isdropped = True

	' set some initial variables
	MummRAMode = 1 'Options:  0 = easy,   1 = hard
	MultiballMod = 0




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


    If MummRAMode = 0 Then
        Set LMagnet = New cvpmMagnet
        With LMagnet
            .InitMagnet Magnet1, 30
            .GrabCenter = False
            .CreateEvents "LMagnet"
        End With

        Set RMagnet = New cvpmMagnet
        With RMagnet
            .InitMagnet Magnet2, 30
            .GrabCenter = False
            .CreateEvents "RMagnet"
        End With
    Else
        Set LMagnet = New cvpmTurnTable
        With LMagnet
            .InitTurnTable Magnet1, 45
            .spinCW = False
            .SpinUp = 45
            .SpinDown = 45
            .MotorOn = False
            .CreateEvents "LMagnet"
        End With

        Set RMagnet = New cvpmTurnTable
        With RMagnet
            .InitTurnTable Magnet2, 45
            .spinCW = True
            .SpinUp =45
            .SpinDown = 45
            .MotorOn = False
            .CreateEvents "RMagnet"
        End With
    End If

'==========================
   

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 1
    Loadhs

	'load pup pack
	if HasPuP Then
		PuPStart(cPuPPack)  ' Start Pup Pack
	End If

    ' Initalise the DMD display
	if EnableFlexDMD Then
		DMD_Init
	End If

	if EnablePuPDMD or ScorbitActive = 1 Then
		PUPInit
	End If

	if EnablePuPDMD Then
		pDMDTimer.Enabled = 1
	End If

    ' Init main variables and any other flags
	BallsPerGame = 3
    bAttractMode = False
    bOnTheFirstBall = False
	bOnTheFirstBallScorbit = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
	TrialAwardCounter = 0
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bInstantInfo = False
' Need to do something here
    NotInBossFight = True
    NotInMutantFight = True

	'set starting difficulty
	SetDifficulty

    For c = 0 to 4
	TrialReq(c) = 1
	EBLCycle(c) = 3
	LevelTracker(c) = 1
    Next


    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Remove the cabinet rails if in FS mode
    If Table1.ShowDT = False then
        lrail.Visible = False
        rrail.Visible = False
	Else
        lrail.Visible = True
        rrail.Visible = True
    End If
    LoadLUT
    
	if TrustPostEnabled = 0 Then
		RemoveTrustPost
	End If

End Sub

Sub Table_Paused:Controller.Pause = 1:End Sub
Sub Table_unPaused:Controller.Pause = 0:End Sub

'********************************************************************************************************************************************
Sub SetDifficulty

Dim d
    For d = 0 to 4
        MB(d) = 6
		BossHealth(d) = 70
		DiffMod(d) = 1
		MaxMultiballs(d) = 4
    Next


	if StartingDifficulty = 2 Then
	Debug "Difficulty MEDIUM"
		BallsPerGame = 4
		MultiballMod = 0

    For d = 0 to 4
        MB(d) = 6
		BossHealth(d) = 70
		DiffMod(d) = 1
		MaxMultiballs(d) = 4
    Next

	Elseif StartingDifficulty = 3 Then  
		RemoveTrustPost
	Debug "Difficulty HARD"
		BallsPerGame = 3
		MultiballMod = 0

	Primitive13.Y = 1439
	zCol_Rubber_Post042.Y = 1439
	Primitive14.Y = 1404
	zCol_Rubber_Post038.Y = 1404

    For d = 0 to 4
        MB(d) = 10
		BossHealth(d) = 100
		DiffMod(d) = 2
		MaxMultiballs(d) = 4
    Next

	Else
	Debug "Difficulty EASY"
		BallsPerGame = 5  ' 3 or 5

    For d = 0 to 4
        MB(d) = 2
		BossHealth(d) = 65
		DiffMod(d) = 0
		MaxMultiballs(d) = 5
    Next
	End IF

Debug "Setting Initial Difficulty MB:BH:DM:MRM:MM "&MB(CurrentPlayer) &"/"&BossHealth(CurrentPlayer)&"/"&DiffMod(CurrentPlayer)&"/"&MummRAMode&"/"&MaxMultiballs(CurrentPlayer)
End Sub

Sub RemoveTrustPost
	Debug " Removing Trust Post"
	r25.visible = 0
	primitive7.visible = 0
	RubberPin4.visible = 0
	zCol_Rubber_Post044.collidable = 0
End Sub


Sub PSDIncreaseDifficulty
	MB(CurrentPlayer) = MB(CurrentPlayer) + 10
	BossHealth(CurrentPlayer) = BossHealth(CurrentPlayer) + 50
	DiffMod(CurrentPlayer) = Diffmod(CurrentPlayer) + 1

	'MummRAMode = MummRAMode + 1
	Debug "Increasing Difficulty MB:BH:DM:MRM:MM "&MB(CurrentPlayer) &"/"&BossHealth(CurrentPlayer)&"/"&DiffMod(CurrentPlayer)&"/"&MummRAMode&"/"&MaxMultiballs(CurrentPlayer)
	'Debug "MaxSlope : " &Table1.SlopeMax
	'Table1.SlopeMax = Table1.SlopeMax + ".5"
	'TiltSensitivity = TiltSensitivity + 1
	EBLCycle(CurrentPlayer) = EBLCycle(CurrentPlayer) + 2
	MaxMultiballs(CurrentPlayer) = MaxMultiballs(CurrentPlayer) - 1
	TrialReq(CurrentPlayer) = TrialReq(CurrentPlayer) + 1


'Change rail guides to widen outlanes
	Primitive13.Y = 1439
	zCol_Rubber_Post042.Y = 1439
	Primitive14.Y = 1404
	zCol_Rubber_Post038.Y = 1404

' Need to check MummRa Mode and handle magnets vs turntables
'	if MummRAMode = 0 Then
'		if LMagnet.Strength < 120 Then
'			LMagnet.Strength = LMagnet.Strength + 30
'			RMagnet.Strength = RMagnet.Strength + 30
'	Debug "Magnet Strength: "&LMagnet.Strength
'		End If
'	Elseif MummRAMode = 1 Then
'	' Change turn table values
'		LMagnet.maxSpeed = LMagnet.maxSpeed + 15
'		LMagnet.SpinUp = LMagnet.SpinUp + 15
'		LMagnet.SpinDown = LMagnet.SpinDown + 15
'
'		RMagnet.maxSpeed = RMagnet.maxSpeed + 15
'		RMagnet.SpinUp = RMagnet.SpinUp + 15
'		RMagnet.SpinDown = RMagnet.SpinDown + 15
'	Debug "Turntable Speed " & RMagnet.maxSpeed
'	End If

End Sub
'********************************************************************************************************************************************

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If


    If keycode = LeftMagnaSave Then bLutActive = True:Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If

    If Keycode = AddCreditKey Then
		if bFreePlay = False Then
			Credits = Credits + 1
		End If
        DOF 114, DOFOn
        If(Tilted = False)Then
			if EnableFlexDMD Then
				DMDFlush
				DMD "black.jpg", "", "CREDITS " &credits, 500
			Elseif EnablePuPDMD Then
				DisplayLargeHelper "Credits " &Credits, 500 
			End If
            PlaySound "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo  ' 107
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True

		If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper,LFPress
		If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

        If keycode = StartGameKey Then
           If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True)AND(MummRADefeatedExpiredTimer.Enabled = False))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
					if EnableFlexDMD Then
						DMDFlush
						DMD "black.jpg", " ", PlayersPlayingGame & " PLAYERS", 500
					Elseif EnablePuPDMD Then
						DisplayLargeHelper PlayersPlayingGame & " PLAYERS", 500 
					End If
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						if EnableFlexDMD Then
							DMDFlush
							DMD "black.jpg", " ", PlayersPlayingGame & " PLAYERS", 500
						Elseif EnablePuPDMD Then
							DisplayLargeHelper PlayersPlayingGame & " PLAYERS", 500 
						End If
                    Else
                        ' Not Enough Credits to start a game.
                        DOF 114, DOFOff
						if EnableFlexDMD Then
							DMDFlush
							DMD "black.jpg", "CREDITS " &credits, "INSERT COIN", 500
						Elseif EnablePuPDMD Then
							DisplayLargeHelper "CREDITS " &credits & "INSERT COIN", 500 
						End If
                    End If
                End If
		   Elseif MummRADefeatedExpiredTimer.Enabled = True Then
					bSkipBossVideo = True  ' Flag used to quit out of long end game video
		   ElseIf GameOverExpiredTimer.Enabled = True Then
					bSkipGameOverVideo = True
           End If
        End If ' End   If keycode = StartGameKey Then
        Else ' If (GameInPlay)
		if hsbModeActive = False Then   'Added to prevent high score entry completion from launching new game at same time
            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
						bSkipGameOverVideo = True
						Debug "Setting Skip GameOverVideo to True: "&bSkipGameOverVideo
                        ResetForNewGame()
					Else
						bSkipBossVideo = True
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
							Debug "Setting Skip GameOverVideo to True 2"
							bSkipGameOverVideo = True
                            ResetForNewGame()
						Else
							bSkipBossVideo = True
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DOF 114, DOFOff
						if EnableFlexDMD Then
							DMDFlush
							DMD "black.jpg", "CREDITS " &credits, "INSERT COIN", 500
						Elseif EnablePuPDMD Then
							TwoLineHelper "CREDITS " &credits, "INSERT COIN", 500
						End If
                        ShowTableInfo '107
                    End If
                End If
            End If ' StartGameKey
		End If ' end if hsbModeActive    End If ' If (GameInPlay)
	End If
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If hsbModeActive Then
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = False:LutBox.text = ""

    If keycode = PlungerKey Then
Debug " In plunger Key"
		if bOnTheFirstBall = True Then
	Debug " In FirstBall"
			'PuPEvent(501)
	Debug " After First Ball"
		End If
        Plunger.Fire
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
			FlipperDeActivate LeftFlipper, LFPress
		SolLFlipper False   'This would be called by the solenoid callbacks if using a ROM
            InstantInfoTimer.Enabled = False
            If bInstantInfo And EnableFlexDMD Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            FlipperDeActivate RightFlipper, RFPress
		SolRFlipper False   'This would be called by the solenoid callbacks if using a ROM
            InstantInfoTimer.Enabled = False
            If bInstantInfo And EnableFlexDMD Then
                DMDScoreNow 
                bInstantInfo = False
            End If
        End If
'        If keycode = StartGameKey Then
'			bSkipBossVideo = True
'		End If

    End If
End Sub

'***************************
'   LUT - Darkness control
'***************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage + 1:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:ChangeGIIntensity 1
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:ChangeGIIntensity 1
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:ChangeGIIntensity 1
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:ChangeGIIntensity 1
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:ChangeGIIntensity 1
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:ChangeGIIntensity 1
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:ChangeGIIntensity 1
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:ChangeGIIntensity 1
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:ChangeGIIntensity 1
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:ChangeGIIntensity 1
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:ChangeGIIntensity 1
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:ChangeGIIntensity 1
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:ChangeGIIntensity 1
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:ChangeGIIntensity 1
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:ChangeGIIntensity 1
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1               'used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'*************
'  INFO Game
'*************

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    bInstantInfo = True
	if EnableFlexDMD Then
		DMDFlush
	End If
	UltraDMDTimer.Enabled = 1
End Sub

Sub InstantInfo
    Jackpot = 1000000 + Round(Score(CurrentPlayer) / 10, 0)
	if EnableFlexDMD Then
		DMD "black.jpg", "", "INSTANT INFO", 500
		DMD "black.jpg", "JACKPOT", Jackpot, 800
		DMD "black.jpg", "LEVEL", Level(CurrentPlayer), 800
		DMD "black.jpg", "BONUS MULT", BonusMultiplier(CurrentPlayer), 800
		DMD "black.jpg", "ORBIT BONUS", OrbitHits, 800
		DMD "black.jpg", "LANE BONUS", LaneBonus, 800
		DMD "black.jpg", "TARGET BONUS", TargetBonus, 800
		DMD "black.jpg", "RAMP BONUS", RampBonus, 800
		DMD "black.jpg", "MUTANTS DEFEATED", Monsters(CurrentPlayer), 800
	Elseif EnablePuPDMD Then
		
	End If
End Sub

'*************
' Dev Mode 
'*************
Sub DevModeStart
	DevWall1.visible = True
	DevWall1.isdropped = False
	DevWall2.visible = True
	DevWall2.isdropped = False
	DevWall3.visible = True
	DevWall3.isdropped = False
End Sub

Sub DevModeStop
	DevWall1.visible = False
	DevWall1.isdropped = True
	DevWall2.visible =  False
	DevWall2.isdropped = True
	DevWall3.visible =  False
	DevWall3.isdropped = True
End Sub


'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
	Debug "EXIT TABLE"
    Savehs
    if B2SOn Then Controller.stop
	if EnableFlexDMD Then
		DMD_Exit
	Elseif EnablePuPDMD Then
		'PuPlayer.LabelSet pDMD,"LargeMessage","EXITING TABLE",1,""
	End If
	DebugTimer.enabled = 0
End Sub







'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LF.Fire 'LeftFlipper.EOSTorque = 0.65:LeftFlipper.RotateToEnd
        If bSkillshotReady = False Then
            RotateLaneLightsLeft
        End If
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart 'LeftFlipper.EOSTorque = 0.15:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RF.Fire 'RightFlipper.EOSTorque = 0.65:RightFlipper.RotateToEnd
        If bSkillshotReady = False Then
            RotateLaneLightsRight
        End If
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart 'RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
    End If
End Sub


' flippers hit Sound

'**************************************************
' Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
'RandomSoundFlipper() 'Remove this line if Fleep is integrated
'LeftFlipperCollide parm 'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
CheckLiveCatch Activeball, RightFlipper, RFCount, parm
'RandomSoundFlipper() 'Remove this line if Fleep is integrated
'RightFlipperCollide parm 'This is the Fleep code
End Sub

Sub RotateLaneLightsLeft
    Dim TempState
    'flipper lanes
    TempState = l15.State
    l15.State = l16.State
    l16.State = l17.State
    l17.State = l18.State
    l18.State = l19.State
    l19.State = TempState
    'top lanes
    TempState = l20.State
    l20.State = l21.State
    l21.State = l22.State
    l22.State = TempState
    If Mode(0) <> 2 AND bBumperFrenzy = False Then UpdateBumperLights
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    'flipperlanes
    TempState = l19.State
    l19.State = l18.State
    l18.State = l17.State
    l17.State = l16.State
    l16.State = l15.State
    l15.State = TempState
    'top lanes
    TempState = l22.State
    l22.State = l21.State
    l21.State = l20.State
    l20.State = TempState
    If Mode(0) <> 2 AND bBumperFrenzy = False Then UpdateBumperLights
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
		if EnableFlexDMD Then
			DMDFlush
			DMD "black.jpg", " ", "CAREFUL!", 800
		Elseif EnablePuPDMD Then
			DisplayLargeHelper "CAREFUL!", 800
		End If
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
		if EnableFlexDMD Then
			DMDFlush
			DMD "black.jpg", " ", "TILT!", 99999
		Elseif EnablePuPDMD Then
			DisplayLargeHelper "TILT!", 9999
		End If
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
		if EnableFlexDMD Then
			DMDFlush
		End If
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            If Song = "mu_end" Then
                PlaySound Song, 0, 0.1  'this last number is the volume, from 0 to 1
            Else
                PlaySound Song, -1, 0.1 'this last number is the volume, from 0 to 1
            End If
        End If
    End If
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim i
    ActiveDOFCol = 140 + col
    DOF ActiveDOFCol, DOFOn
    'debug.print ActiveDOFCol
    For i = 140 to 150
        If i <> ActiveDOFCol Then DOF i, DOFOff
    Next
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF ActiveDOFCol, DOFOn
    PlaySound "fx_gion"
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next

End Sub

Sub GiOff
    DOF ActiveDOFCol, DOFOff
    PLaySound "fx_gioff"
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next

End Sub


' GI & light sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
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
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
End Sub

' Flasher Effects using lights

Dim FEStep, FEffect
FEStep = 0
FEffect = 0

Sub FlashEffect(n)
    Dim ii
    Select case n
        Case 0 ' all off
            LightSeqFlasher.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqRandom, 50, , 1000
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

'*********************************
' Diverse Collection Hit Sounds
'*********************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls

ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

	if ScorbitActive = 1 And (Scorbit.bNeedsPairing) = False Then 
		Scorbit.StartSession()
	End If

    bGameInPLay = True
	NotInBossFight = True
	NotInMutantFight = True
	bSkipBossVideo = False

	triggerscript 1000, "bSkipGameOverVideo = False"   ' delay for gameover skip

    'resets the score display, and turn off attrack mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
	bOnTheFirstBallScorbit = True

    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

	TriggerScript 1500,"FirstBall"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
	PuPEvent(500)
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False
    ResetNewBallLights()
    'Reset any table specific
    ResetNewBallVariables

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
	Debug "Current Ball: "&Balls
	'EOBTracker = False

	'if EOBTracker = False Then
	Debug " EOB FALSE"
		if EnableFlexDMD Then
			DMDScoreNow
		Elseif EnablePuPDMD Then

		End If
		' create a ball in the plunger lane kicker.
		BallRelease.CreateSizedball BallSize / 2

		' There is a (or another) ball on the playfield
		BallsOnPlayfield = BallsOnPlayfield + 1

		' kick it out..
		PlaySoundAt SoundFXDOF("fx_Ballrel", 110, DOFPulse, DOFContactors), Ballrelease
		BallRelease.Kick 90, 4

	'PlaySong "intro-battle"
	' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
	' set the bAutoPlunger flag to kick the ball in play automatically
		If BallsOnPlayfield > 1 Then
			DOF 111, DOFPulse
			DOF 112, DOFPulse
			DOF 126, DOFPulse
			bMultiBallMode = True
			bAutoPlunger = True
		End If
'	Else
	if EOBTracker = True Then
		mBalls2Eject = 0
		Debug " EOB TRUE"
		BallHandlingQueue.Add "EOBTracker-F","EOBTracker = False",25,2500,0,0,0,False
	End If

End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
	if EOBTracker = False Then
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballTimer.Enabled = True
		'and eject the first ball
		CreateMultiballTimer_Timer
	End If
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
		if BallsOnPlayfield = 0 And EOBTracker = True Then
			mBalls2Eject = 0
			CreateMultiballTimer.Enabled = False  ' stop the timer
		Else
			If BallsOnPlayfield < MaxMultiballs(CurrentPlayer) And mBalls2Eject > 0 Then
				CreateNewBall()
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
					CreateMultiballTimer.Enabled = False
				End If
			Else 'the max number of multiballs is reached, so stop the timer
				mBalls2Eject = 0
				CreateMultiballTimer.Enabled = False
			End If
		End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()

	'FlexDMD.Color = &hff1ae8
	
    Dim  ii
    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

        'Count the bonus. This table uses several bonus
        'Lane Bonus
        'AwardPoints = LaneBonus * 1000
        'TotalBonus = AwardPoints
		EOBQueue.Add "Award-1","AwardPoints = LaneBonus * 1000",97,0,0,0,0,True
		EOBQueue.Add "TB-1","TotalBonus = TotalBonus + AwardPoints",95,0,0,0,0,False
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "LANE BONUS", AwardPoints, 700
			EOBQueue.Add "LE-1","LightEffect 3",25,0,0,0,0,False
			EOBQueue.Add "Playsound bonus1","Playsound ""bonus"" ",25,0,0,0,0,False
			'EOBQueue.Add "Pup 803-1","Pupevent 803",25,600,0,0,0,True
			'TriggerScript 800, "PlaySound ""bonus"", 0, 1, 0, 0, 0, 0, 0"
		Elseif EnablePuPDMD Then
			EOBQueue.Add "Playsound bonus1","Playsound ""bonus"" ",25,0,0,0,0,False
			EOBQueue.Add "LE-1","LightEffect 3",25,0,0,0,0,False
			EOBQueue.Add "pDMDSetPage-2","pDMDSetPage(3)",95,0,0,0,0,True
			'EOBQueue.Add "LargeMessage-1","DisplayLarge ""LANE BONUS""  &AwardPoints ",25,50,0,0,0,False
			EOBQueue.Add "TwoLine-1","EOBHelper ""LANE BONUS"", AwardPoints",25,50,0,0,0,False
		End If

        'Number of Target banks completed
		'AwardPoints = 0
        'AwardPoints = TargetBonus * 100000
        'TotalBonus = TotalBonus + AwardPoints
		EOBQueue.Add "Award-2","AwardPoints = TargetBonus * 100000",97,0,0,0,0,True
		EOBQueue.Add "TB-2","TotalBonus = TotalBonus + AwardPoints",95,0,0,0,0,False
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "TARGET BONUS", AwardPoints, 700
			EOBQueue.Add "LE-2","LightEffect 3",25,700,0,0,0,False
			EOBQueue.Add "Playsound bonus2","Playsound ""bonus"" ",25,700,0,0,0,False
			'EOBQueue.Add "Pup 803-2","Pupevent 803",25,1500,0,0,0,True
			'TriggerScript 1500, "PlaySound ""bonus"", 0, 1, 0, 0, 1000, 0, 0"
		Elseif EnablePuPDMD Then
			EOBQueue.Add "Playsound bonus2","Playsound ""bonus"" ",25,700,0,0,0,False
			EOBQueue.Add "LE-2","LightEffect 3",25,700,0,0,0,False
			EOBQueue.Add "ClearTwoLine-2","ClearTwoLine",25,700,0,0,0,False
			EOBQueue.Add "TwoLine-2","EOBHelper ""TARGET BONUS "", AwardPoints",25,750,0,0,0,False
			'PuPlayer.LabelSet pDMD,"LargeMessage","TARGET BONUS " &AwardPoints,1,""
		End If
        'Number of Ramps completed
		'AwardPoints = 0
        'AwardPoints = RampBonus * 10000
        'TotalBonus = TotalBonus + AwardPoints
		EOBQueue.Add "Award-3","AwardPoints = RampBonus * 10000",97,0,0,0,0,True
		EOBQueue.Add "TB-3","TotalBonus = TotalBonus + AwardPoints",95,0,0,0,0,False
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "RAMP BONUS", AwardPoints, 700
			EOBQueue.Add "LE-3","LightEffect 3",25,1400,0,0,0,False
			EOBQueue.Add "Playsound bonus3","Playsound ""bonus"" ",25,1400,0,0,0,False
			'EOBQueue.Add "Pup 803-3","Pupevent 803",25,2100,0,0,0,True
			'TriggerScript  2200,"PlaySound ""bonus"", 0, 1, 0, 0, 2000, 0, 0"
		Elseif EnablePuPDMD Then
			EOBQueue.Add "LE-3","LightEffect 3",25,1400,0,0,0,False
			EOBQueue.Add "Playsound bonus3","Playsound ""bonus"" ",25,1400,0,0,0,False
			EOBQueue.Add "ClearTwoLine-3","ClearTwoLine",25,700,0,0,0,False
			EOBQueue.Add "TwoLine-3","EOBHelper ""RAMP BONUS "", AwardPoints",25,1450,0,0,0,False
		End If

        'Number of Orbits registered
		'AwardPoints = 0
        'AwardPoints = OrbitHits * 32260
        'TotalBonus = TotalBonus + AwardPoints
		EOBQueue.Add "Award-4","AwardPoints = OrbitHits * 32260",97,0,0,0,0,True
		EOBQueue.Add "TB-4","TotalBonus = TotalBonus + AwardPoints",95,0,0,0,0,False
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "ORBIT BONUS", AwardPoints, 700
			EOBQueue.Add "LE-4","LightEffect 3",25,2100,0,0,0,False
			EOBQueue.Add "Playsound bonus4","Playsound ""bonus"" ",25,2100,0,0,0,False
			'EOBQueue.Add "Pup 803-4","Pupevent 803",25,2800,0,0,0,True
			'TriggerScript 2900,"PlaySound ""bonus"", 0, 1, 0, 0, 3000, 0, 0"
		Elseif EnablePuPDMD Then
			EOBQueue.Add "LE-4","LightEffect 3",25,2100,0,0,0,False
			EOBQueue.Add "Playsound bonus4","Playsound ""bonus"" ",25,2100,0,0,0,False
			EOBQueue.Add "ClearTwoLine-4","ClearTwoLine",25,700,0,0,0,False
			EOBQueue.Add "TwoLine-4","EOBHelper ""ORBIT BONUS "", AwardPoints",25,2150,0,0,0,False
		End If

        'Number of monsters defeated
		'AwardPoints = 0
        'AwardPoints = Monsters(CurrentPlayer) * 25130
		EOBQueue.Add "Award-5","AwardPoints = Monsters(CurrentPlayer) * 25130",97,0,0,0,0,True
		EOBQueue.Add "TB-5","TotalBonus = TotalBonus + AwardPoints",95,0,0,0,0,False
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "MUTANTS DEFEATED", monsters(CurrentPlayer), 700   ' RTP
			EOBQueue.Add "LE-5","LightEffect 3",25,2800,0,0,0,False
			EOBQueue.Add "Playsound bonus5","Playsound ""bonus"" ",25,2800,0,0,0,False
			'EOBQueue.Add "Pup 803-5","Pupevent 803",25,3600,0,0,0,True
			'TriggerScript 3600, "PlaySound ""bonus"", 0, 1, 0, 0, 4000, 0, 0"
		Elseif EnablePuPDMD Then
			EOBQueue.Add "LE-5","LightEffect 3",25,2800,0,0,0,False
			EOBQueue.Add "Playsound bonus5","Playsound ""bonus"" ",25,2800,0,0,0,False
			EOBQueue.Add "ClearTwoLine-5","ClearTwoLine",25,700,0,0,0,False
			EOBQueue.Add "TwoLine-5","EOBHelper ""MUTANTS DEFEATED "", AwardPoints",25,2150,0,0,0,False
		End If

        'Player Level
		'AwardPoints = 0
        'AwardPoints = Level(CurrentPlayer) * 50000
        'TotalBonus = TotalBonus + AwardPoints
		EOBQueue.Add "Award-6","AwardPoints = Level(CurrentPlayer) * 50000",97,0,0,0,0,True
		EOBQueue.Add "TB-6","TotalBonus = TotalBonus + AwardPoints",95,0,0,0,0,False
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "LEVEL BONUS", Level(CurrentPlayer), 700
			EOBQueue.Add "LE-6","LightEffect 3",25,3400,0,0,0,False
			EOBQueue.Add "Playsound bonus6","Playsound ""bonus"" ",25,3600,0,0,0,False
			'EOBQueue.Add "Pup 803-6","Pupevent 803",25,4200,0,0,0,True
			'TriggerScript  4300, "PlaySound ""bonus"", 0, 1, 0, 0, 5000, 0, 0"
		Elseif EnablePuPDMD Then
			EOBQueue.Add "LE-6","LightEffect 3",25,3400,0,0,0,False
			EOBQueue.Add "Playsound bonus6","Playsound ""bonus"" ",25,3700,0,0,0,False
			EOBQueue.Add "ClearTwoLine-6","ClearTwoLine",25,700,0,0,0,False
			EOBQueue.Add "TwoLine-6","EOBHelper ""LEVEL BONUS "", AwardPoints",25,2850,0,0,0,False
		End If

        ' calculate the totalbonus
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)

        ' handle the bonus held
        ' reset the bonus held value since it has been already added to the bonus
        BonusHeldPoints(CurrentPlayer) = 0

        ' the player has won the bonus held award so do something with it :)
        If bBonusHeld Then
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If
        Else ' this is not the last ball so save the bonus for the next ball
            BonusHeldPoints(CurrentPlayer) = TotalBonus
        End If
        bBonusHeld = False

        ' Add the bonus to the score
		if EnableFlexDMD Then
			DMD "bonus-background.wmv", "TOTAL BONUS " &BonusMultiplier(CurrentPlayer), TotalBonus, 2200
			EOBQueue.Add "LE-7","LightEffect 3",25,3700,0,0,0,False
			'EOBQueue.Add "Playsound bonus6","Playsound ""bonus"" ",25,3700,0,0,0,False
			EOBQueue.Add "Playsound bonus7","Playsound ""bonus"" ",25,4400,0,0,0,False
		Elseif EnablePuPDMD Then
			EOBQueue.Add "LE-7","LightEffect 3",25,3700,0,0,0,False
			'EOBQueue.Add "Playsound bonus7","Playsound ""bonus"" ",25,3600,0,0,0,False
			EOBQueue.Add "ClearTwoLine-7","ClearTwoLine",25,0,0,0,0,False
			EOBQueue.Add "TwoLine-7","EOBHelper ""TOTAL BONUS ""  , TotalBonus",25,3750,0,0,0,False
			EOBQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,4900,0,0,0,True
		End If
        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
		'if EnableFlexDMD Then
			BallHandlingQueue.Add "EndOfBall2","EndOfBall2",25,5000,0,0,0,False
		'end If
    Else 'if tilted then only add a short delay
		'if EnableFlexDMD Then
			BallHandlingQueue.Add "EndOfBall2","EndOfBall2",25,100,0,0,0,False
		'End If
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBall2()
'FlexDMD.Color = &hffffff
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        'DMD "Extra-ball.wmv", "", "", 5000
		PuPEvent(632)

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
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
	EOBTracker = True
	ResetEBLights ' reset extraball lights

	Debug "In EOBC"

	if EnableFlexDMD Then
		DMDScoreNow
	Elseif EnablePuPDMD Then

	End If

    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
			if CurrentPlayer = 2 Then
				AudioQueue.Add "Player-2","PuPEvent 902",25,0,0,0,AudioExpiry,False
			Elseif CurrentPlayer = 3 Then
				AudioQueue.Add "Player-3","PuPEvent 903",25,0,0,0,AudioExpiry,False
			Elseif CurrentPlayer = 4 Then
				AudioQueue.Add "Player-4","PuPEvent 904",25,0,0,0,AudioExpiry,False
			Else
				AudioQueue.Add "Player-1","PuPEvent 901",25,0,0,0,AudioExpiry,False
			End IF
			
			if EnableFlexDMD Then
				DMD "black.jpg", " ", "PLAYER " &CurrentPlayer, 800
			Elseif EnablePuPDMD Then
				DisplayLargeHelper "PLAYER " &CurrentPlayer, 800
			End If
        End If
    End If

End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
    'PlaySong "m_end"
    End If
    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    ' show game over on the DMD
    'DMD "game-over.wmv", "", "", 11000
	PuPEvent(640)


    ' set any lights for the attract mode
    GiOff
	'pDMDGameOver              ' This call will break the game on game over     RTP
	StopScorbit
	GameOverExpiredTimer.Enabled = True
	CheckGOSkipTimer.Enabled = True
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
	

Debug "Drain: " &bBallSaverActive &" " &BallsOnPlayfield
    ' Destroy the ball
    Drain.DestroyBall
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

	if BallsOnPlayfield = 0 Then
		Debug " Stopping multiball timer"
		CreateMultiballTimer.Enabled = False
	End If

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    DOF 113, DOFPulse
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active,
        If(bBallSaverActive = True)Then
	Debug "Drain: Ball Saved " &bBallSaverActive &" " &BallsOnPlayfield
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
			PuPEvent(605)
            'DMD "ball-save.wmv", "", "", 5000
        Else

	Debug "Drain No Ball Saver Mode Active: " &bBallSaverActive &" " &BallsOnPlayfield
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ' ResetJackpotLights
                    If Mode(0) <> 4 Then 'Belial not running
                        l42.State = 0
                        l43.State = 0
                        l44.State = 0
                        l45.State = 0
                        l46.State = 0
                    End If
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
	Debug "Should have been last ball on playfield : " &bBallSaverActive &" " &BallsOnPlayfield
                ' End Mode and timers
                'StopSound Song:Song = ""
                ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
				if EnableFlexDMD Then
					DMDFlush
				Elseif EnablePuPDMD Then

				End If
                Select Case Mode(0)
                    Case 0
					AudioQueue.Add "BallLost","PuPEvent 905",95,0,0,0,6000,True
					Triggerscript 2500, "PuPEvent(604)"
						EndOfBall
                    Case 1, 3, 5, 7
					AudioQueue.Add "BallLost","PuPEvent 905",95,0,0,0,6000,True
					Triggerscript 2500, "PuPEvent(611)"
						EndOfBall
                    Case 2, 4, 6, 8, 10
					AudioQueue.Add "BallLost","PuPEvent 905",95,0,0,0,6000,True
						Triggerscript 2500, "PuPEvent(604)"
						EndOfBall
                End Select
	Debug "BallsOn Field 0 : " &bBallSaverActive &" " &BallsOnPlayfield
                StopEndOfBallMode
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
	DOF 199, DOFOn
    if EnablePuPDMD then
	pDMDStartGame
	End If

	if bOnTheFirstBallScorbit And ScorbitActive = 1 And (Scorbit.bNeedsPairing) = false then ScorbitClaimQR(True)

Debug "Ball detected in plunger lane "
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any
    if EnableFlexDMD Then 
		UltraDMDScoreTimer.Enabled = 1
	Elseif EnablePuPDMD Then

	End If

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
Debug "Ball Should Have Autofired"
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 111, DOFPulse
        DOF 112, DOFPulse
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
	Debug "Ball Saver ENABLED "
        EnableBallSaver BallSaverTime
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        swPlungerRest.TimerEnabled = 1 ' this is a new ball, so show the launch ball if inactive for 6 seconds
        UpdateSkillshot()
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
Debug "Ball Left plunger lane "
	HSQueue.RemoveAll(True)
    DOF 199, DOFOff
    DOF 210, DOFPulse
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ResetSkillShotTimer.Enabled = 1
		ScorbitClaimQR(False)
		hideScorbit 'backup call to make sure all scorbit QR codes are gone
    End If
	bOnTheFirstBallScorbit = False
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    swPlungerRest.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    SetLightColor LightShootAgain, amber, 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
Debug "Ball SAVER ENDED "
    BallSaverTimerExpired.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * SecondRound
        ' update the score displays
		if EnableFlexDMD Then
			DMDScore
		Elseif EnablePuPDMD Then

		End If
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
        ' update the score displays
		if EnableFlexDMD Then
			DMDScore
		Elseif EnablePuPDMD Then

		End If
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points) 'not used in this table
' Jackpots only generally increment in multiball mode AND not tilted
' but this doesn't have to be the case
'If(Tilted = False)Then

' If(bMultiBallMode = True) Then
' Jackpot = Jackpot + points
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000) Then
'		Jackpot = 6000
' 	End if
'End if
'End if
End Sub

Sub AddSuperJackpot(points)
    If(Tilted = False)Then

    ' If(bMultiBallMode = True) Then
    '   SuperJackpot = SuperJackpot + points
    ' you may wish to limit the jackpot to a upper limit, ie..
    '	If (Jackpot >= 6000) Then
    '		Jackpot = 6000
    ' 	End if
    'End if
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n < MaxMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
    Else
        l2.State = 2:l3.State = 2:l4.State = 2:l5.State = 2
        AddScore 5000000
		if EnableFlexDMD Then
			DMD "black.jpg", " ", "5.000.000", 1000
		Elseif EnablePuPDMD Then
			DisplayLargeHelper "5.000.000", 1000
		End If
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly
' There is no bonus multiplier lights in this table

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    ' Update the lights
    Select Case Level
        Case 1:l2.State = 0:l3.State = 0:l4.State = 0:l5.State = 0
        Case 2:l2.State = 1:l3.State = 0:l4.State = 0:l5.State = 0
        Case 3:l2.State = 0:l3.State = 1:l4.State = 0:l5.State = 0
        Case 4:l2.State = 0:l3.State = 0:l4.State = 1:l5.State = 0
        Case 5:l2.State = 0:l3.State = 0:l4.State = 0:l5.State = 1
    End Select
End Sub

Sub AwardExtraBall()
	EBAwarded = EBAwarded + 1
	if EBAwarded < 3 Then   ' limit to 2 extra balls per game
		If NOT bExtraBallWonThisBall Then  ' limit to one extra ball per table ball
			AudioQueue.Add "Extraball","PuPEvent 906",45,0,0,0,AudioExpiry,True
			if EnableFlexDMD Then
				DMDBlink "black.jpg", " ", "EXTRA BALL WON", 100, 10
			Elseif EnablePuPDMD Then
				pDMDFlash "EXTRA BALL WON", 2, ModeColor
			End If

			PuPEvent(632)
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
			DOF 121, DOFPulse
			DOF 112, DOFPulse
			bExtraBallWonThisBall = True
			GiEffect 1
			LightEffect 2
		END If
	End If
End Sub

Sub AwardSpecial()
	if EnableFlexDMD Then
		DMDBlink "black.jpg", " ", "EXTRA GAME WON", 100, 10
	Elseif EnablePuPDMD Then
		pDMDFlash "EXTRA GAME WON", 2, ModeColor
	End If

    Credits = Credits + 1
    DOF 114, DOFOn
    DOF 121, DOFPulse
    DOF 112, DOFPulse
    GiEffect 1
    LightEffect 1
End Sub

'in this table the jackpot is always 1 million + 10% of your score

Sub AwardJackpot() 'award a normal jackpot, double or triple jackpot
    Jackpot = 1000000 + Round(Score(CurrentPlayer) / 10, 0)
	PuPEvent(631)
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "JACKPOT", Jackpot, 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlash "JACKPOT", 2, ModeColor
	End If

	AudioQueue.Add "CriticalHitl","PuPEvent 908",25,0,0,0,AudioExpiry,False
    AddScore Jackpot
    GiEffect 1
    DOF 125, DOFPulse
    DOF 202, DOFPulse
    LightEffect 2
    FlashEffect 2
    DOF 154, DOFPulse
End Sub

Sub AwardDoubleJackpot() 'in this table the jackpot is always 1 million + 10% of your score
    Jackpot = (1000000 + Round(Score(CurrentPlayer) / 10, 0)) * 2
	PuPEvent(631)
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "DOUBLE JACKPOT", Jackpot, 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlash "DOUBLE JACKPOT", 2, ModeColor
	End If

    AudioQueue.Add "CriticalHitl","PuPEvent 908",25,0,0,0,AudioExpiry,False
    AddScore Jackpot
    GiEffect 1
    DOF 125, DOFPulse
    DOF 202, DOFPulse
    LightEffect 2
    FlashEffect 2
    DOF 154, DOFPulse
End Sub

Sub AwardSuperJackpot() 'this is actually a tripple jackpot
    SuperJackpot = (1000000 + Round(Score(CurrentPlayer) / 10, 0)) * 3
	PuPEvent(631)
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "SUPER JACKPOT", SuperJackpot, 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlash "SUPER JACKPOT", 2, ModeColor
	End If
    AudioQueue.Add "CriticalHitl","PuPEvent 908",25,0,0,0,AudioExpiry,False
    AddScore SuperJackpot
    GiEffect 1
    DOF 125, DOFPulse
    DOF 202, DOFPulse
    LightEffect 2
    FlashEffect 2
    DOF 154, DOFPulse
End Sub

Sub AwardSkillshot()
    Dim i
    ResetSkillShotTimer_Timer
    'show dmd animation
	if EnableFlexDMD Then
		DMDFlush
	Elseif EnablePuPDMD Then

	End If

    Select case SkillShotValue(CurrentPlayer)
        case 1000000
			AudioQueue.Add "SkkillShot","PuPEvent 910",25,0,0,0,AudioExpiry,False
			PuPEvent(621)
            AddScore SkillshotValue(CurrentPLayer)
        case 2000000
			AudioQueue.Add "SkkillShot","PuPEvent 910",25,0,0,0,AudioExpiry,False
			PuPEvent(622)
            AddScore SkillshotValue(CurrentPLayer)
        case 3000000
			AudioQueue.Add "SkkillShot","PuPEvent 910",25,0,0,0,AudioExpiry,False
			PuPEvent(623)
            AddScore SkillshotValue(CurrentPLayer)
        case 4000000
			AudioQueue.Add "SkkillShot","PuPEvent 910",25,0,0,0,AudioExpiry,False
			PuPEvent(624)
            AddScore SkillshotValue(CurrentPLayer)
        case 5000000
			AudioQueue.Add "SkkillShot","PuPEvent 910",25,0,0,0,AudioExpiry,False
			PuPEvent(625)
            AddScore SkillshotValue(CurrentPLayer)
        case ELSE
			AudioQueue.Add "SkkillShot","PuPEvent 910",25,0,0,0,AudioExpiry,False
			PuPEvent(620)
            AddScore SkillshotValue(CurrentPLayer)
    End Select
    ' increment the skillshot value with 1 million
    SkillShotValue(CurrentPLayer) = SkillShotValue(CurrentPLayer) + 1000000
    'do some light show
    GiEffect 1
    DOF 203, DOFPulse
    LightEffect 2
    'enable the start act/battle by opening the chest door
    DropChestDoor
End Sub

Sub Congratulation()
    Dim tmp
    tmp = "vo_congrat" & INT(RND * 21 + 1)
    PlaySound tmp
End Sub
'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x):DOF 114, DOFOn:Else Credits = 0 End If

    'x = LoadValue(cGameName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    'SaveValue cGameName, "Jackpot", Jackpot
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
if EnableFlexDMD Then

    tmp = Score(1)

    If Score(2) > tmp Then tmp = Score(2)
    If Score(3) > tmp Then tmp = Score(3)
    If Score(4) > tmp Then tmp = Score(4)

    If tmp > HighScore(1)Then 'add 1 credit for beating the highscore
        AwardSpecial
    End If

    If tmp > HighScore(3)Then
		AudioQueue.Add "Congrats","PuPEvent 911",25,2000,0,0,AudioExpiry,False
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
Elseif EnablePupDMD Then
	pDMDSetPage(4)
	Debug "in Check high score"
    tmp = Score(CurrentPlayer)

    If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
    End If

    If tmp > HighScore(3)Then
        HighScore(3) = tmp
        HighScoreEntryInit()
    Else
         EndOfBallComplete()  
    End If

End If
End Sub

Sub HighScoreEntryInit()
	if EnableFlexDMD Then
		hsbModeActive = True
		AudioQueue.Add "EnterInitials","PuPEvent 912",25,0,0,0,AudioExpiry,False
		hsLetterFlash = 0

		hsEnteredDigits(0) = "A"
		hsEnteredDigits(1) = " "
		hsEnteredDigits(2) = " "
		hsCurrentDigit = 0

		hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
		hsCurrentLetter = 1

		DMDFlush
		DMDId "hsc", "black.jpg", "YOUR NAME:", " ", 999999
	Elseif EnablePuPDMD Then
		pDMDSetPage(4)
		Debug "++++++++++++++++    In High Score Entry Init "
		'PuPlayer.LabelSet pDMD,"CurrScore","",0,""   ' Clear current score
		hsbModeActive = True
		hsLetterFlash = 0

		hsEnteredDigits(0) = " "
		hsEnteredDigits(1) = " "
		hsEnteredDigits(2) = " "
		hsCurrentDigit = 0

		hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
		hsCurrentLetter = 1
		ClearHighScoreTwoLine
		HighScoreHelper "YOUR NAME:", " <A  > ", 9999
		HighScoreDisplayNameNow()

		HighScoreFlashTimer.Interval = 250
		HighScoreFlashTimer.Enabled = True
	End If
    HighScoreDisplayName()
End Sub

Sub HighScoreDisplayNameNow()
	debug "in HS display name now"
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
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
if EnableFlexDMD Then


    TempStr = " >"
    if(hsCurrentDigit > 0)then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2)then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2)then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
	DMDMod "hsc", "YOUR NAME:", Mid(TempStr, 2, 5), 999999
Elseif EnablePuPDMD Then
	debug "in HS display name"
	pDMDSetPage(4)

    TempStr = " >"
    if(hsCurrentDigit> 0)then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2)then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2)then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
    HighScoreHelper "YOUR NAME:", Mid(TempStr, 2, 5), 9999
End If
End Sub

Sub HighScoreCommitName()
if EnableFlexDMD Then
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
	DMDFlush
Elseif EnablePupDMD Then
	pDMDSetPage(4)
	Debug " %%%%%%%%%%%  HighScore Commited"
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if
    HighScoreName(3) = hsEnteredName
    SortHighscore
	ClearInitials
End If
    EndOfBallComplete()
End Sub

Sub ClearInitials
	ClearHighScoreTwoLine
End Sub

Sub Reseths
    HighScoreName(0) = "RTP"
    HighScoreName(1) = "CAP"
    HighScoreName(2) = "AAA"
    HighScoreName(3) = "PEG"
    HighScore(0) = 3000000
    HighScore(1) = 2500000
    HighScore(2) = 2000000
    HighScore(3) = 1500000
    Savehs
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1)Then
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

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    ' add any other real time update subs, like gates or diverters
    LeftflipperTop.Rotz = LeftFlipper.CurrentAngle
    RightflipperTop.Rotz = RightFlipper.CurrentAngle
End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

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
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white

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

Sub SetLightColor(n, col, stat)
    Select Case col
        Case 0
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
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
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub ResetAllLightsColor ' Called at a new game
    'shoot again
    SetLightColor LightShootAgain, amber, -1
    ' bonus
    SetLightColor l2, yellow, -1
    SetLightColor l3, yellow, -1
    SetLightColor l4, yellow, -1
    SetLightColor l5, yellow, -1
    ' Acts
    SetLightColor l7, purple, -1
    SetLightColor l8, purple, -1
    SetLightColor l10, purple, -1
    SetLightColor l12, purple, -1
    SetLightColor l13, purple, -1
    ' Lords
    SetLightColor l6, red, -1
    SetLightColor l9, red, -1
    SetLightColor l11, red, -1
    SetLightColor l14, red, -1
    SetLightColor l47, red, -1
    ' flipper lanes
    SetLightColor l15, orange, -1
    SetLightColor l16, orange, -1
    SetLightColor l17, orange, -1
    SetLightColor l18, orange, -1
    SetLightColor l19, orange, -1
    ' bash
    SetLightColor l28, blue, -1
    SetLightColor l29, blue, -1
    SetLightColor l30, blue, -1
    SetLightColor l31, blue, -1
    ' skill
    SetLightColor l35, yellow, -1
    SetLightColor l36, yellow, -1
    SetLightColor l37, yellow, -1
    SetLightColor l40, yellow, -1
    SetLightColor l41, yellow, -1
    ' life - extra ball
    SetLightColor l23, purple, -1
    SetLightColor l24, purple, -1
    SetLightColor l25, purple, -1
    SetLightColor l26, purple, -1
    SetLightColor l27, purple, -1
    ' attack arrows
    SetLightColor l32, green, -1
    SetLightColor l33, green, -1
    SetLightColor l34, purple, -1
    SetLightColor l38, green, -1
    SetLightColor l39, green, -1
    ' critical - jackpot
    SetLightColor l42, red, -1
    SetLightColor l43, red, -1
    SetLightColor l44, purple, -1
    SetLightColor l45, red, -1
    SetLightColor l46, red, -1
    ' level
    SetLightColor l20, orange, -1
    SetLightColor l21, orange, -1
    SetLightColor l22, orange, -1
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

Sub StopRainbow()
    Dim obj
    RainbowTimer.Enabled = 0
    RainbowTimer.Enabled = 0
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

'***********************************************************************************
'         	    JPS DMD - very simple DMD routines using UltraDMD
'***********************************************************************************

Dim UltraDMD, DMDOldColor, DMDOldFullColor

' DMD using UltraDMD calls

Sub DMD(background, toptext, bottomtext, duration)
    UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
    UltraDMDScoreTimer.Enabled = 1                               'to show the score after the animation/message
End Sub

Sub DMDBlink(background, toptext, bottomtext, duration, nblinks) 'blinks the lower text nblinks times
    Dim i
    For i = 1 to nblinks
        UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
    Next
    UltraDMDScoreTimer.Enabled = 1 'to show the score after the animation/message
End Sub

Sub DMDScore
    If NOT UltraDMD.IsRendering Then
        UltraDMD.SetScoreboardBackgroundImage "scoreboard-background.jpg", 15, 7
        Select case Mode(0)
            Case 0, 11 'no mode is active, then show the 4 players, current player, Level and ball number
                UltraDMD.SetScoreboardBackgroundImage "scoreboard-background.jpg", 15, 12
                UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer, Score(1), Score(2), Score(3), Score(4), "Player " & CurrentPlayer & "  Level " & Level(CurrentPlayer), "Ball " & Balls
            Case 1, 3, 5, 7, 9 'during acts, show the 4 players, player number, Level and number of Monsters hit and the total number of Monsters
                UltraDMD.SetScoreboardBackgroundImage "scoreboard-background.jpg", 15, 12
                UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer, Score(1), Score(2), Score(3), Score(4), "Player " & CurrentPlayer & "  Level " & Level(CurrentPlayer), " " & MonsterHits & "/" & MonsterTotal
                'UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer, Score(1), Score(2), Score(3), Score(4), "Player " & CurrentPlayer & " Level " & Level(CurrentPlayer), "Trial " & MonsterHits & "/" & MonsterTotal
            Case 2 'during monster battles show the monsters life and the score
                UltraDMD.SetScoreboardBackgroundImage Panthro(ColorCode), 15, 12
                UltraDMD.DisplayScoreboard 2, 1, Score(CurrentPlayer), LordStrength, 0, 0, "", ""
            Case 4 'during monster battles show the monsters life and the score
                UltraDMD.SetScoreboardBackgroundImage Cheetara(ColorCode), 15, 12
                UltraDMD.DisplayScoreboard 2, 1, Score(CurrentPlayer), LordStrength, 0, 0, "", ""
            Case 6 'during monster battles show the monsters life and the score
                UltraDMD.SetScoreboardBackgroundImage Kittens(ColorCode), 15, 12
                UltraDMD.DisplayScoreboard 2, 1, Score(CurrentPlayer), LordStrength, 0, 0, "", ""
            Case 8 'during monster battles show the monsters life and the score
                UltraDMD.SetScoreboardBackgroundImage Tygra(ColorCode), 15, 12
                UltraDMD.DisplayScoreboard 2, 1, Score(CurrentPlayer), LordStrength, 0, 0, "", ""
            Case 10 'during monster battles show the monsters life and the score
                UltraDMD.SetScoreboardBackgroundImage MummRA(ColorCode), 15, 12
                UltraDMD.DisplayScoreboard 2, 1, Score(CurrentPlayer), LordStrength, 0, 0, "", ""
        End Select
    Else
        UltraDMDScoreTimer.Enabled = 1
    End If
End Sub

Sub PuPDMDBattle
	Select case Mode(0)
		Case 1, 3, 5, 7, 9
			'Pupevent 815 ' set Default bGameInPlay
		Case 2
			PuPlayer.LabelSet pDMD,"Health",LordStrength,1,""
			'Pupevent 811
	End Select
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMDFLush
    UltraDMDTimer.Enabled = 0
    UltraDMDScoreTimer.Enabled = 0
    UltraDMD.CancelRendering
    UltraDMD.Clear
End Sub

Sub DMDScrollCredits(background, text, duration)
    UltraDMD.ScrollingCredits background, text, 15, 14, duration, 14
End Sub

Sub DMDId(id, background, toptext, bottomtext, duration)
    UltraDMD.DisplayScene00ExwithID id, False, background, toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
End Sub

Sub DMDMod(id, toptext, bottomtext, duration)
    UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
End Sub

Sub UltraDMDTimer_Timer() 'used for repeating the attrack mode and the instant info.
    If bInstantInfo Then
        InstantInfo
    ElseIf bAttractMode Then
        ShowTableInfo
    End If
End Sub

Sub UltraDMDScoreTimer_Timer()
    If NOT UltraDMD.IsRendering Then
        DMDScoreNow
    End If
End Sub


Dim Panthro, Cheetara, Kittens, Tygra, MummRA

Panthro = Array("panthro-0.jpg", "panthro-10.jpg", "panthro-20.jpg", "panthro-30.jpg", "panthro-40.jpg", "panthro-50.jpg", "panthro-60.jpg", "panthro-70.jpg", "panthro-80.jpg", "panthro-90.jpg", "panthro-100.jpg")
Cheetara = Array("cheetara-0.jpg", "cheetara-10.jpg", "cheetara-20.jpg", "cheetara-30.jpg", "cheetara-40.jpg", "cheetara-50.jpg", "cheetara-60.jpg", "cheetara-70.jpg", "cheetara-80.jpg", "cheetara-90.jpg", "cheetara-100.jpg")
Kittens = Array("kittens-0.jpg", "kittens-10.jpg", "kittens-20.jpg", "kittens-30.jpg", "kittens40.jpg", "kittens-50.jpg", "kittens-60.jpg", "kittens-70.jpg", "kittens-80.jpg", "kittens-90.jpg", "kittens-100.jpg")
Tygra = Array("tygra-0.jpg", "tygra-10.jpg", "tygra-20.jpg", "tygra-30.jpg", "tygra-40.jpg", "tygra-50.jpg", "tygra-60.jpg", "tygra-70.jpg", "tygra-80.jpg", "tygra-90.jpg", "tygra-100.jpg")
MummRA = Array("mumm-ra-0.jpg", "mumm-ra-10.jpg", "mumm-ra-20.jpg", "mumm-ra-30.jpg", "mumm-ra-40.jpg", "mumm-ra-50.jpg", "mumm-ra-60.jpg", "mumm-ra-70.jpg", "mumm-ra-80.jpg", "mumm-ra-90.jpg", "mumm-ra-100.jpg")






'FLEX DMD COLORS 
	'Blue - &hFF1111
	'Yellow - &h2bf9fd
	'Red - &h1c1cfd
	'Green - &h05a300
	'Orange - &h1a66ff
	'Purple - &hff1ae8

'========================================================
Sub DMD_Init
    'Set UltraDMD = CreateObject("UltraDMD.DMDObject")


    Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
    If FlexDMD is Nothing Then
        MsgBox "No UltraDMD found.  This table will NOT run without it."
        Exit Sub
    End If
    FlexDMD.GameName = cGameName
    FlexDMD.RenderMode = 2
	'TmpDMDColor = "&hFF1111"
	FlexDMD.Color = &hff1ae8
	'FlexDMD.Color = &hffffff
    Set UltraDMD = FlexDMD.NewUltraDMD()
    UltraDMD.Init
    
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If
    Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir:curDir = fso.GetAbsolutePathName(".")

    Dim DirName
    DirName = curDir& "\" &cGameName& ".UltraDMD"

    If Not fso.FolderExists(DirName) Then _
            Msgbox "UltraDMD userfiles directory '" & DirName & "' does not exist." & CHR(13) & "No graphic images will be displayed on the DMD"
    UltraDMD.SetProjectFolder DirName


    DMD "black.jpg", "THUNDERCATS", "VERSION " &myVersion, 1000	
End Sub

'========================================================


Sub DMD_Init2
    If UltraDMD.GetMinorVersion <1 Then
        MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
        Exit Sub
    End If

    Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir:curDir = fso.GetAbsolutePathName(".")

    Dim DirName
    DirName = curDir& "\" &cGameName& ".UltraDMD"

    If Not fso.FolderExists(DirName) Then _
            Msgbox "UltraDMD userfiles directory '" & DirName & "' does not exist." & CHR(13) & "No graphic images will be displayed on the DMD"
    UltraDMD.SetProjectFolder DirName

    ' wait for the animation to end
    While UltraDMD.IsRendering = True
    WEnd

    ' Show ROM version number
    DMD "black.jpg", "THUNDERCATS", "VERSION " &myVersion, 1000
End Sub



Sub DMD_Exit
    If UltraDMD.IsRendering Then UltraDMD.CancelRendering
    Dim WshShell:Set WshShell = CreateObject("WScript.Shell")
    WshShell.RegWrite "HKCU\Software\UltraDMD\color", DMDOldColor, "REG_SZ"
    WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor", DMDOldFullColor, "REG_SZ"
    UltraDMD = Null
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo

    Dim i
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
		if EnableFlexDMD Then
			'DMD "black.jpg", "PLAYER 1", "" &FormatNumber(Score(1),0), 3000
			DMD "black.jpg", "PLAYER 1", Score(1), 3000
		Elseif EnablePuPDMD Then
			'DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""PLAYER 1"", &FormatNumber(Score(1),0), 3000",25,0,0,0,0,False
		End If
    End If
    If Score(2)Then
		if EnableFlexDMD Then
			DMD "black.jpg", "PLAYER 2", Score(2), 3000
		Elseif EnablePuPDMD Then
			'DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""PLAYER 2"", &FormatNumber(Score(2),0), 3000",25,3000,0,0,0,False
		End If
    End If
    If Score(3)Then
		if EnableFlexDMD Then
			DMD "black.jpg", "PLAYER 3", Score(3), 3000
		Elseif EnablePuPDMD Then
			'DMDQueue.Add "TwoLineHelper-3","TwoLineHelper ""PLAYER 3"", &FormatNumber(Score(3),0), 3000",25,9000,0,0,0,False
		End If
    End If
    If Score(4)Then
		if EnableFlexDMD Then
			DMD "black.jpg", "PLAYER 4", Score(4), 3000
		Elseif EnablePuPDMD Then
			'DMDQueue.Add "TwoLineHelper-4","TwoLineHelper ""PLAYER 1"", &FormatNumber(Score(1),0), 3000",25,12000,0,0,0,False
		End If
    End If

    'coins or freeplay
    If bFreePlay Then
		if EnableFlexDMD Then
			DMD "black.jpg", " ", "FREE PLAY", 2000
		Elseif EnablePuPDMD Then
			'TwoLineHelper " ", "FREE PLAY", 2000
		End If
    Else
        If Credits > 0 Then
			if EnableFlexDMD Then
				DMD "black.jpg", "CREDITS " &credits, "PRESS START", 2000
			Elseif EnablePuPDMD Then
				'TwoLineHelper "CREDITS " &credits, "PRESS START", 2000 				
			End If
        Else
			if EnableFlexDMD Then
				DMD "black.jpg", "CREDITS " &credits, "INSERT COIN", 2000
			Elseif EnablePuPDMD Then
				'TwoLineHelper "CREDITS " &credits, "INSERT COIN", 2000
			End If
        End If
        'DMD "intro-coins.wmv", "", "", 65000
    End If

	if EnableFlexDMD Then
		DMD "black.jpg", "HIGHSCORES", "1> " & HighScoreName(0) & " " & FormatNumber(HighScore(0), 0, , , -1), 3000
		DMD "black.jpg", "HIGHSCORES", "2> " & HighScoreName(1) & " " & FormatNumber(HighScore(1), 0, , , -1), 3000
		DMD "black.jpg", "HIGHSCORES", "3> " & HighScoreName(2) & " " & FormatNumber(HighScore(2), 0, , , -1), 3000
		DMD "black.jpg", "HIGHSCORES", "4> " & HighScoreName(3) & " " & FormatNumber(HighScore(3), 0, , , -1), 3000
	Elseif EnablePuPDMD Then
		HSQueue.Add "HSHelper-1","TwoLineHelper ""HIGHSCORE 1"", HighScoreName(0) & "" "" & FormatNumber(HighScore(0), 0, , , -1), 3000",25,0,0,0,0,False
		HSQueue.Add "HSHelper-2","TwoLineHelper ""HIGHSCORE 2"", HighScoreName(1) & "" "" & FormatNumber(HighScore(1), 0, , , -1), 3000",25,3000,0,0,0,False
		HSQueue.Add "HSHelper-3","TwoLineHelper ""HIGHSCORE 3"", HighScoreName(2) & "" "" & FormatNumber(HighScore(2), 0, , , -1), 3000",25,6000,0,0,0,False
		HSQueue.Add "HSHelper-4","TwoLineHelper ""HIGHSCORE 4"", HighScoreName(3) & "" "" & FormatNumber(HighScore(3), 0, , , -1), 3000",25,9000,0,0,0,False
	End If
End Sub

Sub StartAttractMode()
    bAttractMode = True
    UltraDMDTimer.Enabled = 1
    StartLightSeq
    ShowTableInfo   '107
    StartRainbow aLights
End Sub

Sub StopAttractMode()
    bAttractMode = False
	if EnableFlexDMD Then
		DMDScoreNow
	Elseif EnablePuPDMD Then

	End If
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
    StopRainbow
    ResetAllLightsColor
'StopSong
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqFlasher.UpdateInterval = 150
    LightSeqFlasher.Play SeqRandom, 10, , 50000
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
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

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init

End Sub

' tables variables and Mode init
Dim Level(4)
Dim demons(4)
Dim monsters(4)
Dim LaneBonus
Dim TargetBonus
Dim RampBonus
Dim OrbitHits
Dim MonsterHits
Dim MonsterTotal
Dim bActReady
Dim LordStrength
Dim LordLifeStep
Dim LordLifeInterval
Dim bBumperFrenzy
Dim BumperValue(4)
Dim SecondRound
Dim bJackpot
Dim ComboValue

Sub Game_Init() 'called at the start of a new game
    Dim i

    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music
    'PlaySong "intro-battle"
    'Init Variables
    For i = 0 to 4
        Level(i) = 0
        demons(i) = 0
        monsters(i) = 0
        SkillshotValue(i) = 1000000 ' increases by 1000000 each time it is collected
		TrialReq(i) = 1
		LevelTracker(i) = 1
    Next

    LaneBonus = 0                   'it gets deleted when a new ball is launched
    TargetBonus = 0
	TrialAwardCounter = 0

    RampBonus = 0
	ModeColor = cWhite
    OrbitHits = 0
    MonsterHits = 0
    MonsterTotal = 15
    bActReady = False
    LordStrength = 100
    LordLifeStep = 1
    LordLifeInterval = 10000
    BumperValue(1) = 150
    BumperValue(2) = 150
    BumperValue(3) = 150
    BumperValue(4) = 150
    bBumperFrenzy = False
    SecondRound = 1
    bJackpot = False
    ComboValue = 0
	LaneAwardEB = ""
	EBAwarded = 0


    For i = 0 to 10
        Mode(i) = 0
    Next
'Init lights
	if EnablePuPDMD Then pDMDSetPage(1)

End Sub

Sub StopEndOfBallMode()             'this sub is called after the last ball is drained
    ResetSkillShotTimer_Timer
    If Mode(0)Then StopMode Mode(0) 'a mode is active so stop it
    Gate1.Open = False              'close the top gates
    Gate2.Open = False
    StopBumperFrenzy
    ResetChestDoor
    MalthaelAttackTimer.Enabled = 0
    StopJackpots
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    LaneBonus = 0
    TargetBonus = 0
    RampBonus = 0
    OrbitHits = 0
	TrialAwardCounter = 0
    MalthaelAttackTimer.Enabled = 1
End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
    'top Level lights
    l20.State = 0
    l21.State = 0
    l22.State = 0
    SetLightColor bumperLight1, white, 0
    SetLightColor bumperLight1a, white, 0
    SetLightColor bumperLight2, white, 0
    SetLightColor bumperLight2a, white, 0
    SetLightColor bumperLight3, white, 0
    SetLightColor bumperLight3a, white, 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Updates the skillshot light
    LightSeqSkillshot.Play SeqAllOff

tmpx = INT(RND(1) * 3)
	if tmpx = 1 Then
		'Debug " TMPX 1"
		l20.State = 2
		SSLight = "L20"
	ElseIf tmpx = 2 Then
		'Debug " TMPX 2"
		l21.State = 2
		SSLight = "L21"
	Else
		'Debug " TMPX 3"
		l22.State = 2
		SSLight = "L22"
	End If

'DMDFlush
End Sub

Sub SkillshotOff_Hit 'trigger to stop the skillshot due to a weak plunger shot
    If bSkillShotReady Then
        ResetSkillShotTimer_Timer
    End If
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
	if SSLight = "L20" Then
		If l20.State = 2 Then l20.State= 0
	ElseIf SSLight = "L21" Then
		If l21.State = 2 Then l21.State = 0
	ElseIf SSLight = "L22" Then
		If l22.State = 2 Then l22.State = 0
	End If
	    Select Case Mode(0)
        Case 0:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
        Case 1:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
        Case 2:SetLightColor l34, blue, 2 'enable the start act/battle at the chest after the skillshot
        Case 3:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
        Case 4:SetLightColor l34, yellow, 2 'enable the start act/battle at the chest after the skillshot
        Case 5:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
        Case 6:SetLightColor l34, orange, 2 'enable the start act/battle at the chest after the skillshot
        Case 7:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
        Case 8:SetLightColor l34, green, 2 'enable the start act/battle at the chest after the skillshot
        Case 9:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
        Case 10:SetLightColor l34, red, 2 'enable the start act/battle at the chest after the skillshot
        Case 11:SetLightColor l34, purple, 2 'enable the start act/battle at the chest after the skillshot
    End Select
    bActReady = True
'DMDScoreNow
End Sub

'**********
' Jackpots
'**********
' One of the red arrows blinks, hit it for a Jackpot

Sub StartJackpots
    JackpotTimerExpired.Enabled = 1
    ' always start the jackpot with the left ramp & right ramp
    SetLightColor l43, red, 2
    SetLightColor l45, red, 2
    bJackpot = true
End Sub

Sub StopJackpots
    JackpotTimerExpired.Enabled = 0
    l42.State = 0
    l43.State = 0
    l45.State = 0
    l46.State = 0
    bJackpot = False
End Sub

Sub JackpotTimerExpired_Timer
    StopJackpots
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), Lemk
    DOF 104, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    DragonLS.RotZ = -15
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110



    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14:dragonLS.RotZ = -10
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2:dragonLS.RotZ = -5
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:dragonLS.RotZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 105, DOFPulse, DOFContactors), Remk
    DOF 106, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    DragonRS.RotZ = -15
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14:dragonRS.RotZ = -10
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2:dragonRS.RotZ = -5
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:dragonRS.RotZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********
' Bumpers
'*********
Sub Bumper1_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_Bumper", 107, DOFPulse, DOFContactors), Bumper1
        DOF 129, DOFPulse
        PlaySound "di_sword1"
        ' add some points
        If bBumperFrenzy Then
            AddScore BumperValue(CurrentPLayer) * 10
        Else
            AddScore BumperValue(CurrentPLayer) + 1000 * BumperLight1.State ' during the Butcher's fight they will score double (the lights are blinking, so the light.State has a value of 2)
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper1"
        If Mode(0) = 2 Then
            LordStrength = LordStrength - 1 - Level(CurrentPLayer)
            FlashEffect 2
            DOF 154, DOFPulse
            CheckLordStrength
            ' Restart the Lords life restore timer
            LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
            RestartLordLifeTimer
        End If
    End If
End Sub

Sub Bumper2_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_Bumper", 108, DOFPulse, DOFContactors), Bumper2
        DOF 130, DOFPulse
        PlaySound "di_sword2"
        ' add some points
        If bBumperFrenzy Then
            AddScore BumperValue(CurrentPLayer) * 10
        Else
            AddScore BumperValue(CurrentPLayer) + 1000 * BumperLight1.State ' during the Butcher's fight they will score double (the lights are blinking, so the light.State has a value of 2)
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper2"
        If Mode(0) = 2 Then
            LordStrength = LordStrength - 1 - Level(CurrentPLayer)
            FlashEffect 2
            DOF 154, DOFPulse
            CheckLordStrength
            ' Restart the Lords life restore timer
            LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
            RestartLordLifeTimer
        End If
    End If
End Sub

Sub Bumper3_Hit
    If NOT Tilted Then
        PlaySound SoundFXDOF("fx_Bumper", 109, DOFPulse, DOFContactors)
        DOF 131, DOFPulse
        PlaySound "di_sword3"
        ' add some points
        If bBumperFrenzy Then
            AddScore BumperValue(CurrentPLayer) * 10
        Else
            AddScore BumperValue(CurrentPLayer) + 1000 * BumperLight1.State ' during the Butcher's fight they will score double (the lights are blinking, so the light.State has a value of 2)
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper3"
        If Mode(0) = 2 Then
            LordStrength = LordStrength - 1 - Level(CurrentPLayer)
            FlashEffect 2
            DOF 154, DOFPulse
            CheckLordStrength
            ' Restart the Lords life restore timer
            LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
            RestartLordLifeTimer
        End If
    End If
End Sub

Sub UpdateBumperLights
    SetLightColor bumperLight1, white, 0
    SetLightColor bumperLight1a, white, 0
    SetLightColor bumperLight2, white, 0
    SetLightColor bumperLight2a, white, 0
    SetLightColor bumperLight3, white, 0
    SetLightColor bumperLight3a, white, 0
    bumperLight1.State = l20.State
    bumperLight2.State = l21.State
    bumperLight3.State = l22.State
    bumperLight1a.State = l20.State
    bumperLight2a.State = l21.State
    bumperLight3a.State = l22.State
End Sub

Sub StartBumperFrenzy
    bBumperFrenzy = True
    BumperFrenzyTimer.Enabled = 1
    ' turn on the bumper lights
    SetLightColor bumperLight1, white, 2
    SetLightColor bumperLight1a, white, 2
    SetLightColor bumperLight2, white, 2
    SetLightColor bumperLight2a, white, 2
    SetLightColor bumperLight3, white, 2
    SetLightColor bumperLight3a, white, 2
    ' Start the rainbow lights on the bumpers
    StartRainbow aBumperLights
End Sub

Sub StopBumperFrenzy
    Dim ii
    bBumperFrenzy = False
    ' turn off the timer
    BumperFrenzyTimer.Enabled = 0
    ' restore the bumper lights
    StopRainbow
    For each ii in aBumperLights
        SetLightColor ii, White, 0
    Next
    UpdateBumperLights
End Sub

Sub BumperFrenzyTimer_Timer ' the timer is up, so stop the mode
    StopBumperFrenzy
End Sub

'***********
' Level Lanes
'***********

Sub triggertop1_Hit
    PlaySoundAt "fx_sensor", triggertop1
    DOF 122, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If bskillshotReady And SSLight = "L20" Then
        AwardSkillShot
	Else
		ResetSkillShotTimer_Timer
    End If
    l20.State = 1
    If(Mode(0) <> 2)AND(bBumperFrenzy = False)Then UpdateBumperLights
    AddScore 2870
    ' Do some sound or light effect: turn on bumper 1 lights
    If Mode(0) <> 2 Then 'during the Butcher's fight the bumpers are blinking
        FlashForms BumperLight1, 1000, 40, 1:FlashForms BumperLight1a, 1000, 40, 1
        DOF 138, DOFPulse
    End If
    LastSwitchHit = "triggertop1"
    ' do some check
    CheckLevelLanes
End Sub

Sub triggertop2_Hit 'this is the skillshot lane
    PlaySoundAt "fx_sensor", triggertop2
    DOF 123, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If bskillshotReady And SSLight = "L21" Then
        AwardSkillShot
	Else
		ResetSkillShotTimer_Timer
    End If
    l21.State = 1
    If(Mode(0) <> 2)AND(bBumperFrenzy = False)Then UpdateBumperLights
    AddScore 2870 'normal score
    ' Do some sound or light effect: turn on bumper 2 lights
    If Mode(0) <> 2 Then 'during the Butcher's fight the bumpers are blinking
        FlashForms BumperLight2, 1000, 40, 1:FlashForms BumperLight2a, 1000, 40, 1
        DOF 139, DOFPulse
    End If
    LastSwitchHit = "triggertop2"
    ' do some check
    CheckLevelLanes
End Sub

Sub triggertop3_Hit
    PlaySoundAt "fx_sensor", triggertop3
    DOF 124, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If bskillshotReady And SSLight = "L22" Then
        AwardSkillShot
	Else
		ResetSkillShotTimer_Timer
    End If
    l22.State = 1
    If(Mode(0) <> 2)AND(bBumperFrenzy = False)Then UpdateBumperLights
    AddScore 2870
    ' Do some sound or light effect: turn on bumper 3 lights
    If Mode(0) <> 2 Then 'during the Butcher's fight the bumpers are blinking
        FlashForms BumperLight3, 1000, 40, 1:FlashForms BumperLight3a, 1000, 40, 1
        DOF 140, DOFPulse
    End If
    LastSwitchHit = "triggertop3"
    ' do some check
    CheckLevelLanes
End Sub

Sub CheckLevelLanes() 'use the lane lights
    If l20.State + l21.State + l22.State = 3 Then
        ' turn off only the Level lights, not the bumpers
        l20.State = 0
        l21.State = 0
        l22.State = 0
        'up one level to the current player
		'LevelTracker(CurrentPlayer)  = LevelTracker(CurrentPlayer)  + 1
		
		if Mode(0) <> 0 And Level(CurrentPlayer) <= (Mode(0) * 2) Then 
			Level(CurrentPlayer) = Level(CurrentPlayer) + 1
			AudioQueue.Add "LevelUP","PuPEvent 913",25,0,0,0,AudioExpiry,False
			PuPEvent(630)
		Elseif Level(CurrentPlayer) < 3 Then 
			Level(CurrentPlayer) = Level(CurrentPlayer) + 1		
			AudioQueue.Add "LevelUP","PuPEvent 913",25,0,0,0,AudioExpiry,False
			PuPEvent(630)
		Else
			if EnableFlexDMD Then
				DMD "black.jpg", "MAX LEVEL REACHED", "UNTIL NEXT BOSS", 3000   'RTP
			Elseif EnablePuPDMD Then
				TwoLineHelper "MAX LEVEL REACHED", "UNTIL NEXT BOSS", 3000
			End If
		End If
       'Increase bonus multiplier
        AddBonusMultiplier 1
        'flash and Light effect
        LightEffect 3
        FlashEffect 3
        ' open the gates and enable the orbit shots
        Gate1.Open = true:Gate2.Open = True
        OrbitCloseTimer.Enabled = 1
    End If
End Sub

Sub OrbitCloseTimer_Timer
    Gate1.Open = False:Gate2.Open = False
    OrbitCloseTimer.Enabled = 0
End Sub

Sub CriticalHit

        Select Case Mode(0)
            Case 1, 3, 5, 7, 9
                MonsterTotal = MonsterTotal - Level(CurrentPlayer)
                CheckActMonsters
            Case 2, 4, 6, 8, 10
                LordStrength = LordStrength -15
                CheckLordStrength
		Case Else

        End Select

End Sub

'**********************
' Flipper Lanes: QUEST
'**********************

Sub sw1_Hit
    PlaySoundAt "fx_sensor", sw1
    DOF 132, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    l15.State = 1
    AddScore 50050
    ' Do some sound or light effect
    ' do some check
    CheckQUESTLanes
End Sub

Sub sw2_Hit
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
    PlaySoundAt "fx_sensor", sw2
    DOF 133, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    l16.State = 1
    AddScore 10010
    ' Do some sound or light effect
    ' do some check
    CheckQUESTLanes
End Sub

Sub sw3_Hit
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
    PlaySoundAt "fx_sensor", sw3
    DOF 133, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    l17.State = 1
    AddScore 10010
    ' Do some sound or light effect
    ' do some check
    CheckQUESTLanes
End Sub

Sub sw4_Hit
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
    PlaySoundAt "fx_sensor", sw4
    DOF 134, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    l18.State = 1
    AddScore 10010
    ' Do some sound or light effect
    ' do some check
    CheckQUESTLanes
End Sub

Sub sw5_Hit
    PlaySoundAt "fx_sensor", sw5
    DOF 135, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    l19.State = 1
    AddScore 50050
    ' Do some sound or light effect
    ' do some check
    CheckQUESTLanes
End Sub

Sub CheckQUESTLanes() 'use the lane lights
    If l15.State + l16.State + l17.State + l18.State + l19.State = 5 Then
		TrialAwardCounter = TrialAwardCounter + 1
		PuPEvent(626)  ' play TRIAL topper
        'Increase bonus multiplier
        AddBonusMultiplier 1
        'flash and Light effect
        l15.State = 0
        l16.State = 0
        l17.State = 0
        l18.State = 0
        l19.State = 0
        LightEffect 1
        GIEffect 1
        DOF 117, DOFPulse
        FlashEffect 1
        DOF 155, DOFPulse
        DropChestDoor  'open the door to the chest
		if TrialAwardCounter = TrialReq(CurrentPlayer)  Then
			AddMultiball 1 'add a multiball
			PuPEvent(602) ' trial multiball
		End If
    End If
End Sub

'*****************
'  BASH Targets
'*****************

Sub dt1_Hit
    DOF 211, DOFPulse
    PlaySoundAtBall SoundFXDOF("fx_target", 118, DOFPulse, DOFTargets)
    PlaySound "bash"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l28.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l28.State = 1
            ' do some check
            CheckBASHTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt1"
End Sub

Sub dt2_Hit
    DOF 211, DOFPulse
    PlaySoundAtBall SoundFXDOF("fx_target", 118, DOFPulse, DOFTargets)
    PlaySound "bash"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l29.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l29.State = 1
            ' do some check
            CheckBASHTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt2"
End Sub

Sub dt3_Hit
    DOF 211, DOFPulse
    PlaySoundAtBall SoundFXDOF("fx_target", 118, DOFPulse, DOFTargets)
    PlaySound "bash"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l30.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l30.State = 1
            ' do some check
            CheckBASHTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt3"
End Sub

Sub dt4_Hit
    DOF 211, DOFPulse
    PlaySoundAtBall SoundFXDOF("fx_target", 118, DOFPulse, DOFTargets)
    PlaySound "bash"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l31.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l31.State = 1
            ' do some check
            CheckBASHTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt4"
End Sub

Sub CheckBASHTargets
    If l28.State + l29.State + l30.State + l31.State = 4 Then
        FlashEffect 2
        DOF 154, DOFPulse
			' increment the target bonus
			TargetBonus = TargetBonus + 1
			If TargetBonus = EBLCycle(CurrentPlayer) Then 'lit extra ball at the chest
				l25.State = 2
				LaneAwardEB = "l25"
				'ExtraBallLitTimer.Enabled = 1
				AudioQueue.Add "ExtraBallLit","PuPEvent 907",25,0,0,0,AudioExpiry,False
				if EnableFlexDMD Then
					DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
				Elseif EnablePuPDMD Then
					pDMDFlash "EXTRA BALL IS LIT", 2, ModeColor
				End If
			End If
	

        Select Case Mode(0)
            Case 1, 3, 5, 7, 9
				PuPEvent(628)
                MonsterHits = MonsterHits + 5 + Level(CurrentPlayer)
				RandomMutant
                CheckActMonsters
                MonsterHitTimer_Timer
                ResetBashLights
            Case 2, 4, 6, 8, 10
				PuPEvent(628)
                LordStrength = LordStrength -10 + Level(CurrentPlayer)
                CheckLordStrength
                ResetBashLights
        End Select
    End If
' set a cooldown timer before extraball lit can be set again


End Sub

Sub ResetBashLights
    l28.State = 0
    l29.State = 0
    l30.State = 0
    l31.State = 0
End Sub

Sub ResetEBLights
    l23.State = 0
    l24.State = 0
    l25.State = 0
    l26.State = 0
    l27.State = 0
End Sub

'*****************
'  SKILL Targets
'*****************

Sub dt5_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 119, DOFPulse, DOFTargets)
    PlaySound "chains"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l35.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l35.State = 1
            ' do some check
            CheckSKILLTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt5"
End Sub

Sub dt6_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 119, DOFPulse, DOFTargets)
    PlaySound "chains"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l36.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l36.State = 1
            ' do some check
            CheckSKILLTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt6"
End Sub

Sub dt7_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 119, DOFPulse, DOFTargets)
    PlaySound "chains"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l37.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l37.State = 1
            ' do some check
            CheckSKILLTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt7"
End Sub

Sub dt8_Hit
    DOF 212, DOFPulse
    PlaySoundAtBall SoundFXDOF("fx_target", 120, DOFPulse, DOFTargets)
    PlaySound "chains"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l40.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back some life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l40.State = 1
            ' do some check
            CheckSKILLTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt8"
End Sub

Sub dt9_Hit
    DOF 212, DOFPulse
    PlaySoundAtBall SoundFXDOF("fx_target", 120, DOFPulse, DOFTargets)
    PlaySound "chains"
    If Tilted Then Exit Sub
    Select Case Mode(0)
        Case 4                    ' Belial
            If l41.State = 1 Then 'the light is lit so hit Belial
                LordStrength = LordStrength -10 - Level(CurrentPLayer)
                GiEffect 2
                DOF 117, DOFPulse
                CheckLordStrength
                ' Restart the Lords life restore timer
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
            Else ' the light is off so give back life to Belial
                LordStrength = LordStrength + 5
                If LordStrength > 100 Then
                    LordStrength = 100
                End If
            End If
        Case Else
            l41.State = 1
            ' do some check
            CheckSKILLTargets
    End Select
    AddScore 25010
    ' Do some sound or light effect
    LightEffect 1
    LastSwitchHit = "dt9"
End Sub

Sub CheckSKILLTargets
Debug " In check SNARF Targets"
    If l35.State + l36.State + l37.State + l40.State + l41.State = 5 Then
        FlashEffect 2
        DOF 154, DOFPulse
        ' increment the target bonus
			TargetBonus = TargetBonus + 1
			If TargetBonus = EBLCycle(CurrentPlayer) Then 'lit extra ball at the chest
			Debug " SNARF EXTRA BALL"
				l25.State = 2
				LaneAwardEB = "l25"
				'ExtraBallLitTimer.Enabled = 1
				AudioQueue.Add "ExtraBallLit","PuPEvent 907",25,0,0,0,AudioExpiry,False
				if EnableFlexDMD Then
					DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
				Elseif EnablePuPDMD Then
					pDMDFlash "EXTRA BALL IS LIT", 2, ModeColor
				End If
			End If



        Select Case Mode(0)
            Case 1, 3, 5, 7, 9
				PuPEvent(627)
                MonsterHits = MonsterHits + 7 + Level(CurrentPlayer)
                CheckActMonsters
				RandomMutant
                MonsterHitTimer_Timer
                ResetSkillLights
            Case 2, 4, 6, 8, 10
				PuPEvent(627)
                LordStrength = LordStrength -15 + Level(CurrentPlayer)
                CheckLordStrength
                ResetSkillLights
        End Select
    End If
End Sub

Sub ResetSkillLights
    l35.State = 0
    l36.State = 0
    l37.State = 0
    l40.State = 0
    l41.State = 0
End Sub

'*******************
' Resplendent Chest
'*******************

Sub ChestDoor_Hit
    PlaySoundAt SoundFXDOF("fx_droptarget", 116, DOFPulse, DOFContactors), ChestHole
    AudioQueue.Add "CenterShot","PuPEvent 909",25,0,0,0,AudioExpiry,False
    If Tilted Then Exit Sub
    DropChestDoor
End Sub

Sub DropChestDoor
    ChestDoor.IsDropped = 1
    DOF 116, DOFPulse
    'Play a sound, do some light/flash effect
    GiEffect 1
    DOF 117, DOFPulse
End Sub

Sub ResetChestDoor
    ChestDoor.IsDropped = 0
    DOF 116, DOFPulse
    PlaySoundAt "fx_resetdrop", ChestHole
End Sub

Sub ChestHole_Hit
    DOF 213, DOFPulse
		'Debug "In Chest Exit 0"
	TestSub
    DOF 128, DOFOn
    PlaySoundAt "fx_kicker_enter", ChestHole
    ResetChestDoor
    If NOT Tilted Then
        FlashForMs f16, 2000, 50, 0
        DOF 136, DOFPulse
        If Mode(0) = 0 Then 'we could check here too for the variable bActReady
		'Debug "In Chest Exit 1"
		if BallsOnPlayfield < 3 Then
           StartNextMode
		else
		if EnableFlexDMD Then
			DMD "black.jpg", "Too Many Balls", "On Playfield", 3000   'RTP
			DMD "black.jpg", "Need Less Than", "3 Balls", 3000   'RTP
			DMD "black.jpg", "To Start", "New Trial", 3000   'RTP  
		Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""Too Many Balls"", ""On Playfield"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""Need Less Than"", ""3 Balls"", 3000",25,3000,0,0,0,False
		DMDQueue.Add "TwoLineHelper-3","TwoLineHelper ""To Start"", ""New Trial"", 3000",25,6000,0,0,0,False
		End If
		GiveRandomAward
		' Message to reduce balls on playfield to continue trials
		End If
			TriggerScript 600,"ChestExit"
        Else
            If Mode(0) = 11 Then 'the end
		'Debug "In Chest Exit 2"
                WinMummRA2
            Else
		'Debug "In Chest Exit 3"
                GiveRandomAward
				TriggerScript 600,"ChestExit"
            End If
        End If
        If l25.State = 2 Then 'give the extra ball
            l25.State = 0
            AwardExtraBall
        End If
		if l44.state = 2 Then  'Do Critical Hit Damage
			CriticalHit
		End If
        If bJackpot AND l44.State = 2 Then 'give the SuperJackpot & enable the next jackpot
            l44.State = 0
            SetLightColor l43, red, 2
            SetLightColor l45, red, 2
            AwardSuperJackpot
        End If
    Else 'if tilted then just exit the ball
		TriggerScript 100,"ChestExit"
    End If
End Sub

Sub ChestExit()
    DOF 128, DOFOff
    If B2SOn Then Controller.B2SSetData bsnr, 1:Controller.B2SSetData 20, 0:Controller.B2SSetData 80, 1
    FlashForMs f16, 1000, 50, 0
    DOF 137, DOFPulse
    ChestHole.DestroyBall
    ChestOut.CreateBall
    PlaySoundAt SoundFXDOF("fx_popper", 115, DOFPulse, DOFContactors), ChestOut
    DOF 112, DOFPulse
    ChestOut.kick 65, 27, 1.56
End Sub

Sub GiveRandomAward() 'from the Chest and the Orbits
    Dim tmp, tmp2
    PlaySound "gold"
    ' show some random values on the dmd
	if EnableFlexDMD Then
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "EXTRA BALL IS LIT", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "BONUS MULTIPLIER", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "LEVEL UP", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "BUMPER VALUE", 20
		DMD "black.jpg", " ", "LEVEL UP", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "EXTRA BALL IS LIT", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "BONUS MULTIPLIER", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "LEVEL UP", 20
		DMD "black.jpg", " ", "EXTRA POINTS", 20
		DMD "black.jpg", " ", "BUMPER VALUE", 20
		DMD "black.jpg", " ", "LEVEL UP", 20
		DMD "black.jpg", " ", " ", 200
	Elseif EnablePuPDMD Then

	End If

    tmp = INT(RND(1) * 80)
	if EBAwarded > 1 Then
		tmp = tmp + 5
	End If

		Select Case tmp
        Case 1         'Light Extra Ball
			AudioQueue.Add "ExtraBallIsLit1","PuPEvent 907",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				l23.State = 2:DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
			Elseif EnablePuPDMD Then
				l23.state = 2:pDMDFlash "EXTRA BALL IS LIT", 1, ModeColor
			End If
				LaneAwardEB = "True"
				'ExtraBallLitTimer.Enabled = 1
        Case 2         'Light Extra Ball
			AudioQueue.Add "ExtraBallIsLit1","PuPEvent 907",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				l24.State = 2:DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
			Elseif EnablePuPDMD Then
				l24.state = 2:pDMDFlash "EXTRA BALL IS LIT", 1, ModeColor
			End If
				LaneAwardEB = "True"
				'ExtraBallLitTimer.Enabled = 1
        Case 3         'Light Extra Ball
			AudioQueue.Add "ExtraBallIsLit1","PuPEvent 907",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				l25.State = 2:DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
			Elseif EnablePuPDMD Then
				l25.state = 2:pDMDFlash "EXTRA BALL IS LIT", 1, ModeColor
			End If
				LaneAwardEB = "True"
				'ExtraBallLitTimer.Enabled = 1
        Case 4         'Light Extra Ball
			AudioQueue.Add "ExtraBallIsLit1","PuPEvent 907",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				l26.State = 2:DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
			Elseif EnablePuPDMD Then
				l26.state = 2:pDMDFlash "EXTRA BALL IS LIT", 1, ModeColor
			End If
				LaneAwardEB = "True"
				'ExtraBallLitTimer.Enabled = 1
        Case 5         'Light Extra Ball
			AudioQueue.Add "ExtraBallIsLit1","PuPEvent 907",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				l27.State = 2:DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
			Elseif EnablePuPDMD Then
				l27.state = 2:pDMDFlash "EXTRA BALL IS LIT", 1, ModeColor
			End If
				LaneAwardEB = "True"
				'ExtraBallLitTimer.Enabled = 1
        Case 6, 41, 42 'Start Bumper frenzy, where each bumper hit will award 50x the bumper value. The bumpers will lit blue during this mode.
			if EnableFlexDMD Then
				DMDBlink "black.jpg", " ", "BUMPER FRENZY", 50, 20
				DMD "black.jpg", "BUMPERS", "10X VALUE", 3000   'RTP
			Elseif EnablePuPDMD Then
				
			End If
            StartBumperFrenzy
        Case 7, 8 '2,000,000 points
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "BIG POINTS", "2,000,000", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "BIG POINTS", "2,000,000", 1, ModeColor
			End If
            AddScore 2000000
        Case 9, 10, 11, 12 'Hold Bonus
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "BONUS HELD", "ACTIVATED", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "BONUS HELD", "ACTIVATED", 1, ModeColor				
			End If
            bBonusHeld = True
        Case 13, 14, 15 '1,000,000 points
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "BIG POINTS", "1,000,000", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "BIG POINTS", "1,000,000", 1, ModeColor
			End If
            AddScore 2000000
        Case 16, 17, 18 'Increase Bonus Multiplier
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "INCREASED", "BONUS MULTIPLIER", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "INCREASED", "BONUS MULTIPLIER", 1, ModeColor
			End If
            AddBonusMultiplier 1
        Case 19, 20, 21 'Level up 1
			AudioQueue.Add "LevelUP","PuPEvent 913",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				DMDBlink "black.jpg", " ", "1 LEVEL UP", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlash "1 LEVEL UP", 1, ModeColor
			End If
            Level(CurrentPlayer) = Level(CurrentPlayer) + 1
        Case 22, 23 'Level up 2
			AudioQueue.Add "LevelUP","PuPEvent 913",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				DMDBlink "black.jpg", " ", "2 LEVELS UP", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlash "2 LEVELS UP", 1, ModeColor
			End If
            Level(CurrentPlayer) = Level(CurrentPlayer) + 2
        Case 24, 25, 26, 27, 28 '500,000 points
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "BIG POINTS", "500,000", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "BIG POINTS", "500,000", 1, ModeColor
			End If
            AddScore 500000
        Case 29, 30, 31, 32, 33, 34, 35, 36, 37, 38 'Increase Bumper value
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 300
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "BUMPER VALUE", BumperValue(CurrentPlayer), 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "BUMPER VALUE", BumperValue(CurrentPlayer), 1, ModeColor
			End If
        Case 39, 40, 43, 44 'extra multiball
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "EXTRA", "MULTIBALL", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlash "MULTIBALL", 1, ModeColor
			End If
            AddMultiball 1
        Case 45, 46, 47, 48 ' Ball Save
            EnableBallSaver 20
			if EnableFlexDMD Then
				DMDBlink "black.jpg", "BALL SAVE", "ACTIVATED", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "BALL SAVE", "ACTIVATED", 1, ModeColor
			End If
        Case 50 'Level up 3
			AudioQueue.Add "LevelUP","PuPEvent 913",25,0,0,0,AudioExpiry,False
			if EnableFlexDMD Then
				DMDBlink "black.jpg", " ", "3 LEVELS UP", 50, 20
			Elseif EnablePuPDMD Then
				pDMDFlash "3 LEVELS UP", 1, ModeColor
			End If
            Level(CurrentPlayer) = Level(CurrentPlayer) + 3
        Case ELSE 'Add a Random score from 100,000 to 300,000 points
           ' tmp2 = INT((RND) * 3) * 100000 + 100000
			if EnableFlexDMD Then
				'DMDBlink "black.jpg", "EXTRA POINTS", tmp2, 50, 20
				DMDBlink "black.jpg", "LAIR UNDER", "CONSTRUCTION", 300, 20
			Elseif EnablePuPDMD Then
				pDMDFlashTwoLine "LAIR UNDER", "CONSTRUCTION", 1, ModeColor
			end If
            'AddScore tmp2
    End Select
End Sub

'*****************
'   The Orbit
'*****************
Dim tmpVideo
Sub RandomMutant
	PuPEvent(400)

	TmpHitCount = MonsterTotal - MonsterHits
	if TmpHitCount > 1 Then
		if EnableFlexDMD Then
			DMD "black.jpg", ""&TmpHitCount &" Mutants Left", "", 3000  ' RTP
		Elseif EnablePuPDMD Then
			DisplayLargeHelper TmpHitCount &" Mutants Left", 3000
		End If
	Elseif TmpHitCount > 0 Then
		if EnableFlexDMD Then
			DMD "black.jpg", ""&TmpHitCount &" Mutant Left", "", 3000  ' RTP
		Elseif EnablePuPDMD Then
			DisplayLargeHelper TmpHitCount &" Mutants Left", 3000
		End If
	End IF
End Sub

Sub OrbitTrigger1_Hit
    DOF 205, DOFPulse
    If Tilted Then Exit Sub
    If LastSwitchHit = "OrbitTrigger2" Then 'this is a completed orbit
        OrbitHits = OrbitHits + 1
        GiveRandomAward
    Else
        If l32.State = 2 Then
            Select Case Mode(0)
                Case 1, 3, 5, 7, 9
                    GiEffect 2
                    DOF 125, DOFPulse
                    Monsters(CurrentPlayer) = Monsters(CurrentPlayer) + 1 'bonus
                    MonsterHits = MonsterHits + 1 + Level(CurrentPlayer)
					RandomMutant
                    CheckActMonsters
                    MonsterHitTimer_Timer
                    UpdateModeLights
                Case 4, 6, 10
                    FlashEffect 2
                    DOF 154, DOFPulse
                    LordStrength = LordStrength -7 - Level(CurrentPLayer)
                    CheckLordStrength
                    ' Restart the Lords life restore timerted)
                    LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                    RestartLordLifeTimer
                Case 8 'diablo
                    l32.State = 0:l39.State = 2
                    FlashEffect 2
                    DOF 154, DOFPulse
                    LordStrength = LordStrength -7 - Level(CurrentPLayer)
                    CheckLordStrength
                    ' Restart the Lords life restore timerted)
                    LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                    RestartLordLifeTimer
            End Select
        End If
		if l42.state = 2 Then  'Do Critical Hit Damage
			CriticalHit
		End If
        If bJackpot AND l42.State = 2 Then 'give the Jackpot & enable the next one
            l42.State = 0
            SetLightColor l46, red, 2
            AwardJackpot
        End If
        If l23.State = 2 Then 'give the extra ball
            l23.State = 0
            AwardExtraBall
        End If
    End If
    LastSwitchHit = "OrbitTrigger1"
End Sub

Sub OrbitTrigger2_Hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    If LastSwitchHit = "OrbitTrigger1" Then 'this is a completed orbit
        DOF 206, DOFPulse
        OrbitHits = OrbitHits + 1
        GiveRandomAward
    Else
        If l39.State = 2 Then
            Select Case Mode(0)
                Case 1, 3, 5, 7, 9
                    GiEffect 2
                    DOF 125, DOFPulse
                    Monsters(CurrentPlayer) = Monsters(CurrentPlayer) + 1 'bonus
                    MonsterHits = MonsterHits + 1 + Level(CurrentPlayer)
					RandomMutant
                    CheckActMonsters
                    MonsterHitTimer_Timer
                    UpdateModeLights
                Case 4, 6, 10
                    FlashEffect 2
                    DOF 154, DOFPulse
                    LordStrength = LordStrength -7 - Level(CurrentPLayer)
                    CheckLordStrength
                    ' Restart the Lords life restore timerted)
                    LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                    RestartLordLifeTimer
                Case 8 'diablo
					RandomMutant
                    l32.State = 2:l39.State = 0
                    FlashEffect 2
                    DOF 154, DOFPulse
                    LordStrength = LordStrength -7 - Level(CurrentPLayer)
                    CheckLordStrength
                    ' Restart the Lords life restore timerted)
                    LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                    RestartLordLifeTimer
            End Select
        End If
		if l46.state = 2 Then  'Do Critical Hit Damage
			CriticalHit
		End If
        If bJackpot AND l46.State = 2 Then 'give the Jackpot & enable the next one
            l46.State = 0
            SetLightColor l44, red, 2
            AwardJackpot
        End If
        If l27.State = 2 Then 'give the extra ball
            l27.State = 0
            AwardExtraBall
        End If
    End If
    LastSwitchHit = "OrbitTrigger2"
End Sub

'****************
'     Ramps
'****************

Sub LeftRampDone_Hit
    DOF 200, DOFPulse
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If l33.State = 2 Then
        Select Case Mode(0)
            Case 1, 3, 5, 7, 9
                GiEffect 2
                DOF 125, DOFPulse
                Monsters(CurrentPlayer) = Monsters(CurrentPlayer) + 1 'bonus
                MonsterHits = MonsterHits + 1 + Level(CurrentPlayer)
				RandomMutant
                CheckActMonsters
                MonsterHitTimer_Timer
                UpdateModeLights
            Case 4, 6, 8, 10
                FlashEffect 2
                DOF 154, DOFPulse
                LordStrength = LordStrength -7 - Level(CurrentPLayer)
                CheckLordStrength
                ' Restart the Lords life restore timerted)
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
        End Select
    End If
    If LastSwitchHit = "RightRampDone" Then 'give combo
        DOF 117, DOFPulse
        ComboValue = 100000 + Round(Score(CurrentPlayer) / 10, 0)
		if EnableFlexDMD Then
			DMDBlink "black.jpg", "COMBO", ComboValue, 100, 10
		Elseif EnablePuPDMD Then
			pDMDFlashTwoLine "COMBO", ComboValue, 1, ModeColor
		End If
        AddScore ComboValue
    End If
	if l43.state = 2 Then  'Do Critical Hit Damage
		CriticalHit
	End If
    If bJackpot AND l43.State = 2 Then 'give the Jackpot
        AwardJackpot
        l43.State = 0
        l45.State = 0
        SetLightColor l42, red, 2
    End If
    If l24.State = 2 Then 'give the extra ball
        l24.State = 0
        AwardExtraBall
    End If
    LastSwitchHit = "LeftRampDone"
End Sub

Sub RightRampDone_Hit
    DOF 201, DOFPulse
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If l38.State = 2 Then
        Select Case Mode(0)
            Case 1, 3, 5, 7, 9
                GiEffect 2
                DOF 125, DOFPulse
                Monsters(CurrentPlayer) = Monsters(CurrentPlayer) + 1 'bonus
                MonsterHits = MonsterHits + 1 + Level(CurrentPlayer)
				RandomMutant
                CheckActMonsters
                MonsterHitTimer_Timer
                UpdateModeLights
            Case 4, 6, 8, 10
                FlashEffect 2
                DOF 154, DOFPulse
                LordStrength = LordStrength -7 - Level(CurrentPLayer)
                CheckLordStrength
                ' Restart the Lords life restore timerted)
                LordLifeTimer.Interval = LordLifeInterval + Level(CurrentPlayer) * 200
                RestartLordLifeTimer
        End Select
    End If
    If LastSwitchHit = "LeftRampDone" Then 'give combo
        DOF 117, DOFPulse
        ComboValue = 100000 + Round(Score(CurrentPlayer) / 10, 0)
		if EnableFlexDMD Then
			DMDBlink "black.jpg", "COMBO", ComboValue, 100, 10
		Elseif EnablePuPDMD Then
			pDMDFlashTwoLine "COMBO", ComboValue, 1, ModeColor
		End If
        AddScore ComboValue
    End If
	if l45.state = 2 Then  'Do Critical Hit Damage
		CriticalHit
	End If
    If bJackpot AND l45.State = 2 Then 'give the Jackpot
        AwardJackpot
        l43.State = 0
        l45.State = 0
        SetLightColor l42, red, 2
    End If
    If l26.State = 2 Then 'give the extra ball
        l26.State = 0
        AwardExtraBall
    End If
    LastSwitchHit = "RightRampDone"
End Sub

'************************
'  Mode: Acts & Battles
'************************

' Mode are played one after each other
' This is a cooperative table where each player will continue right after where the last player finished
' If you loose the ball you'll need to restart the mode :)
'

Dim Mode(10) 'this is for easier programming

' Mode(0) will have the current mode number
' when a mode is not active Mode(n) = 0
' when a mode is active Mode(n) = 2
' when a mode is completed Mode(n) = 1
' this is to be consistent with the light states

' Only one mode can be active at a time.

' Mode(1): Act 1
' Mode(2): end of Act 1: Battle the Butcher
' Mode(3): Act 2
' Mode(4): end of Act 2: Battle Belial
' Mode(5): Act 3
' Mode(6): end of Act 3: Battle Azmodan
' Mode(7): Act 4
' Mode(8): end of Act 4: Battle Diablo
' Mode(9): Act 5
' Mode(10): end of Act 5: Battle Malthael

Sub StartNextMode
    ' stop the start act lights & variables
    l34.State = 0
    l44.State = 0
    bActReady = False
'Debug  "In Start Next Mode"
    If Mode(1) = 0 Then
        StartMode 1
		ModeColor = cGreen
    ElseIf Mode(2) = 0 Then
        StartMode 2
		ModeColor = cBlue
    ElseIf Mode(3) = 0 Then
        StartMode 3
		ModeColor = cGreen
    ElseIf Mode(4) = 0 Then
        StartMode 4
		ModeColor = cYellow
    ElseIf Mode(5) = 0 Then
        StartMode 5
		ModeColor = cGreen
    ElseIf Mode(6) = 0 Then
        StartMode 6
		ModeColor = cOrange
    ElseIf Mode(7) = 0 Then
        StartMode 7
		ModeColor = cGreen
    ElseIf Mode(8) = 0 Then
        StartMode 8
		ModeColor = cLightGreen
    ElseIf Mode(9) = 0 Then
        StartMode 9
		ModeColor = cYellow
    ElseIf Mode(10) = 0 Then
        StartMode 10
		ModeColor = cPurple
	Else
		ModeColor = cGrey
    End If
End Sub

Sub StartMode(n)
'Debug  "Start Mode"
    l34.State = 0
	if EnableFlexDMD Then
		DMDFlush
	Elseif EnablePuPDMD Then
	'pDMDSetPage(1)
	End If
    Select Case n
        Case 1:StartAct1
        Case 2:StartPanthro ' The Trial of Strength - Panthro
        Case 3:StartAct2
        Case 4:StartCheetara ' The Trial of Speed - Cheetara
        Case 5:StartAct3
        Case 6:StartKittens ' The Trial of Cunning - Thunderkittens , WilyKat & WilyKit
        Case 7:StartAct4
        Case 8:StartTygra ' The Trial of Mind-Power - Tygra
        Case 9:StartAct5
        Case 10:StartMummRA ' The Trial of Evil - Mumm-Ra
    End Select
    If B2sOn Then
        Controller.B2SSetData 80, 0:Controller.B2SSetData 20, 1
        for ao = 1 to 11:Controller.B2SSetData ao, 0:next
        bsnr = n
    End If
End Sub

Sub StopMode(n) 'called at the end of a ball
'Debug  "In Stop Mode"
    l34.State = 2
	'pDMDSetPage(1)

    Select Case n
        Case 1:StopAct1
        Case 2:StopPanthro
        Case 3:StopAct2
        Case 4:StopCheetara 
        Case 5:StopAct3
        Case 6:StopKittens
        Case 7:StopAct4
        Case 8:StopTygra
        Case 9:StopAct5
        Case 10:StopMummRA
    End Select
    If B2sOn Then
        Controller.B2SSetData bsnr, 0
    End If

	if EnablePuPDMD Then ModeColor = cWhite
End Sub

Sub WinMode(n) 'called after completing a mode
	if EnableFlexDMD Then
		DMDFlush
	Elseif EnablePuPDMD Then
		ModeColor = cWhite
	End If
    Select Case n
        Case 1:WinAct1
        Case 2:WinPanthro
        Case 3:WinAct2
        Case 4:WinCheetara 
        Case 5:WinAct3
        Case 6:WinKittens
        Case 7:WinAct4
        Case 8:WinTygra
        Case 9:WinAct5
        Case 10:WinMummRA
    End Select
End Sub

Sub UpdateModeLights()
    'update the lights according to the mode state
    l7.State = Mode(1)
    l6.State = Mode(2)
    l8.State = Mode(3)
    l9.State = Mode(4)
    l10.State = Mode(5)
    l11.State = Mode(6)
    l12.State = Mode(7)
    l14.State = Mode(8)
    l13.State = Mode(9)
    l47.State = Mode(10)
    'change the color of the active mode depending of how far the act or battle is finished
    Select Case Mode(0)
        Case 1:SetLightColor l7, ColorCode, 2
        Case 2:SetLightColor l6, ColorCode, 2
        Case 3:SetLightColor l8, ColorCode, 2
        Case 4:SetLightColor l9, ColorCode, 2
        Case 5:SetLightColor l10, ColorCode, 2
        Case 6:SetLightColor l11, ColorCode, 2
        Case 7:SetLightColor l12, ColorCode, 2
        Case 8:SetLightColor l14, ColorCode, 2
        Case 9:SetLightColor l13, ColorCode, 2
        Case 10:SetLightColor l47, ColorCode, 2
    End Select
End Sub

Function ColorCode ' gives the percent completed, this will be a number from 0 to 10
    Select Case Mode(0)
        Case 1, 3, 5, 7, 9
            ColorCode = INT((MonsterHits / MonsterTotal) * 10)
        Case 2, 4, 6, 8, 10
            ColorCode = LordStrength \ 10
    End Select
    If ColorCode < 0 Then ColorCode = 0
    If ColorCode > 10 Then ColorCode = 10
End Function

'********************
' Monsters Hit Timer
'********************

Dim mStep:mStep = False

Sub MonsterHitTimer_Timer() ' will change the lights differently in each act
    Dim tmp
    Select Case Mode(0)
        Case 1 ' all 4 lights are turned on
            SetLightColor l32, White, 2
            SetLightColor l33, White, 2
            SetLightColor l38, White, 2
            SetLightColor l39, White, 2
        Case 3 ' 3 lights are turned on
            tmp = INT(RND * 4)
            SetLightColor l32, White, 2
            SetLightColor l33, White, 2
            SetLightColor l38, White, 2
            SetLightColor l39, White, 2
            Select Case tmp
                Case 0:l32.State = 0
                Case 1:l33.State = 0
                Case 2:l38.State = 0
                Case 3:l39.State = 0
            End Select
        Case 5 ' 2 lights, alternate between orbits and ramps
            mStep = NOT mStep
            If mStep Then
                SetLightColor l32, White, 2
                SetLightColor l33, White, 0
                SetLightColor l38, White, 0
                SetLightColor l39, White, 2
            Else
                SetLightColor l32, White, 0
                SetLightColor l33, White, 2
                SetLightColor l38, White, 2
                SetLightColor l39, White, 0
            End If
        Case 7 ' left or right lights
            mStep = NOT mStep
            If mStep Then
                SetLightColor l32, White, 2
                SetLightColor l33, White, 2
                SetLightColor l38, White, 0
                SetLightColor l39, White, 0
            Else
                SetLightColor l32, White, 0
                SetLightColor l33, White, 0
                SetLightColor l38, White, 2
                SetLightColor l39, White, 2
            End If
        Case 9 ' 1 random light at a time
            tmp = INT(RND * 4)
            l32.State = 0
            l33.State = 0
            l38.State = 0
            l39.State = 0
            Select Case tmp
                Case 0:SetLightColor l32, White, 2
                Case 1:SetLightColor l33, White, 2
                Case 2:SetLightColor l38, White, 2
                Case 3:SetLightColor l39, White, 2
            End Select
    End Select
End Sub

Sub StartMonsterTimer
    MonsterHitTimer_Timer 'start one of the lights
    MonsterHitTimer.Enabled = 1
End Sub

Sub StopMonsterTimer
    MonsterHitTimer.Enabled = 0
    l32.State = 0
    l33.State = 0
    l38.State = 0
    l39.State = 0
End Sub

Sub CheckActMonsters
    If MonsterHits >= MonsterTotal Then
        WinMode Mode(0)
    End If
    UpdateModeLights()
End Sub

Sub CheckLordStrength
    If LordStrength <= 0 Then
        WinMode Mode(0)
    End If
    UpdateModeLights()
	if EnableFlexDMD Then
		DMDScore
	Elseif EnablePuPDMD Then

	End If
End Sub

Sub RestartLordLifeTimer()
    LordLifeTimer.Enabled = 0
    LordLifeTimer.Enabled = 1
End Sub

Sub LordLifeTimer_Timer ' increases the Lords strength if not hit
    LordLifeTimer.Interval = LordLifeInterval
    If LordStrength < 100 then
        LordStrength = LordStrength + (LordLifeStep * DiffMod(CurrentPlayer))
        UpdateModeLights()
		if EnableFlexDMD Then
			DMDScore
		Elseif EnablePuPDMD Then

		End If
    End If
End Sub

'****************
' Mode 1: Act 1
'****************
' All the acts use these 2 variables:
' MonsterHits, this the numbers of monsters to kill to end the Act and start the battle against the Lord
' MonsterTotal, the total number of monsters of the act, or the total life of the Lord during battles

Sub StartAct1
Debug  "In Start Trial 1"
	AudioQueue.Add "MutantBattle","PuPEvent 914",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(612)"
	 NotInMutantFight = False
	if EnableFlexDMD Then
		 DMD "black.jpg", "TRIAL I", "Trial of Strength", 3000   'RTP
		 DMD "black.jpg", "Shoot Ramps To", "DEFEAT MUTANTS", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""TRIAL I"", ""Trial of Strength"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""Shoot Ramps To"", ""DEFEAT MUTANTS"", 3000",25,3000,0,0,0,False
	End If

	'MonsterTotal = MB(CurrentPlayer) + INT(RND * 5) ' from 10 to 15 monsters
	MonsterTotal = MB(CurrentPlayer)
    MonsterHits = 0
    Mode(0) = 1                      ' this is the active mode
    Mode(1) = 2                      ' this means it is started, and the corresponding light will be turned on and start blinking
    'update lights
    UpdateModeLights()
    ChangeGi white
    ' Start a timer to light a random arrow light and to change it from time to time
    StartMonsterTimer
End Sub

Sub StopAct1
Debug  "In Stop Trial 1"
	NotInMutantFight = True
    StopMonsterTimer
    Mode(0) = 0
    Mode(1) = 0
    UpdateModeLights()
    ChangeGi white
End Sub

Sub WinAct1
Debug  "In Win Act 1"
	NotInMutantFight = True
    StopMonsterTimer
	AudioQueue.Add "MutantsDefeated","PuPEvent 915",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(603)"
    FlashEffect 2
	DOF 154, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(1) = 1
    'update lights
    UpdateModeLights()
    ChangeGi white
   ' SetLightColor l7, purple, 1
    SetLightColor l7, blue, 1
    ' lit the boss light
    SetLightColor l44, blue, 2
	if EnableFlexDMD Then
		DMD "black.jpg", "MUTANTS", "DEFEATED", 3000   'RTP
		DMDBlink "black.jpg", "EXTRA SCORE", "2,000,000", 100, 5
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""MUTANTS"", ""DEFEATED"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""EXTRA SCORE"", ""2,000,000"", 500",25,3000,0,0,0,False
	End If
	AddScore 2000000
End Sub

Sub EndBossFight
	PuPEvent 815
	NotInBossFight = True
	BattleTimer.Enabled = 0
	pDMDSetPage(1)
	ModeColor = cWhite
	'FlexDMD.Color = &hff1ae8
	'FlexDMD.Color = &hffffff
End Sub
'*********************
' Mode 2: The Butcher
'*********************
' The fights with the Lords use just one variable LordStrength

Sub StartPanthro
Debug "In Start Panthro"
' change DMD color to blue
	NotInBossFight = False
	'FlexDMD.Color = &hFF1111
	if EnableFlexDMD Then
		DMD "black.jpg", "DEFEAT", "PANTHRO", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""DEFEAT"", ""PANTHRO"", 2000",26,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""HIT BUMPERS"", ""TO DEFEAT"", 2000",25,2000,0,0,0,False
		DMDQueue.Add "dmdsetpage5","pDMDSetPage(5)",75,4050,0,0,0,True
		DMDQueue.Add "pup810","PuPEvent 810",25,4100,0,0,0,False
		DMDQueue.Add "BattleTimerOn","BattleTimer.Enabled = 1",25,4150,0,0,0,False
	End If
	if EnableFlexDMD Then TriggerScript 3000,"MessagePANTHRO"
	AudioQueue.Add "PanthroBattle","PuPEvent 916",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(511)"
    LordStrength = BossHealth(CurrentPlayer)		'RTP
    Mode(0) = 2
    Mode(2) = 2
    'update lights
    UpdateModeLights()
    'ChangeGi red
    ChangeGi blue
    ' you need to hit the bumpers to defeat the Butcher so turn them red and blinking
    SetLightColor bumperLight1, red, 2
    SetLightColor bumperLight1a, red, 2
    SetLightColor bumperLight2, red, 2
    SetLightColor bumperLight2a, red, 2
    SetLightColor bumperLight3, red, 2
    SetLightColor bumperLight3a, red, 2
    ' add 1 multiball after the video is finished
	TriggerScript 2000, "AddMultiBall 1"
	'TriggerScript 12000, "AddMultiBall 3"
	PuPEvent(635)
    ' Start the life restore timer
    LordLifeStep = 1
    LordLifeInterval = 6000
    LordLifeTimer.Interval = LordLifeInterval
    RestartLordLifeTimer
    StartJackpots
End Sub

Sub MessagePANTHRO
	if EnableFlexDMD Then
		DMD "black.jpg", "HIT BUMPERS", "TO DEFEAT", 6000   'RTP
	'Elseif EnablePuPDMD Then
		'DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""HIT BUMPERS"", ""TO DEFEAT"", 6000",25,0,0,0,0,False
		'DMDQueue.Add "dmdsetpage5","pDMDSetPage(5)",75,0,0,4050,0,True
		'DMDQueue.Add "pup810","PuPEvent 810",25,4100,0,0,0,False
		'DMDQueue.Add "BattleTimerOn","BattleTimer.Enabled = 1",25,4200,0,0,0,False
	End If
End Sub

Sub StopPanthro
	'PuPEvent(560)
Debug "In Stop Panthro"
    Mode(0) = 0
    Mode(2) = 0
    'update lights
    SetLightColor bumperLight1, white, 0
    SetLightColor bumperLight1a, white, 0
    SetLightColor bumperLight2, white, 0
    SetLightColor bumperLight2a, white, 0
    SetLightColor bumperLight3, white, 0
    SetLightColor bumperLight3a, white, 0
    UpdateModeLights()
    SetLightColor l6, white, 0
    ChangeGi white
	EndBossFight
End Sub

Sub WinPanthro
	'PuPEvent(560)
Debug "In Win Panthro"
	AudioQueue.Add "Panthro Defeat","PuPEvent 917",25,0,0,0,AudioExpiry,False
	EndBossFight
	Triggerscript 2500, "PuPEvent(512)"
	if EnableFlexDMD Then
		DMD "black.jpg", "PANTHRO", "DEFEATED", 7000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""PANTHRO"", ""DEFEATED"", 7000",25,0,0,0,0,False
	End If
    FlashEffect 2
	DOF 154, DOFPulse
    LightEffect 2
    GiEffect 2
	DOF 125, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(2) = 1
    'update lights
    SetLightColor bumperLight1, white, 0
    SetLightColor bumperLight1a, white, 0
    SetLightColor bumperLight2, white, 0
    SetLightColor bumperLight2a, white, 0
    SetLightColor bumperLight3, white, 0
    SetLightColor bumperLight3a, white, 0
    UpdateModeLights()
    'SetLightColor l6, purple, 1
    SetLightColor l6, blue, 1
    ' lit the start act light to show the player that he can start the next mode
    SetLightColor l34, purple, 2
    ChangeGi white
	'EndBossFight
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "EXTRA SCORE", "4,000,000", 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlashTwoLine "EXTRA SCORE", "4,000,000", 1, ModeColor
	End If
	AddScore 4000000
End Sub

'****************
' Mode 3: Act 2
'****************
' All the acts use these 2 variables:
' MonsterHits, this the numbers of monsters to kill to end the Act and start the battle against the Lord
' MonsterTotal, the total number of monsters of the act, or the total life of the Lord during battles

Sub StartAct2
Debug  "In Start Trial 2"
	AudioQueue.Add "MutantBattle","PuPEvent 914",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(612)"
	NotInMutantFight = False
	if EnableFlexDMD Then
		DMD "black.jpg", "TRIAL II", "Trial of Speed", 3000  ' RTP
		DMD "black.jpg", "Shoot Ramps To", "DEFEAT MUTANTS", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""TRIAL II"", ""Trial of Speed"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""Shoot Ramps To"", ""DEFEAT MUTANTS"", 3000",25,3000,0,0,0,False
	End If
    'setup variables
    MonsterTotal = MB(CurrentPlayer) + 8 + INT(RND * 5) ' from 15 to 20 monsters
    MonsterHits = 0
    Mode(0) = 3                      ' this is the active mode
    Mode(3) = 2                      ' this means it is started, and the corresponding light will be turned on and start blinking
    'update lights
    UpdateModeLights()
    ChangeGi white
    ' Start a timer to light a random arrow light and to change it from time to time
    StartMonsterTimer
End Sub

Sub StopAct2
Debug  "In Stop Trial 2"
	NotInMutantFight = True
    StopMonsterTimer
    Mode(0) = 0
    Mode(3) = 0
    UpdateModeLights()
    ChangeGi white
End Sub

Sub WinAct2
Debug  "In Win Act 2"
	NotInMutantFight = True
    StopMonsterTimer
	AudioQueue.Add "MutantsDefeated","PuPEvent 915",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(603)"
    FlashEffect 2
	DOF 154, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(3) = 1
    'update lights
    UpdateModeLights()
    ChangeGi white
    'SetLightColor l8, purple, 1
    SetLightColor l8, yellow, 1
    ' lit the boss light
    SetLightColor l44, yellow, 2
	if EnableFlexDMD Then
		DMD "black.jpg", "MUTANTS", "DEFEATED", 3000   'RTP
		DMDBlink "black.jpg", "EXTRA SCORE", "4,000,000", 100, 5
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""MUTANTS"", ""DEFEATED"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""EXTRA SCORE"", ""4,000,000"", 500",25,3000,0,0,0,False
	End If
	AddScore 4
End Sub

'**********************
' Mode 4: Battle Belial
'**********************
' Belial hides behind the targets.
' Hit the lit targets to kill him
' Do not hit the the off targets, they will increase the life of Belial
Dim TargetsPos

Sub StartCheetara
Debug "In Start Cheetara"
	NotInBossFight = False
	AudioQueue.Add "CheetaBattle","PuPEvent 918",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(521)"
	'FlexDMD.Color = &h2bf9fd
	if EnableFlexDMD Then
		DMD "black.jpg", "DEFEAT", "CHEETARA", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""DEFEAT"", ""CHEETARA"", 2000",26,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""HIT LIT TARGETS"", ""TO DEFEAT"", 2000",25,2000,0,0,0,False
		DMDQueue.Add "dmdsetpage5","pDMDSetPage(5)",75,4050,0,0,0,True
		DMDQueue.Add "pup811","PuPEvent 811",25,4100,0,0,0,False
		DMDQueue.Add "BattleTimerOn","BattleTimer.Enabled = 1",25,4150,0,0,0,False
	End If
	if EnableFlexDMD Then TriggerScript 3000,"MessageCHEETARA"
    LordStrength = BossHealth(CurrentPlayer)
    Mode(0) = 4
    Mode(4) = 2
    'update lights
    UpdateModeLights()
    ChangeGi yellow
    ' you need to hit the lit targets to defeat Belial so turn start
    TargetsPos = 0
    BelialTargets.Enabled = 1
    ' add 2 multiball after the video is finished
	TriggerScript 2000,"AddMultiBall 2 "
	PuPEvent(635)
    ' Start the life restore timer
    LordLifeStep = 2
    LordLifeInterval = 5000
    LordLifeTimer.Interval = LordLifeInterval
    RestartLordLifeTimer
    StartJackpots
End Sub

Sub MessageCHEETARA
	if EnableFlexDMD Then
		DMD "black.jpg", "HIT LIT TARGETS", "TO DEFEAT", 6000   'RTP
	'Elseif EnablePuPDMD Then
	'	DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""HIT LIT TARGETS"", ""TO DEFEAT"", 6000",25,0,0,0,0,False
	End If
End Sub

Sub StopCheetara
	'PuPEvent(560)
Debug "In Stop Cheetara"
    Mode(0) = 0
    Mode(4) = 0
    BelialTargets.Enabled = 0
    UpdateModeLights()
    l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
    ChangeGi white
	'EndBossFight
End Sub

Sub WinCheetara
	'PuPEvent(560)
Debug "In Win Cheetara"
	AudioQueue.Add "CheetaraDefeated","PuPEvent 919",25,0,0,0,AudioExpiry,False
	EndBossFight
	Triggerscript 2500, "PuPEvent(522)"
	if EnableFlexDMD Then
		DMD "black.jpg", "CHEETARA", "DEFEATED", 5000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""CHEETARA"", ""DEFEATED"", 5000",25,0,0,0,0,False
	End If
    FlashEffect 2
	DOF 154, DOFPulse
    LightEffect 2
    GiEffect 2
	DOF 125, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(4) = 1
    'update lights
    BelialTargets.Enabled = 0
    UpdateModeLights()
    l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
    ChangeGi white
    'SetLightColor l9, purple, 1
    SetLightColor l9, yellow, 1
    ' lit the start act light to show the player that he can start the next mode
    SetLightColor l34, purple, 2
	'EndBossFight
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "EXTRA SCORE", "8,000,000", 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlashTwoLine "EXTRA SCORE", "8,000,000", 1, ModeColor
	End If
	AddScore 8000000
End Sub

Sub BelialTargets_Timer()
    Select Case TargetsPos
        Case 0:l28.State = 1:l29.State = 1:l30.State = 1:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
        Case 1:l28.State = 0:l29.State = 1:l30.State = 1:l31.State = 1:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
        Case 2:l28.State = 0:l29.State = 0:l30.State = 1:l31.State = 1:l35.State = 1:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
        Case 3:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 1:l35.State = 1:l36.State = 1:l37.State = 0:l40.State = 0:l41.State = 0
        Case 4:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 1:l36.State = 1:l37.State = 1:l40.State = 0:l41.State = 0
        Case 5:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 1:l37.State = 1:l40.State = 1:l41.State = 0
        Case 6:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 1:l40.State = 1:l41.State = 1
        Case 7:l28.State = 1:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 1:l41.State = 1
        Case 8:l28.State = 1:l29.State = 1:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 1
        Case 9:l28.State = 1:l29.State = 1:l30.State = 1:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
        Case 10:l28.State = 0:l29.State = 1:l30.State = 1:l31.State = 1:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
        Case 11:l28.State = 0:l29.State = 0:l30.State = 1:l31.State = 1:l35.State = 1:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 0
        Case 12:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 1:l35.State = 1:l36.State = 1:l37.State = 0:l40.State = 0:l41.State = 0
        Case 13:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 1:l36.State = 1:l37.State = 1:l40.State = 0:l41.State = 0 ' the 3 lights "SKI" stay lit for a few seconds
        Case 25:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 1:l37.State = 1:l40.State = 1:l41.State = 0
        Case 26:l28.State = 0:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 1:l40.State = 1:l41.State = 1
        Case 27:l28.State = 1:l29.State = 0:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 1:l41.State = 1
        Case 28:l28.State = 1:l29.State = 1:l30.State = 0:l31.State = 0:l35.State = 0:l36.State = 0:l37.State = 0:l40.State = 0:l41.State = 1:TargetsPos = -1
    End Select
    TargetsPos = TargetsPos + 1
End Sub

'****************
' Mode 5: Act 3
'****************
' All the acts use these 2 variables:
' MonsterHits, this the numbers of monsters to kill to end the Act and start the battle against the Lord
' MonsterTotal, the total number of monsters of the act, or the total life of the Lord during battles

Sub StartAct3
Debug  "In Start Trial 3"
	AudioQueue.Add "MutantBattle","PuPEvent 914",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(612)"
	NotInMutantFight = False
	if EnableFlexDMD Then
		DMD "black.jpg", "TRIAL III", "Trial of Cunning", 3000 ' RTP
		DMD "black.jpg", "Shoot Ramps To", "DEFEAT MUTANTS", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""TRIAL III"", ""Trial of Cunning"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""Shoot Ramps To"", ""DEFEAT MUTANTS"", 3000",25,3000,0,0,0,False
	end If
    MonsterTotal = MB(CurrentPlayer) + 15 + INT(RND * 5) ' from 20 to 25 monsters
    MonsterHits = 0
    Mode(0) = 5                      ' this is the active mode
    Mode(5) = 2                      ' this means it is started, and the corresponding light will be turned on and start blinking
    'update lights
    UpdateModeLights()
    ChangeGi white
    ' Start a timer to light a random arrow(s) light and to change it from time to time
    StartMonsterTimer
End Sub

Sub StopAct3
Debug  "In Stop Trial 3"
	NotInMutantFight = True
    StopMonsterTimer
    Mode(0) = 0
    Mode(5) = 0
    UpdateModeLights()
    ChangeGi white
End Sub

Sub WinAct3
Debug  "In Win Act 3"
	NotInMutantFight = True
    StopMonsterTimer
	AudioQueue.Add "MutantsDefeated","PuPEvent 915",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(603)"
	FlashEffect 2
	DOF 154, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(5) = 1
    'update lights
    UpdateModeLights()
    ChangeGi white
    'SetLightColor l10, purple, 1
    SetLightColor l10, orange, 1
    ' lit the boss light
    SetLightColor l44, orange, 2
	if EnableFlexDMD Then
		DMD "black.jpg", "MUTANTS", "DEFEATED", 3000   'RTP
		DMDBlink "black.jpg", "EXTRA SCORE", "6,000,000", 100, 5
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""MUTANTS"", ""DEFEATED"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""EXTRA SCORE"", ""6,000,000"", 500",25,3000,0,0,0,False
	End If
	AddScore 6000000
End Sub

'************************
' Mode 6: Battle Azmodan
'************************
' The fights with the Lords use just one variable LordStrength
' you need to hit at least 10 times the orbits left or right

Sub StartKittens
Debug "In Start Kittens"
	NotInBossFight = False
	AudioQueue.Add "KittensStart","PuPEvent 920",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(531)"
	'FlexDMD.Color = &h1a66ff    ' Orange
	if EnableFlexDMD Then
		DMD "black.jpg", "DEFEAT", "THUNDERKITTENS", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""DEFEAT"", ""THUNDERKITTENS"", 2000",26,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""SHOOT LIT RAMPS"", ""TO DEFEAT"", 2000",25,2000,0,0,0,False
		DMDQueue.Add "dmdsetpage5","pDMDSetPage(5)",75,4050,0,0,0,True
		DMDQueue.Add "pup812","PuPEvent 812",25,4100,0,0,0,False
		DMDQueue.Add "BattleTimerOn","BattleTimer.Enabled = 1",25,4150,0,0,0,False
	End If
	if EnableFlexDMD Then TriggerScript 3000,"MessageKITTENS"
    LordStrength = BossHealth(CurrentPlayer) + 40   ' add extra health as they are too easy
    Mode(0) = 6
    Mode(6) = 2
    'update lights
    UpdateModeLights()
    'ChangeGi green
    ChangeGi orange
    ' turn on the arrow lights
    SetLightColor l32, white, 2
    SetLightColor l39, white, 2
    ' add 3 multiball after the video is finished

	TriggerScript 2000,"AddMultiBall 2 "
	PuPEvent(635)
    ' Start the life restore timer
    LordLifeStep = 2
    LordLifeInterval = 4000
    LordLifeTimer.Interval = LordLifeInterval
    RestartLordLifeTimer
    StartJackpots
End Sub

Sub MessageKITTENS
	if EnableFlexDMD Then
		DMD "black.jpg", "SHOOT LIT RAMPS", "TO DEFEAT", 6000   'RTP
	'Elseif EnablePuPDMD Then
		'DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""SHOOT LIT RAMPS"", "" TO DEFEAT"", 6000",25,0,0,0,0,False
	End If
End Sub

Sub StopKittens
	'PuPEvent(560)
Debug "In Stop Kittens"
    Mode(0) = 0
    Mode(6) = 0
    UpdateModeLights()
    l32.state = 0
    l39.State = 0
    ChangeGi white
	'EndBossFight
End Sub

Sub WinKittens
	'PuPEvent(560)
Debug "In Win Kittens"
	AudioQueue.Add "KittensDefeated","PuPEvent 921",25,0,0,0,AudioExpiry,False
	EndBossFight
	Triggerscript 2500, "PuPEvent(532)"
	if EnableFlexDMD Then
		DMD "black.jpg", "KITTENS", "DEFEATED", 5000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""KITTENS"", ""DEFEATED"", 5000",25,0,0,0,0,False
	End If
    FlashEffect 2
	DOF 154, DOFPulse
    LightEffect 2
    GiEffect 2
	DOF 125, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(6) = 1
    'update lights
    UpdateModeLights()
    l32.state = 0
    l39.State = 0
    ChangeGi white
    'SetLightColor l11, purple, 1
    SetLightColor l11, orange, 1
    ' lit the start act light to show the player that he can start the next mode
    SetLightColor l34, purple, 2
	'EndBossFight
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "EXTRA SCORE", "12,000,000", 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlashTwoLine "EXTRA SCORE", "12,000,000", 1, ModeColor
	End If
	AddScore 12000000
End Sub

'****************
' Mode 7: Act 4
'****************
' All the acts use these 2 variables:
' MonsterHits, this the numbers of monsters to kill to end the Act and start the battle against the Lord
' MonsterTotal, the total number of monsters of the act, or the total life of the Lord during battles

Sub StartAct4
Debug  "In Start Trial 4"
	AudioQueue.Add "MutantBattle","PuPEvent 914",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(612)"
	NotInMutantFight = False
	if EnableFlexDMD Then
		DMD "black.jpg", "TRIAL IV", "Trial of Mind-Power", 3000 ' RTP
		DMD "black.jpg", "Shoot Ramps To", "DEFEAT MUTANTS", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""TRIAL IV"", ""Trial of Mind-Power"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""Shoot Ramps To"", ""DEFEAT MUTANTS"", 3000",25,3000,0,0,0,False
	End If
    MonsterTotal = MB(CurrentPlayer) + 20 + INT(RND * 5) ' from 20 to 25 monsters
    MonsterHits = 0
    Mode(0) = 7                      ' this is the active mode
    Mode(7) = 2                      ' this means it is started, and the corresponding light will be turned on and start blinking
    'update lights
    UpdateModeLights()
    ChangeGi white
    ' Start a timer to light a random arrow light and to change it from time to time
    StartMonsterTimer
End Sub

Sub StopAct4
Debug  "In Stop Trial 4"
	NotInMutantFight = True
    StopMonsterTimer
    Mode(0) = 0
    Mode(7) = 0
    UpdateModeLights()
End Sub

Sub WinAct4
Debug  "In Win Act 4"
	NotInMutantFight = True
    StopMonsterTimer
	AudioQueue.Add "MutantsDefeated","PuPEvent 915",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(603)"
    FlashEffect 2
	DOF 154, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(7) = 1
    'update lights
    UpdateModeLights()
    'SetLightColor l12, purple, 1
    SetLightColor l12, green, 1
    ' lit the boss light
    SetLightColor l44, green, 2
	if EnableFlexDMD Then
		DMD "black.jpg", "MUTANTS", "DEFEATED", 3000   'RTP
		DMDBlink "black.jpg", "EXTRA SCORE", "8,000,000", 100, 5
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""MUTANTS"", ""DEFEATED"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""EXTRA SCORE"", ""8,000,000"", 500",25,3000,0,0,0,False
	End If
	AddScore 8000000
End Sub

'************************
' Mode 8: Battle Diablo
'************************
' The fights with the Lords use just one variable LordStrength

Sub StartTygra
Debug "In Start Tygra"
	NotInBossFight = False
	AudioQueue.Add "TygraBattle","PuPEvent 922",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(541)"
	'FlexDMD.Color = &h05a300 ' Green
	if EnableFlexDMD Then
		DMD "black.jpg", "DEFEAT", "TYGRA", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""DEFEAT"", ""TYGRA"", 2000",26,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""SHOOT LIT RAMPS"", ""TO DEFEAT"", 2000",25,2000,0,0,0,False
		DMDQueue.Add "dmdsetpage5","pDMDSetPage(5)",75,4050,0,0,0,True
		DMDQueue.Add "pup813","PuPEvent 813",25,4100,0,0,0,False
		DMDQueue.Add "BattleTimerOn","BattleTimer.Enabled = 1",25,4150,0,0,0,False
	End If
	if EnableFlexDMD Then TriggerScript 3000,"MessageTYGRA"
    LordStrength = BossHealth(CurrentPlayer) + 30 ' add extra health as they are too easy
    Mode(0) = 8
    Mode(8) = 2
    'update lights
    UpdateModeLights()
    'ChangeGi blue
    ChangeGi green
    ' you need to hit the top lanes and ramps, only one light will be active
    SetLightColor l32, white, 2
    ' Start the life restore timer
    LordLifeStep = 3
    LordLifeInterval = 3000
    LordLifeTimer.Interval = LordLifeInterval
    RestartLordLifeTimer
    ' add 4 multiball after the video is finished
	'TriggerScript 25000,"AddMultiBall 3 "
	TriggerScript 2000,"AddMultiBall (TygraMB) "
	PuPEvent(635)
    StartJackpots
End Sub

Sub MessageTYGRA
	if EnableFlexDMD Then
		DMD "black.jpg", "SHOOT LIT RAMPS", "TO DEFEAT", 6000   'RTP
	'Elseif EnablePuPDMD Then
	'	DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""SHOOT LIT RAMPS"", ""TO DEFEAT"", 6000",25,0,0,0,0,False
	End If
End Sub

Sub StopTygra
	'PuPEvent(560)
Debug "In Stop Tygra"
    Mode(0) = 0
    Mode(8) = 0
    UpdateModeLights()
    l32.State = 0
    l39.State = 0
    ChangeGi white
	'EndBossFight
End Sub

Sub WinTygra
	'PuPEvent(560)
Debug "In Win Tygra"
	EndBossFight
	if EnableFlexDMD Then
		DMD "black.jpg", "TYGRA", "DEFEATED", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""TYGRA"", ""DEFEATED"", 3000",25,0,0,0,0,False
	End If
	AudioQueue.Add "TygraDefeated","PuPEvent 923",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(542)"
    FlashEffect 2
	DOF 154, DOFPulse
    LightEffect 2
    GiEffect 2
	DOF 125, DOFPulse
    'setup variables
    Mode(0) = 0
    Mode(8) = 1
    'update lights
    UpdateModeLights()
    l32.State = 0
    l39.State = 0
    ChangeGi white
    'SetLightColor l14, purple, 1
    SetLightColor l14, green, 1
    ' lit the start act light to show the player that he can start the next mode
    SetLightColor l34, purple, 2
	'EndBossFight
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "EXTRA SCORE", "16,000,000", 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlashTwoLine "EXTRA SCORE", "16,000,000", 1, ModeColor
	End If
	AddScore 16000000
End Sub

'****************
' Mode 9: Act 5
'****************
' All the acts use these 2 variables:
' MonsterHits, this the numbers of monsters to kill to end the Act and start the battle against the Lord
' MonsterTotal, the total number of monsters of the act, or the total life of the Lord during battles

Sub StartAct5
Debug  "In Start Trial 5"
	AudioQueue.Add "MutantBattle","PuPEvent 914",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(612)"
	NotInMutantFight = False
	if EnableFlexDMD Then
		DMD "black.jpg", "TRIAL V", "Trial of EVIL", 3000 ' RTP
		DMD "black.jpg", "Shoot Ramps To", "DEFEAT MUTANTS", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""TRIAL V"", ""Trial of Evil"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""Shoot Ramps To"", ""DEFEAT MUTANTS"", 3000",25,3000,0,0,0,False
	End If
    'setup variables
    MonsterTotal = MB(CurrentPlayer) + 30 + INT(RND * 5) ' from 20 to 25 monsters
    MonsterHits = 0
    Mode(0) = 9                      ' this is the active mode
    Mode(9) = 2                      ' this means it is started, and the corresponding light will be turned on and start blinking
    'update lights
    UpdateModeLights()
    ChangeGi white
    ' Start a timer to light a random arrow light and to change it from time to time
    StartMonsterTimer
End Sub

Sub StopAct5
Debug  "In Stop Trial 5"
	NotInMutantFight = True
    StopMonsterTimer
    Mode(0) = 0
    Mode(9) = 0
    UpdateModeLights()
End Sub

Sub WinAct5
Debug  "In Win Act 5"
	NotInMutantFight = True
    StopMonsterTimer
	AudioQueue.Add "MutantsDefeated","PuPEvent 915",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(603)"
    FlashEffect 2
	DOF 154, DOFPulse
    ' Score an extra 5 million points
	if EnableFlexDMD Then
		DMD "black.jpg", "MUTANTS", "DEFEATED", 3000   'RTP
		DMDBlink "black.jpg", "EXTRA SCORE", "15,000,000", 100, 5
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""MUTANTS"", ""DEFEATED"", 3000",25,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""EXTRA SCORE"", ""15,000,000"", 500",25,3000,0,0,0,False
	End If
    Addscore 15000000
    'setup variables
    Mode(0) = 0
    Mode(9) = 1
    'update lights
    UpdateModeLights()
    SetLightColor l13, red, 1
    ' lit the boss light
    SetLightColor l44, red, 2
End Sub

'************************
' Mode 10: Battle Malthael
'************************
' The fights with the Lords use just one variable LordStrength

Sub StartMummRA
Debug "In Start MummRA"
MummRAPupTimer.Enabled = True
	NotInBossFight = False
	AudioQueue.Add "MummRAStart","PuPEvent 924",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(551)"
	'FlexDMD.Color = &h1c1cfd
	if EnableFlexDMD Then
		DMD "black.jpg", "DEFEAT", "MUMM-RA", 3000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""DEFEAT"", ""MUMM-RA"", 2000",26,0,0,0,0,False
		DMDQueue.Add "TwoLineHelper-2","TwoLineHelper ""SHOOT LIT RAMPS"", ""TO DEFEAT"", 2000",25,2000,0,0,0,False
		DMDQueue.Add "dmdsetpage5","pDMDSetPage(5)",75,4050,0,0,0,True
		DMDQueue.Add "pup814","PuPEvent 814",25,4100,0,0,0,False
		DMDQueue.Add "BattleTimerOn","BattleTimer.Enabled = 1",25,4150,0,0,0,False
	End If
	if EnableFlexDMD Then TriggerScript 3000,"MessageMUMMRA"
    LordStrength = BossHealth(CurrentPlayer)
    Mode(0) = 10
    Mode(10) = 2
    'update lights
    UpdateModeLights()
    ChangeGi purple
    ' you need to hit the ramps
    SetLightColor l33, white, 2
    SetLightColor l38, white, 2
    ' add 4 multiball after the video is finished
	'Triggerscript 18000,"AddMultiBall 3"
	TriggerScript 2000,"AddMultiBall (MummraMB) "
	PuPEvent(635)
    ' Start the life restore timer
    LordLifeStep = 4
    LordLifeInterval = 2000
    LordLifeTimer.Interval = LordLifeInterval
    RestartLordLifeTimer
    StartJackpots
End Sub

Sub MessageMUMMRA
	if EnableFlexDMD Then
		DMD "black.jpg", "SHOOT LIT RAMPS", "TO DEFEAT", 6000   'RTP
	'Elseif EnablePuPDMD Then
	'	DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""SHOOT LIT RAMPS"", ""TO DEFEAT"", 6000",25,0,0,0,0,False
	End If
End Sub

Sub StopMummRA
	'PuPEvent(560)
Debug "In Stop MummRA"
    Mode(0) = 0
    Mode(10) = 0
    UpdateModeLights()
    l33.State = 0
    l38.State = 0
    ChangeGi white
End Sub

Sub WinMummRA
	'PuPEvent(560)
Debug "In Win MummRA 1"
    'setup variables
    Mode(0) = 11 'the end
    Mode(10) = 1
    'update lights
    l33.State = 0
    l38.State = 0
    UpdateModeLights()
    ChangeGi white
    SetLightColor l47, red, 1
	if EnableFlexDMD Then
		DMD "black.jpg", "ENTER THE LAIR", "TO KILL MUMM-RA", 5000
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""ENTER THE LAIR"", ""TO KILL MUMM-RA"", 5000",25,0,0,0,0,False
	End If
    MalthaelAttackTimer.Enabled = 0
    ChestDoor.IsDropped = 1
	DOF 116, DOFPulse
    ' lit the 3 chest lights
    SetLightColor l44, purple, 2
    SetLightColor l34, purple, 2
    SetLightColor l25, purple, 2
    FlashEffect 2
	DOF 154, DOFPulse
End Sub

Sub WinMummRA2
Debug "In Win MummRA 2"
	MummRAPupTimer.Enabled = False
	EndBossFight
	PuPEvent(607)  ' Clear BG - Stop MummRA Battle Video if playing
	CheckVideoSkipTimer.Enabled = True
	MummRADefeatedExpiredTimer.Enabled = True

    Dim i
	AudioQueue.Add "MummRADefeated","PuPEvent 925",25,0,0,0,AudioExpiry,False
	Triggerscript 2500, "PuPEvent(552)"
	if EnableFlexDMD Then
		DMD "black.jpg", "MUMM-RA", "DEFEATED", 4000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""MUMM-RA"", ""DEFEATED"", 4000",25,0,0,0,0,False
	End If
    'light show
    LightSeqFlasher.UpdateInterval = 150
    LightSeqFlasher.Play SeqRandom, 20, , 80000
    LightSeqInserts.UpdateInterval = 150
    LightSeqInserts.Play SeqRandom, 50, , 80000
    ' Score an extra 30 million points
	if EnableFlexDMD Then
		DMDBlink "black.jpg", "EXTRA SCORE", "30,000,000", 100, 5
	Elseif EnablePuPDMD Then
		pDMDFlashTwoLine "EXTRA SCORE", "30,000,000", 1, ModeColor
	End If
	AddScore 30000000
    'reset all lights and modes
    For i = 0 to 10
        Mode(i) = 0
    Next

    SecondRound = 2 'used to multiply by 2 all the scores in the scond round
	PSDIncreaseDifficulty  ' increase difficulty setting
    UpdateModeLights()
    l25.State = 0
    l34.State = 0
    l44.State = 0
    ' lit the start act light to show the player that he can start the next mode
    SetLightColor l34, purple, 2
	'EndBossFight
	if EnableFlexDMD Then
		DMD "black.jpg", "PRESS START KEY", "RELEASES BALL", 113000   'RTP
	Elseif EnablePuPDMD Then
		DMDQueue.Add "TwoLineHelper-1","TwoLineHelper ""PRESS START KEY"", ""RELEASES BALL"", 11300",25,0,0,0,0,False
	End If
End Sub

' this is the Malthael attack timer, starts at 2 attacks per minute and increases the frecuency on each act or battle
' In the last battle he will attack each 10 seconds. Each attack will be from 1 to 5 seconds long depending on the mode chosen.

Sub MalthaelAttackTimer_Timer
    Dim tmp
    tmp = Mode(0)
    MalthaelAttackTimer.Interval = 30000 - tmp * 2000
    ' play a sound
	'if NotInBossFight = True Then
		'Debug "Not in Boss Fight " &NotInBossFight
		'DMD "blue-lightning2.wmv", " ", "         ", 4100
	'End If
    PlaySound "di_thunder"
	'AudioQueue.Add "Thunder","PuPEvent 701",25,0,0,0,AudioExpiry,False
    ' flash the flashers
    tmp = 1000 + MummRAMode * 2000 '1, 3 or 5 seconds attack
    FlashForMs f2, tmp , 50, 0
    DOF 151, DOFPulse
    FlashForMs f14, tmp, 50, 0
    DOF 152, DOFPulse
    FlashForMs f15, tmp, 50, 0
    DOF 153, DOFPulse
    FlashForMs ray1, tmp, 50, 0
    FlashForMs ray2, tmp, 50, 0
    ' enable the magnets for a few seconds
    EnableMagnets
	TriggerScript tmp,"DisableMagnets"
End Sub

Sub DisableMagnets
    'debug.print "magnets off"
    DOF 127, DOFOff
    If MummRAMode = 0 Then
        LMagnet.MagnetOn = False
        RMagnet.MagnetOn = False
    Else
        LMagnet.MotorOn = False
        RMagnet.MotorOn = False
    End If
End Sub

Sub EnableMagnets
    DOF 127, DOFOn
    If MummRAMode = 0 Then
        LMagnet.MagnetOn = True
        RMagnet.MagnetOn = True
    Else
        LMagnet.MotorOn = True
        RMagnet.MotorOn = True
   End If
End Sub

Sub DebugTimer_Timer()
	CurrTime = Timer
End Sub


Sub CheckVideoSkipTimer_Timer()
		' Check for Intertupt  bSkipBossVideo
	if bSkipBossVideo  = True Then
		PuPEvent(607)
		if EnableFlexDMD Then
			DMDFLush
		Elseif EnablePuPDMD Then

		End If
		bSkipBossVideo = False
		ChestExit
		MummRADefeatedExpiredTimer.Enabled = False
		CheckVideoSkipTimer.Enabled = False
	End If
End Sub

Sub CheckGOSkipTimer_Timer()
	Debug " Checking GO Skip: "&bSkipGameOverVideo
		' Check for Intertupt  bSkipGameOverVideo
	if bSkipGameOverVideo = True Then
		Debug " Should exit game over video"
		PuPEvent(607)
		if EnableFlexDMD Then
			DMDFLush
		Elseif EnablePuPDMD Then

		End If
		bSkipGameOverVideo = False
		GameOverExpiredTimer.Enabled = False
		CheckGOSkipTimer.Enabled = False
	End If
End Sub

Sub MummRADefeatedExpiredTimer_Timer()
		bSkipBossVideo = False
		ChestExit
End Sub

Sub MummRAPupTimer_Timer()
	PuPEvent(610)
	MummRAPupTimer.Enabled = False
	TriggerScript 15000,"MummRAPupTimer.Enabled = True"
End Sub

Sub GameOverExpiredTimer_Timer()
	bSkipGameOverVideo = False
End Sub


'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs
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
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
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

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity


'*******************************************
' Early 90's and after

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, -5.5
		x.AddPt "Polarity", 2, 0.4, -5.5
		x.AddPt "Polarity", 3, 0.6, -5.0
		x.AddPt "Polarity", 4, 0.65, -4.5
		x.AddPt "Polarity", 5, 0.7, -4.0
		x.AddPt "Polarity", 6, 0.75, -3.5
		x.AddPt "Polarity", 7, 0.8, -3.0
		x.AddPt "Polarity", 8, 0.85, -2.5
		x.AddPt "Polarity", 9, 0.9,-2.0
		x.AddPt "Polarity", 10, 0.95, -1.5
		x.AddPt "Polarity", 11, 1, -1.0
		x.AddPt "Polarity", 12, 1.05, -0.5
		x.AddPt "Polarity", 13, 1.1, 0
		x.AddPt "Polarity", 14, 1.3, 0

		x.AddPt "Velocity", 0, 0,	   1
		x.AddPt "Velocity", 1, 0.160, 1.06
		x.AddPt "Velocity", 2, 0.410, 1.05
		x.AddPt "Velocity", 3, 0.530, 1'0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF 
	RF.SetObjects "RF", RightFlipper, TriggerRF 
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************
 
' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers. 
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger) 
'   Otherwise it should function exactly the same as before
 
Class FlipperPolarity 
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	private Name
 
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
 
	Public Sub SetObjects(aName, aFlipper, aTrigger) 
 
		if typename(aName) <> "String" then msgbox "FlipperPolarity: .SetObjects error: first argument must be a string (and name of Object). Found:" & typename(aName) end if
		if typename(aFlipper) <> "Flipper" then msgbox "FlipperPolarity: .SetObjects error: second argument must be a flipper. Found:" & typename(aFlipper) end if
		if typename(aTrigger) <> "Trigger" then msgbox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & typename(aTrigger) end if
		if aFlipper.EndAngle > aFlipper.StartAngle then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper : FlipperStart = aFlipper.x 
		FlipperEnd = Flipper.Length * sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
 
		dim str : str = "sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)  
 
	End Sub
 
	Public Property Let EndPoint(aInput) :  : End Property ' Legacy: just no op 
 
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function		'Timer shutoff for polaritycorrect
 
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				end if
			Next
 
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
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
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
 
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			if DebugOn then debug.print "PolarityCorrect" & " " & Name & " @ " & gametime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
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
	for x = 0 to uBound(aArray)		'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)		'Resize original array
	for x = 0 to aCount-1				'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)		'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
 
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )		 'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )		'Clamp upper
 
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	   Dim BOT
	   BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

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
	Case 0
	SOSRampup = 2.5
	Case 1
	SOSRampup = 6
	Case 2
	SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
'Debug " In Flipper Deact"
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
				BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
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
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle)	'-1 for Right Flipper
	Dim LiveCatchBounce																														'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime
	CatchTime = GameTime - FCount
	
	If CatchTime <= LiveCatch And parm > 6 And Abs(Flipper.x - ball.x) > LiveDistanceMin And Abs(Flipper.x - ball.x) < LiveDistanceMax Then
		If CatchTime <= LiveCatch * 0.5 Then												'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		Else
			LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)		'Partial catch when catch happens a bit late
		End If
		
		If LiveCatchBounce = 0 And ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx = 0
		ball.angmomy = 0
		ball.angmomz = 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************
'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script in-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'   debug.print " " 
		End If
	End Sub
End Class






Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " in vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = getballs
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
Sub RDampen_Timer
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************
Sub TestSub
Debug "In Test Sub"
'PSDIncreaseDifficulty	
'PuPEvent(700)
'UpdateSkillShot

'DMDBlink "black.jpg", "LAIR UNDER", "CONSTRUCTION", 50, 20
End Sub


Dim CurrMode
Sub BattleStatusTimer_Timer()
CurrMode = Mode(0)
	'Debug "Mode0: "&CurrMode
	if Mode(CurrMode) = 0 And BallsOnPlayfield < 3 Then
		if EnableFlexDMD Then
			DMD "black.jpg", "ENTER CAT LAIR", "TO START BATTLE", 3000   'RTP
		Elseif EnablePuPDMD Then
			TwoLineHelper "ENTER CAT LAIR", "TO START BATTLE", 3000
'			DMDQueue.Add "pDMDSetPage-3","pDMDSetPage(3)",25,0,0,0,0,False
'			PuPlayer.LabelSet pDMD,"Line1","ENTER CAT LAIR",1,""
'			PuPlayer.LabelSet pDMD,"Line2","TO START BATTLE",1,""
'			DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",25,3000,0,0,0,False
		End If
	End If
End Sub

sub rtp

end sub
'*********************************************************************************************************************************
'******************************* DMD PUP FRAMEWORK SUPPORT *****************************************************
'*********************************************************************************************************************************

'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName

PuPlayer.LabelInit pBackglass
PuPlayer.LabelInit pDMD

pSetPageLayouts

pDMDSetPage(1)   'set blank text overlay page.
pBackglassSetPage(1)
'pDMDStartUP				 ' firsttime running for like an startup video..
if ScorbitActive = 1 Then DelayPairing


	if Scorbitactive then 
		if Scorbit.DoInit(4110, "PupOverlays", myVersion, "tcats") then 	' Prod
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End if 

End Sub 'end PUPINIT

sub delayPairing
	BallHandlingQueue.Add "CheckPairing","CheckPairing",25,2500,0,0,0,False
'TriggerScript 2500, "CheckPairing"
end sub

sub CheckPairing
		Debug4 "In check pairing"

	if (Scorbit.bNeedsPairing) then 
		Debug4 "Scorbit Needs Pairing"
		PuPEvent 801
		pBackglassLabelSetSizeImage "ScorbitQR1",26.5,38
		pBackglassLabelSetPos "ScorbitQR1",49.75,72.25
		pBackglasslabelshow "ScorbitQR1"
		pBackglasslabelshow "ScorbitQRIcon1"
		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	end if
End sub

Sub hideScorbit
	pBackglasslabelhide "ScorbitQR1"
	pBackglasslabelhide "ScorbitQRIcon1"
	pBackglasslabelhide "ScorbitQR1"
	pBackglasslabelhide "ScorbitQRIcon1"
End Sub

Sub pDMDFlash(msgText,timeSec,mColor)
'	if PDMDCurPage <> 5 Then
		PuPEvent 815
		ClearLargeMessage
		pDMDSetPage(2)
		PuPlayer.LabelSet pDMD,"Flash",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'color':" & mColor & "}"
		if NotInBossFight Then
			DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,(timeSec*1000),0,0,0,True
		Else
			if pDMDCurPage <> 5 Then
				DMDQueue.Add "pDMDSetPage-5","LoadBG:pDMDSetPage(5)",95,(timeSec*1000),0,0,0,True
			End If
		End If
'	End If
end Sub


sub ClearLargeMessage
	PuPlayer.LabelSet pDMD,"LargeMessage","",0,""
End Sub

sub DisplayLarge(lTitle)
'if PDMDCurPage <> 5 Then
		ClearLargeMessage
		PuPEvent 815
		PuPlayer.LabelSet pDMD,"LargeMessage",lTitle,1,"{'mt':2,'color':" & ModeColor & "}"
'	End If
end sub

sub DisplayLargeHelper(lTitle, lTime)
'	if PDMDCurPage <> 5 Then
		PuPEvent 815
		ClearLargeMessage
		pDMDSetPage(2)
		PuPlayer.LabelSet pDMD,"LargeMessage",lTitle,1,"{'mt':2,'color':" & ModeColor & "}"
		if NotInBossFight Then
			DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,lTime,0,0,0,True
		Else
			if pDMDCurPage <> 5 Then
				DMDQueue.Add "pDMDSetPage-5","LoadBG:pDMDSetPage(5)",95,lTime,0,0,0,True
			End if
		End If
'	End If
end Sub

Sub pDMDFlashTwoLine(lOne,lTwo,timeSec,mColor)
'	if PDMDCurPage <> 5 Then
		PuPEvent 815
		ClearLargeMessage
		pDMDSetPage(2)
		PuPlayer.LabelSet pDMD,"FLine1",lOne,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'color':" & mColor & "}"
		PuPlayer.LabelSet pDMD,"FLine2",lTwo,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'color':" & mColor & "}"
		if NotInBossFight Then
			DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,(timeSec*1000),0,0,0,True
		Else
			if pDMDCurPage <> 5 Then
				DMDQueue.Add "pDMDSetPage-5","LoadBG:pDMDSetPage(5)",95,(timeSec*1000),0,0,0,True
			End if
		End If
'	End If
end Sub

Sub TwoLineHelper(lOne,lTwo,lTime)
'	if PDMDCurPage <> 5 Then
		PuPEvent 815
		ClearTwoLine
		pDMDSetPage(3)
		PuPlayer.LabelSet pDMD,"Line1",lOne,1,"{'mt':2,'color':" & ModeColor & "}"
		PuPlayer.LabelSet pDMD,"Line2",lTwo,1,"{'mt':2,'color':" & ModeColor & "}"
		if NotInBossFight Then
			DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,lTime,0,0,0,True
		Else
			if pDMDCurPage <> 5 Then
				DMDQueue.Add "pDMDSetPage-5","LoadBG:pDMDSetPage(5)",95,lTime,0,0,0,True
			End if
		End If
'	End If
End Sub

Sub HighScoreHelper(lOne,lTwo,lTime)
	PuPEvent 815
	ClearHighScoreTwoLine
	pDMDSetPage(4)
	PuPlayer.LabelSet pDMD,"HSLine1",lOne,1,"{'mt':2,'color':" & ModeColor & "}"
	PuPlayer.LabelSet pDMD,"HSLine2",lTwo,1,"{'mt':2,'color':" & ModeColor & "}"
	DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,lTime,0,0,0,True
End Sub

Sub EOBHelper(lOne,lTwo)
	PuPEvent 815
	ClearTwoLine
	PuPlayer.LabelSet pDMD,"FLine1",lOne,1,"{'mt':2,'color':" & ModeColor & "}"
	PuPlayer.LabelSet pDMD,"FLine2",lTwo,1,"{'mt':2,'color':" & ModeColor & "}"
End Sub

sub ClearTwoLine
	PuPlayer.LabelSet pDMD,"Line1","",0,""
	PuPlayer.LabelSet pDMD,"Line2","",0,""
	PuPlayer.LabelSet pDMD,"FLine1","",0,""
	PuPlayer.LabelSet pDMD,"FLine2","",0,""
End Sub

sub ClearHighScoreTwoLine
	PuPlayer.LabelSet pDMD,"HSLine1","",0,""
	PuPlayer.LabelSet pDMD,"HSLine2","",0,""
End Sub

'PinUP Player DMD Helper Functions
sub pBackglassLabelSetPos(labName, xpos, ypos)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pBackglassLabelSetSizeImage(labName, lWidth, lHeight)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub

Sub AudioDuckPuP(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" }"             
end Sub

Sub AudioDuckPuPAll(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" , ""ALL"":1 }"             
end Sub

Sub pDMDSetTextQuality(AALevel)  '0 to 4 aa.  4 is sloooooower.  default 1,  perhaps use 2-3 if small desktop view.  only affect text quality.  can set per label too with 'qual' settings.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":52, ""SC"": "& AALevel &" }"    'slow pc mode
end Sub  


Sub pSetAspectRatio(PuPID, arWidth, arHeight)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 50, ""WIDTH"": "&arWidth&", ""HEIGHT"": "&arHeight&" }"   
end Sub  

Sub pDMDLabelFlash(LabName,byVal timeSec, mColor)   'timeSec in ms
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec) & ",'fc':" & mColor & "}"   
end sub



Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

sub pDMDPNGAnimate(labName,cSpeed)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&"}" 
end sub

sub pDMDPNGAnimateEx(labName,startFrame,endFrame,LoopMode)  'sets up the apng/gif settings before you call animate.  if you set start/end frame same if will display that frame, set start to -1 to reset settings.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&startFrame&",'gifend':"&endFrame&",'gifloop':"&loopMode&" }"          'gifstart':3, 'gifend':10, 'gifloop': 1
end sub

sub pDMDPNGShowFrame(labName,fFrame)  'in a animated png/gif, will set it to an individual frame so you could use as an imagelist control
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&fFrame&",'gifend':"&fFrame&" }"          '
end sub

sub pDMDPNGAnimateOnce(labName,cSpeed)  'will show an animated gif/png and then hide when done, overrides loop to force stop at end.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&", 'gifloop': 0 , 'aniendhide':1 }" 
end sub

sub pDMDPNGAnimateReset(labName)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer, this will show anigif and hide at end no loop
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':0, 'gifloop': 1 , 'aniendhide':0 , 'gifstart':-1}" 
end sub

sub pDMDLabelSetColor(labName, lCol)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&lCol&"}" 
end sub

Sub pDMDLabelSendToFront(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'ztop': 1 }"   
end sub

sub pDMDLabelPulseText(LabName,LabValue,mLen,mColor)       'mlen in ms
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumber(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0 }"    
end Sub

sub pDMDLabelPulseImage(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0 }"
end Sub

sub pDMDLabelPulseTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  same subs as above but youspecifiy zoom start and zoom end in % height of original font.
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumberEX(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat,isVis,zStart,zEnd)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0}"    
end Sub

sub pDMDLabelPulseImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0 }"
end Sub


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
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pBackglassSetPage(pagenum)    
    PuPlayer.LabelShowPage pBackglass,pagenum,0,""   'set page to blank 0 page if want off
    PBGCurPage=pagenum
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

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	PuPlayer.LabelNew pBackglass,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pBackglass,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub

Sub pBackglassLabelHide(labName)
PuPlayer.LabelSet pBackglass,labName,"",0,""   
end sub

Sub pBackglassLabelShow(labName)
PuPlayer.LabelSet pBackglass,labName,"",1,""   
end sub


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

Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
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
	'pUpdateScores    

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
'RTP7
Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

	dmdalt="Titania"    
    dmdfixed="ThunderCats-Ho!"
	dmdscr="Fundamental Brigade"  'main score font
	dmddef="AvantGarde LT Medium"

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we’d set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.

	pupCreateLabelImage "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,1,0

	pupCreateLabelImage "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,1,0

	PuPlayer.LabelNew pBackglass,"FinalScore",		dmddef,	15,255	,0,1,1,50,5,1,0

if DMDType = 1 Then

'page 1
	PuPlayer.LabelNew pDMD,"CurrScore",         dmddef,24,cRed   ,0,1,1, 38.5,42.0,1,0
	PuPlayer.LabelNew pDMD,"Player",	             dmddef,12,cPurple   ,0,1,1,86.5,38,1,0
	PuPlayer.LabelNew pDMD,"Ball",	             dmddef,12,cPurple   ,0,1,1,87,57,1,0
	PuPlayer.LabelNew pDMD,"Level",               dmddef,12,cPurple  ,0,1,1, 14,57,1,0
	PuPlayer.LabelNew pDMD,"Mutants",	             dmddef,12,cPurple   ,0,1,1,51,57,1,0

'page 2
	PuPlayer.LabelNew pDMD,"LargeMessage",	             dmddef,26,cPurple   ,0,1,1,50,48,2,0
	PuPlayer.LabelNew pDMD,"Flash",	             dmddef,26,cPurple   ,0,1,1,50,48,2,0

' page 3
	PuPlayer.LabelNew pDMD,"Line1",	             dmdalt,16,cPurple   ,0,1,1,50,40,3,0
	PuPlayer.LabelNew pDMD,"Line2",	             dmdalt,16,cPurple   ,0,1,1,50,55,3,0


	PuPlayer.LabelNew pDMD,"FLine1",	             dmdalt,16,cPurple   ,0,1,1,50,40,3,0
	PuPlayer.LabelNew pDMD,"FLine2",	             dmdalt,16,cPurple   ,0,1,1,48,55,3,0


'page 4
	PuPlayer.LabelNew pDMD,"HSLine1",	             dmdalt,16,cPurple   ,0,1,1,50,40,4,0
	PuPlayer.LabelNew pDMD,"HSLine2",	             dmdalt,16,cPurple   ,0,1,1,50,55,4,0

'page 5
	PuPlayer.LabelNew pDMD,"Health",	             dmddef,18,cPurple   ,0,1,1,13.5,50,5,0

Elseif DMDType = 2 Then
'page 1
	PuPlayer.LabelNew pDMD,"CurrScore",         dmddef,24,cRed   ,0,1,1, 38.5,75.0,1,0
	PuPlayer.LabelNew pDMD,"Player",	             dmddef,12,cPurple   ,0,1,1,86.5,71,1,0
	PuPlayer.LabelNew pDMD,"Ball",	             dmddef,12,cPurple   ,0,1,1,87,92,1,0
	PuPlayer.LabelNew pDMD,"Level",               dmddef,12,cPurple  ,0,1,1, 14,92,1,0
	PuPlayer.LabelNew pDMD,"Mutants",	             dmddef,12,cPurple   ,0,1,1,51,92,1,0

'page 2
	PuPlayer.LabelNew pDMD,"LargeMessage",	             dmddef,26,cPurple   ,0,1,1,50,82,2,0
	PuPlayer.LabelNew pDMD,"Flash",	             dmddef,26,cPurple   ,0,1,1,50,82,2,0

' page 3
	PuPlayer.LabelNew pDMD,"Line1",	             dmdalt,16,cPurple   ,0,1,1,50,73,3,0
	PuPlayer.LabelNew pDMD,"Line2",	             dmdalt,16,cPurple   ,0,1,1,50,90,3,0

	PuPlayer.LabelNew pDMD,"FLine1",	             dmdalt,16,cPurple   ,0,1,1,50,73,3,0
	PuPlayer.LabelNew pDMD,"FLine2",	             dmdalt,16,cPurple   ,0,1,1,48,90,3,0

'page 4
	PuPlayer.LabelNew pDMD,"HSLine1",	             dmdalt,16,cPurple   ,0,1,1,50,73,4,0
	PuPlayer.LabelNew pDMD,"HSLine2",	             dmdalt,16,cPurple   ,0,1,1,50,90,4,0

'page 5
	PuPlayer.LabelNew pDMD,"Health",	             dmddef,18,cPurple   ,0,1,1,12.5,86.5,5,0

Elseif DMDType = 3 Then
'page 1
	PuPlayer.LabelNew pDMD,"CurrScore",         dmddef,24,cRed   ,0,1,1, 38.5,40.0,1,0
	PuPlayer.LabelNew pDMD,"Player",	             dmddef,12,cPurple   ,0,1,1,86.5,12,1,0
	PuPlayer.LabelNew pDMD,"Ball",	             dmddef,12,cPurple   ,0,1,1,87,92,1,0
	PuPlayer.LabelNew pDMD,"Level",               dmddef,12,cPurple  ,0,1,1, 14,92,1,0
	PuPlayer.LabelNew pDMD,"Mutants",	             dmddef,12,cPurple   ,0,1,1,51,92,1,0

'page 2
	PuPlayer.LabelNew pDMD,"LargeMessage",	             dmddef,26,cPurple   ,0,1,1,50,48,2,0
	PuPlayer.LabelNew pDMD,"Flash",	             dmddef,26,cPurple   ,0,1,1,50,48,2,0

' page 3
	PuPlayer.LabelNew pDMD,"Line1",	             dmdalt,16,cPurple   ,0,1,1,50,30,3,0
	PuPlayer.LabelNew pDMD,"Line2",	             dmdalt,16,cPurple   ,0,1,1,50,70,3,0


	PuPlayer.LabelNew pDMD,"FLine1",	             dmdalt,16,cPurple   ,0,1,1,50,30,3,0
	PuPlayer.LabelNew pDMD,"FLine2",	             dmdalt,16,cPurple   ,0,1,1,48,70,3,0


'page 4
	PuPlayer.LabelNew pDMD,"HSLine1",	             dmdalt,16,cPurple   ,0,1,1,50,30,4,0
	PuPlayer.LabelNew pDMD,"HSLine2",	             dmdalt,16,cPurple   ,0,1,1,50,70,4,0

'page 5
	PuPlayer.LabelNew pDMD,"Health",	             dmddef,18,cPurple   ,0,1,1,12.5,62,5,0



End If





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
'


Sub pDMDStartGame
'pInAttract=false
'pDMDSetPage(pScores)   'set blank text overlay page.

end Sub


Sub pDMDStartBall
end Sub

Sub pDMDGameOver
pAttractStart
end Sub

Sub pAttractStart
    bAttractMode = True
    StartLightSeq
    ShowTableInfo   '107
    StartRainbow aLights



'pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pCurAttractPos=0
pInAttract=True          'Startup in AttractMode
pAttractNext
end Sub

Sub pDMDStartUP
 pupDMDDisplay "attract","Welcome","@welcome.mp4",2,0,10
 pInAttract=true
end Sub

DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
	if pInAttract=false Then exit SUB
	pCurAttractPos=pCurAttractPos+1

	Select Case pCurAttractPos

		Case 1  
			pupDMDDisplay "attract","Welcome to^Thundercats", "bgdefault.mp4",3,0,10             
		Case 2 
			pupDMDDisplay "attract", "Thundercats", "bgdefault.mp4", 3, 0, 10
		Case 3
			pupDMDDisplay "attract","REPLAY AT^7,500,000","",3,1,10      

		Case 4
			pupDMDDisplay "attract","HIGHSCORES", "",2,0,10
		Case 5
			pupDMDDisplay "highscore","High Scores^1> " & HighScoreName(0) & "  " & HighScore(0)&"^2> " & HighScoreName(1) & "  " & HighScore(1) , "", 3, 0, 10  
		Case 6
			pupDMDDisplay "highscore","High Scores^3> " & HighScoreName(2) & "  " & HighScore(2)&"^4> " & HighScoreName(3) & "  " & HighScore(3) , "", 3, 0, 10 

		Case 7,8
			pupDMDDisplay "attract","Thundercats Ho!", "",2,0,10

		Case 9 
			If score(currentplayer) > 0 Then    
				pupDMDDisplay "GAMEOVER", "GAME OVER^Last Score "&(score(currentplayer)), "", 3, 1, 10
				pupDMDDisplay "GAMEOVER", "GAME OVER^Last Score "&(score(currentplayer)), "", 3, 1, 10
			Else
				pupDMDDisplay "GAMEOVER", "GAME OVER", "bgdefault.mp4", 3, 1, 10
			end if      
		Case 10	,11
			pupDMDDisplay "attract","CREDITS","Backglass.mp4",10,0,10
		Case 12 
			if Credits = 0 then    
				pupDMDDisplay "attract", "CREDITS 0^INSERT COIN", "", 3, 0, 10
			Else    
				pupDMDDisplay "attract", "CREDITS "&(Credits)&"^PRESS START", "", 3, 0, 10
			End If
		Case 13
			pupDMDDisplay "attract","VPX TABLE^BY MERLINRTP","",3,0,10   
		Case Else
			pCurAttractPos=0
			pAttractNext 'reset to beginning
	end Select
	'note if you want flipper keys to advance PriorityReset=1 will do it.
end Sub


'************************ called during gameplay to update Scores ***************************
Dim CurTestScore

Sub pUpdateScores  'call this ONLY on timer 300ms is good 
	CurTestScore=Score(currentplayer)
	 'Debug "in pUpdateScores"
	 'Debug "pDMDCurPage "&pDMDCurPage
	if pDMDCurPage <> pScores then Exit Sub
	 Debug "PASSED"
	'puPlayer.LabelSet pDMD,"Credits","CREDITS " & ""& Credits ,1,""
	'puPlayer.LabelSet pDMD,"Play1","Player 1",1,""
	'puPlayer.LabelSet pDMD,"Ball"," "&pDMDCurPriority ,1,""

	puPlayer.LabelSet pDMD,"CurrScore","" & FormatNumber(CurTestScore,0),1,""
	puPlayer.LabelSet pDMD,"Play1","play " & 2,1,""
	puPlayer.LabelSet pDMD,"Ball","ball "  & 2,1,""
end Sub


'PUPInit  'this should be called in table1_init at bottom after all else b2s/controller running.


'********************  pretty much only use pupDMDDisplay all over ************************   
' Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
'Samples
'pupDMDDisplay "shoot", "SHOOT AGAIN!", ", 3, 1, 10 
'pupDMDDisplay "default", "DATA GADGET LIT", "@DataGadgetLit.mp4", 3, 1, 10
'pupDMDDisplay "shoot", "SHOOT AGAIN!", "@shootagain.mp4", 3, 1, 10   
'pupDMDDisplay "balllock", "Ball^Locked|16744448", "", 5, 1, 10             '  5 seconds,  1=flash, 10=priority, ball is first line, locked on second and locked has custom color |
'pupDMDDisplay "balllock","Ball 2^is^Locked", "balllocked2.mp4",3, 1,10     '  3 seconds,  1=flash, play balllocked2.mp4 from dmdsplash folder, 
'pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,10       '  3 seconds,  1=flash, play @balllocked.mp4 from dmdsplash folder, because @ text by default is hidden unless useDmDvideos is disabled.


'pupDMDDisplay "shownum", "3^More To|616744448^GOOOO", "", 5, 1, 10         ' "shownum" is special.  layout is line1=BIG NUMBER and line2,line3 are side two lines.  "4^Ramps^Left"

'pupDMDDisplay "target", "POTTER^110120", "blank.mp4", 10, 0, 10            ' 'target'...  first string is line,  second is 0=off,1=already on, 2=flash on for each character in line (count must match)

'pupDMDDisplay "highscore", "High Score^AAA   2451654^BBB   2342342", "", 5, 0, 10            ' highscore is special  line1=text title like highscore, line2, line3 are fixed fonts to show AAA 123,123,123
'pupDMDDisplay "highscore", "High Score^AAA   2451654|616744448^BBB   2342342", "", 5, 0, 10  ' sames as above but notice how we use a custom color for text |

'============================================================================================================
'Nail Busters Trigger Script

Dim pReset(9) 
Dim pStatement(9)           'holds future scripts
Dim FX
                                                                       
    
for fx=0 to 9
    pReset(FX)=0
    pStatement(FX)=""
next

DIM pTriggerCounter:pTriggerCounter=pTriggerScript.interval

Sub pTriggerScript_Timer()
  for fx=0 to 9  
       if pReset(fx)>0 Then    
          pReset(fx)=pReset(fx)-pTriggerCounter 
            if pReset(fx)<=0 Then
            pReset(fx)=0
            execute(pStatement(fx))
          end if     
       End if
  next
End Sub


Sub TriggerScript(pTimeMS,pScript) ' This is used to Trigger script after the pTriggerScript Timer on the playfield expires
for fx=0 to 9  
  if pReset(fx)=0 Then
    pReset(fx)=pTimeMS
    pStatement(fx)=pScript
    Exit Sub
  End If 
next
end Sub

'TriggerScript 1800, "PuPlayer.LabelSet pBackglass,""WeaponCountBonus"",Weapons(CurrentPlayer) & ""X  100000"" ,1,""""" 

Sub ExtraBallLitTimer_Timer()
	ExtraBallLitTimer.Enabled = 0
if LaneAwardEB = "l23" Then
l23.state = 0
Elseif LaneAwardEB = "l24" Then
l24.state = 0
Elseif LaneAwardEB = "l25" Then
l25.state = 0
Elseif LaneAwardEB = "l26" Then
l26.state = 0
Elseif LaneAwardEB = "l27" Then
l27.state = 0
End If

End Sub


Sub GameStartTrigger_Hit()
	Debug "Launch Lane Trigger Hit"
	if bOnTheFirstBall = True Then
		Debug " Should fire 501/2 Events"
		PuPEvent(501)
		PuPEvent(502)	
	End If
End Sub

Sub DebugTimer2_Timer()
	TestSub
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
' To Use:
' 1) Define a timer tmrScorbit
' 2) Call DoInit at the end of PupInit or in Table Init if you are nto using pup with the appropriate parameters
'     Replace 389 with your TableID from Scorbit 
'     Replace GRWvz-MP37P from your table on OPDB - eg: https://opdb.org/machines/2103
'		if Scorbit.DoInit(389, "PupOverlays", "1.0.0", "GRWvz-MP37P") then 
'			tmrScorbit.Interval=2000
'			tmrScorbit.UserValue = 0
'			tmrScorbit.Enabled=True 
'		End if 
' 3) Customize helper functions below for different events if you want or make your own 
' 4) Call 
'		DoInit - After Pup/Screen is setup (PuPInit)
'		StartSession - When a game starts (ResetForNewGame)
'		StopSession - When the game is over (Table1_Exit, EndOfGame)
'		SendUpdate - called when Score Changes (AddScore)
'			SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
'			Example:  Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
'		SetGameMode - When different game events happen like starting a mode, MB etc.  (ScorbitBuildGameModes helper function shows you how)
' 5) Drop the binaries sQRCode.exe and sToken.exe in your Pup Root so we can create session tokens and QRCodes.
'	- Drop QRCode Images (QRCodeS.png, QRcodeB.png) in yur pup PuPOverlays if you want to use those 
' 6) Callbacks 
'		Scorbit_Paired   	- Called when machine is successfully paired.  Hide QRCode and play a sound 
'		Scorbit_PlayerClaimed	- Called when player is claimed.  Hide QRCode, play a sound and display name 
'		ScorbitClaimQR		- Call before/after plunge (swPlungerRest_Hit, swPlungerRest_UnHit)
' 7) Other 
'		Set Pair QR Code	- During Attract
'			if (Scorbit.bNeedsPairing) then 
'				PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':32, 'height':64,'xalign':0,'yalign':0,'ypos':5,'xpos':5}"
'				PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\QRcodeS.png",1,"{'mt':2,'width':36, 'height':85,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
'			End if 
'		Set Player Names 	- Wherever it makes sense but I do it here: (pPupdateScores)
'		   if ScorbitActive then 
'			if Scorbit.bSessionActive then
'				PlayerName=Scorbit.GetName(CurrentPlayer+1)
'				if PlayerName="" then PlayerName= "Player " & CurrentPlayer+1 
'			End if 
'		   End if 
'
'
'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' TABLE CUSTOMIZATION START HERE 

Sub Scorbit_Paired()								' Scorbit callback when new machine is paired 
debug4 "Scorbit PAIRED"
	PlaySound "scorbit_login"
	PuPEvent 800
	pbackglasslabelhide "ScorbitQR1"
	pbackglasslabelhide "ScorbitQRIcon1"
End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
debug4 "Scorbit LOGIN"
	PlaySound "scorbit_login"
	PuPEvent 500
	ScorbitClaimQR(False)
End Sub 


Sub ScorbitClaimQR(bShow)	
debug4 "In ScorbitClaimQR: " &bShow					'  Show QRCode on first ball for users to claim this position
	if Scorbit.bSessionActive=False then Exit Sub 
	if ScorbitShowClaimQR=False then Exit Sub
	if Scorbit.bNeedsPairing then exit sub 

	if bShow and balls=1 and bGameInPlay and Scorbit.GetName(CurrentPlayer+1)="" then 
		if ScorbitClaimSmall=0 then ' Desktop Make it Larger
			pBackglassLabelSetSizeImage "ScorbitQR2",26.5,38
			pBackglassLabelSetPos "ScorbitQR2",49.75,72.25
			PuPEvent 802
			pbackglasslabelshow "ScorbitQRIcon2"
			pbackglasslabelshow "ScorbitQR2"
		else 
			Debug4 "Showing Claim QR"
			pBackglassLabelSetSizeImage "ScorbitQR2",26.5,38
			pBackglassLabelSetPos "ScorbitQR2",49.75,72.25
			
			PuPEvent 802
			pbackglasslabelshow "ScorbitQR2"
			pbackglasslabelshow "ScorbitQRIcon2"
		End if 
	Else 
		PuPEvent 500
		'PuPEvent 501
		'PuPEvent 502
		debug4 "Hiding QR claim"
		pbackglasslabelhide "ScorbitQRIcon2"
		pbackglasslabelhide "ScorbitQR2"
	End if 
End Sub 

Sub StopScorbit
	Scorbit.StopSession Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame   ' Stop updateing scores
End Sub

Sub ScorbitBuildGameModes()		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.bSessionActive=False then Exit Sub 
	GameModeStr="NA:"

	if BallsRemaining(CurrentPlayer) <= 0 Then	'no balls left
		GameModeStr="NA{red}:YOU FAILED!!!"
	Else										'game on
		Debug4 "AMCombo Value - " 
		Select Case Mode(CurrentPlayer,0)
			Case 1:
				GameModeStr="NA{yellow}:Mutants Defeated"
			Case 2:
				GameModeStr="NA{blue}:Panthro Trial of Strength"
			Case 3:
				GameModeStr="NA{yellow}:Mutants Defeated"
			Case 4:
				GameModeStr="NA{orange}:Cheetara Trial of Speed"
			Case 5:
				GameModeStr="NA{yellow}:Mutants Defeated"
			Case 6:
				GameModeStr="NA{white}:Thunderkittens Trial of Wits"
			Case 7:
				GameModeStr="NA{yellow}:Mutants Defeated"
			Case 8:
				GameModeStr="NA{green}:Tygra Trial of Mind-Power"
			Case 9:
				GameModeStr="NA{yellow}:NA{yellow}:Mutants Defeated"
			Case 10:
				GameModeStr="NA{pruple}:Mumm-RA Trial of Evel"
		End Select


	End If ' endif balls remaining
	Scorbit.SetGameMode(GameModeStr)

End Sub 






' END ----------

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			debug4 "CREATING LOG"
		case 1:
			debug4 "Uploading LOG"
		case 2:
			debug4 "LOG Complete"
	End Select 
End Sub 
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
' TABLE CUSTOMIZATION END HERE - NO NEED TO EDIT BELOW THIS LINE


dim Scorbit : Set Scorbit = New ScorbitIF
' Workaround - Call get a reference to Member Function
Sub tmrScorbit_Timer()								' Timer to send heartbeat 
	Scorbit.DoTimer(tmrScorbit.UserValue)
	tmrScorbit.UserValue=tmrScorbit.UserValue+1
	if tmrScorbit.UserValue>5 then tmrScorbit.UserValue=0
End Sub 
Function ScorbitIF_Callback()
	Scorbit.Callback()
End Function 
Class ScorbitIF

	Public bSessionActive
	Public bNeedsPairing
	Private bUploadLog
	Private bActive
	Private LOGFILE(10000000)
	Private LogIdx

	Private bProduction

	Private TypeLib
	Private MyMac
	Private Serial
	Private MyUUID
	Private TableVersion

	Private SessionUUID
	Private SessionSeq
	Private SessionTimeStart
	Private bRunAsynch
	Private bWaitResp
	Private GameMode
	Private GameModeOrig		' Non escaped version for log
	Private VenueMachineID
	Private CachedPlayerNames(4)
	Private SaveCurrentPlayer

	Public bEnabled
	Private sToken
	Private machineID
	Private dirQRCode
	Private opdbID
	Private wsh

	Private objXmlHttpMain
	Private objXmlHttpMainAsync
	Private fso
	Private Domain

	Public Sub Class_Initialize()
		bActive="false"
		bSessionActive=False
		bEnabled=False 
	End Sub 

	Property Let UploadLog(bValue)
		bUploadLog = bValue
	End Property

	Sub DoTimer(bInterval)	' 2 second interval
		dim holdScores(4)
		dim i
		if bInterval=0 then 
			SendHeartbeat()
		elseif bRunAsynch And bSessionActive = True then ' Game in play (Updated for TNA to resolve stutter in CoopMode)
			Scorbit.SendUpdate Score(1), Score(2), Score(3), Score(4), Balls, CurrentPlayer, PlayersPlayingGame
		End if 
	End Sub 

	Function GetName(PlayerNum)	' Return Parsed Players name  
		if PlayerNum<1 or PlayerNum>4 then 
			GetName=""
		else 
			GetName=CachedPlayerNames(PlayerNum-1)
		End if 
	End Function 

	Function DoInit(MyMachineID, Directory_PupQRCode, Version, opdb)
		dim Nad
		Dim EndPoint
		Dim resultStr 
		Dim UUIDParts 
		Dim UUIDFile

		bProduction=1
'		bProduction=0
		SaveCurrentPlayer=0
		VenueMachineID=""
		bWaitResp=False 
		bRunAsynch=False 
		DoInit=False 
		opdbID=opdb
		dirQrCode=Directory_PupQRCode
		MachineID=MyMachineID
		TableVersion=version
		bNeedsPairing=False
		if bProduction then 
			domain = "api.scorbit.io"
		else 
			domain = "staging.scorbit.io"
			domain = "scorbit-api-staging.herokuapp.com"
		End if 
		Set fso = CreateObject("Scripting.FileSystemObject")
		dim objLocator:Set objLocator = CreateObject("WbemScripting.SWbemLocator")
		Dim objService:Set objService = objLocator.ConnectServer(".", "root\cimv2")
		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		Set objXmlHttpMainAsync = CreateObject("Microsoft.XMLHTTP")
		objXmlHttpMain.onreadystatechange = GetRef("ScorbitIF_Callback")
		Set wsh = CreateObject("WScript.Shell")

		' Get Mac for Serial Number 
		dim Nads: set Nads = objService.ExecQuery("Select * from Win32_NetworkAdapter where physicaladapter=true")
		for each Nad in Nads
			if not isnull(Nad.MACAddress) then
				if left(Nad.MACAddress, 6)<>"00090F" then ' Skip over forticlient MAC
debug4 "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
					MyMac=replace(Nad.MACAddress, ":", "")
					Exit For 
				End if 
			End if 
		Next
		Serial=eval("&H" & mid(MyMac, 5))
		if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))		' Mac Address Overflow Special Case 
		if MyMachineID<>2108 then 			' GOTG did it wrong but MachineID should be added to serial number also
			Serial=Serial+MyMachineID
		End if 
'		Serial=123456
		debug4 "Serial:" & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			debug4 "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" then 
			MsgBox "SCORBIT - Can get UUID, Disabling."
			Exit Function
		elseif MyUUID="03000200-0400-0500-0006-000700080009" or ScorbitAlternateUUID then
			If fso.FolderExists(UserDirectory) then 
				If fso.FileExists(UserDirectory & "ScorbitUUID.dat") then
					Set UUIDFile = fso.OpenTextFile(UserDirectory & "ScorbitUUID.dat",1)
					MyUUID = UUIDFile.ReadLine()
					UUIDFile.Close
					Set UUIDFile = Nothing
				Else 
					MyUUID=GUID()
					Set UUIDFile=fso.CreateTextFile(UserDirectory & "ScorbitUUID.dat",True)
					UUIDFile.WriteLine MyUUID
					UUIDFile.Close
					Set UUIDFile=Nothing
				End if
			End if 
		End if

		' Clean UUID
		UUIDParts=split(MyUUID, "-")
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))		 ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
		debug4 "MyUUID:" & MyUUID 


		' Authenticate and get our token 
		if getStoken() then 
			bEnabled=True 
'			SendHeartbeat
			DoInit=True
		End if 
	End Function 

	Sub Callback()
		Dim ResponseStr
		Dim i 
		Dim Parts
		Dim Parts2
		Dim Parts3
		if bEnabled=False then Exit Sub 

		if bWaitResp and objXmlHttpMain.readystate=4 then 
'			debug4 "CALLBACK: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				'debug3 "RESPONSE: " & ResponseStr

				' Parse Name 
				If bSessionActive = True Then
					if CachedPlayerNames(SaveCurrentPlayer-1)="" then  ' Player doesnt have a name
						if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
							Parts=Split(ResponseStr,",{")							' split it 
							if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
								if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 
									CachedPlayerNames(SaveCurrentPlayer-1)=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")		' Get my name
									CachedPlayerNames(SaveCurrentPlayer-1)=Replace(CachedPlayerNames(SaveCurrentPlayer-1), """", "")
									Scorbit_PlayerClaimed SaveCurrentPlayer, CachedPlayerNames(SaveCurrentPlayer-1)
	'								debug4 "Player Claim:" & SaveCurrentPlayer & " " & CachedPlayerNames(SaveCurrentPlayer-1)
								End if 
							End if
						End if 
					else												    ' Check for unclaim 
						if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
							Parts=Split(ResponseStr,"[")						' split it 
	'debug4 "Parts:" & Parts(1)
							Parts2=Split(Parts(1),"}")							' split it 
							for i = 0 to Ubound(Parts2)
	'debug4 "Parts2:" & Parts2(i)
								if instr(1, Parts2(i), """player"":null")<>0 Then
									CachedPlayerNames(i)=""
								End if 
							Next 
						End if 
					End if
				End If

				'Check heartbeat
				HandleHeartbeatResp ResponseStr
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 
		Debug4 "Scorbit Start Session" 
		CachedPlayerNames(0)=""
		CachedPlayerNames(1)=""
		CachedPlayerNames(2)=""
		CachedPlayerNames(3)=""
		bRunAsynch=True 
		bActive="true"
		bSessionActive=True
		SessionSeq=0
		SessionUUID=GUID()
		SessionTimeStart=GameTime
		LogIdx=0
		SendUpdate 0, 0, 0, 0, 1, 1, 1
	End Sub

	' Custom method for TNA to work around coop mode stuttering
	Public Sub ForceAsynch(enabled)
		if bEnabled=False then Exit Sub
		if bSessionActive=True then Exit Sub 'Sessions should always control asynch when active
		bRunAsynch=enabled
	End Sub

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile
		if bEnabled=False then Exit Sub 
		bRunAsynch=False 'Asynch might have been forced on in TNA to prevent coop mode stutter
		if bSessionActive=False then Exit Sub 
debug4 "Scorbit Stop Session" 

		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			debug4 "Creating Scorbit Log: Size" & LogIdx
			Scorbit_LOGUpload(0)
			Set objFile = fso.CreateTextFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			For i = 0 to LogIdx-1 
				objFile.Writeline LOGFILE(i)
			Next 
			objFile.Close
			LogIdx=0
			Scorbit_LOGUpload(1)
			pvPostFile "https://" & domain & "/api/session_log/", puplayer.getroot&"\" & cGameName & "\sGameLog.csv", False
			Scorbit_LOGUpload(2)
			on error resume next
			fso.DeleteFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			on error goto 0
		End if 

	End Sub 

	Public Sub SetGameMode(GameModeStr)
		GameModeOrig=GameModeStr
		GameMode=GameModeStr
		GameMode=Replace(GameMode, ":", "%3a")
		GameMode=Replace(GameMode, ";", "%3b")
		GameMode=Replace(GameMode, " ", "%20")
		GameMode=Replace(GameMode, "{", "%7B")
		GameMode=Replace(GameMode, "}", "%7D")
	End sub 

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  CurrentPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if

		if bSessionActive=False then Exit Sub 
		if bEnabled=False then Exit Sub 
		if bWaitResp then exit sub ' Drop message until we get our next response 

'		msgbox "currentplayer: " & CurrentPlayer
		SaveCurrentPlayer=CurrentPlayer
		PostData = "session_uuid=" & SessionUUID & "&session_time=" & GameTime-SessionTimeStart+1 & _
					"&session_sequence=" & SessionSeq & "&active=" & bActive

		SessionSeq=SessionSeq+1
		if NumberPlayers > 0 then 
			for i = 0 to NumberPlayers-1
				PostData = PostData & "&current_p" & i+1 & "_score="
				if i <= NumberPlayers-1 then 
					if i = 0 then PostData = PostData & P1Score
					if i = 1 then PostData = PostData & P2Score
					if i = 2 then PostData = PostData & P3Score
					if i = 3 then PostData = PostData & P4Score
				else 
					PostData = PostData & "-1"
				End if 
			Next 

			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & CurrentPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode
		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		'if resultStr<>"" then debug3 "SendUpdate Resp:" & resultStr    			'rtp12
	End Sub 

' PRIVATE BELOW 
	Private Function LPad(StringToPad, Length, CharacterToPad)
	  Dim x : x = 0
	  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
	  LPad = String(x, CharacterToPad) & StringToPad
	End Function

	Private Function GUID()		
		Dim TypeLib
		Set TypeLib = CreateObject("Scriptlet.TypeLib")
		GUID = Mid(TypeLib.Guid, 2, 36)
	End Function

	Private Function GetJSONValue(JSONStr, key)
		dim i 
		Dim tmpStrs,tmpStrs2
		if Instr(1, JSONStr, key)<>0 then 
			tmpStrs=split(JSONStr,",")
			for i = 0 to ubound(tmpStrs)
				if instr(1, tmpStrs(i), key)<>0 then 
					tmpStrs2=split(tmpStrs(i),":")
					GetJSONValue=tmpStrs2(1)
					exit for
				End if 
			Next 
		End if 
	End Function

	Private Sub SendHeartbeat()
		Dim resultStr
		if bEnabled=False then Exit Sub 
		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		
		'Customized for TNA
		If bRunAsynch = False Then 
			debug4 "Heartbeat Resp:" & resultStr
			HandleHeartbeatResp ResultStr
		End If
	End Sub 

	'TNA custom method
	Private Sub HandleHeartbeatResp(resultStr)
		dim TmpStr
		Dim Command
		Dim rc
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
debug4 "QRFile: " &QRFile
		If VenueMachineID="" then
			If resultStr<>"" And Not InStr(resultStr, """machine_id"":" & machineID)=0 Then 'We Paired
				bNeedsPairing=False
				debug4 "Scorbit: Paired"
				Scorbit_Paired()
			ElseIf resultStr<>"" And Not InStr(resultStr, """unpaired"":true")=0 Then 'We Did not Pair
				debug4 "Scorbit: NOT Paired"
				bNeedsPairing=True
			Else
				' Error (or not a heartbeat); do nothing
			End If

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr
'debug4 "VenueMachineID=" & VenueMachineID			
				'Command = """" & puplayer.getroot&"\" & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				Command = """" & puplayer.getroot & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				rc = wsh.Run(Command, 0, False)
			End if 
		End if
	End Sub

	Private Function getStoken()
		Dim result
		Dim results
'		dim wsh
		Dim tmpUUID:tmpUUID="adc12b19a3504453a7414e722f58736b"
		Dim tmpVendor:tmpVendor="vscorbitron"
		Dim tmpSerial:tmpSerial="999990104"
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
		'Dim sTokenFile:sTokenFile=puplayer.getroot&"\" & cGameName & "\sToken.dat"
		Dim sTokenFile:sTokenFile=puplayer.getroot & cGameName & "\sToken.dat"

		' Set everything up
		tmpUUID=MyUUID
		tmpVendor="vpin"
		tmpSerial=Serial
		
		on error resume next
		fso.DeleteFile(sTokenFile)
		On error goto 0 

		' get sToken and generate QRCode
'		Set wsh = CreateObject("WScript.Shell")
		Dim waitOnReturn: waitOnReturn = True
		Dim windowStyle: windowStyle = 0
		Dim Command 
		Dim rc
		Dim objFileToRead

		'Command = """" & puplayer.getroot&"\" & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
		Command = """" & puplayer.getroot & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
debug4 "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
debug4 "Return:" & rc
		if FileExists(puplayer.getroot&"\" & cGameName & "\sToken.dat") and rc=0 then
			Set objFileToRead = fso.OpenTextFile(puplayer.getroot&"\" & cGameName & "\sToken.dat",1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
debug4 "Got TOKEN:" & sToken
				getStoken=True
			Else 
debug4 "ERROR:" & result
				getStoken=False
			End if 
		else 
debug4 "ERROR No File:" & rc
		End if 

	End Function 

	private Function FileExists(FilePath)
		If fso.FileExists(FilePath) Then
			FileExists=CBool(1)
		Else
			FileExists=CBool(0)
		End If
	End Function

	Private Function GetMsg(URLBase, endpoint)
		GetMsg = GetMsgHdr(URLBase, endpoint, "", "")
	End Function

	Private Function GetMsgHdr(URLBase, endpoint, Hdr1, Hdr1Val)
		Dim Url
		Url = URLBase + endpoint & "?session_active=" & bActive
'debug4 "Url:" & Url  & "  Async=" & bRunAsynch
		objXmlHttpMain.open "GET", Url, bRunAsynch
'		objXmlHttpMain.setRequestHeader "Content-Type", "text/xml"
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		if Hdr1<> "" then objXmlHttpMain.setRequestHeader Hdr1, Hdr1Val

'		on error resume next
			err.clear
			objXmlHttpMain.send ""
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				debug3 "Server error: (" & err.number & ") " & Err.Description
			End if 
			if bRunAsynch=False then 
debug4 "Status: " & objXmlHttpMain.status
				If objXmlHttpMain.status = 200 Then
					GetMsgHdr = objXmlHttpMain.responseText
				Else 
					GetMsgHdr=""
				End if 
			Else 
				bWaitResp=True
				GetMsgHdr=""
			End if 
'		On error goto 0

	End Function

	Private Function PostMsg(URLBase, endpoint, PostData, bAsynch)
		Dim Url

		Url = URLBase + endpoint
'debug4 "PostMSg:" & Url & " " & PostData			'rtp12

		objXmlHttpMain.open "POST",Url, bAsynch
		objXmlHttpMain.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
		objXmlHttpMain.setRequestHeader "Content-Length", Len(PostData)
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		objXmlHttpMain.setRequestHeader "Authorization", "SToken " & sToken
		if bAsynch then bWaitResp=True 

		on error resume next
			objXmlHttpMain.send PostData
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				'debug3 "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
'debug4 "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File:" & Mid(sFileName, InStrRev(sFileName, "\") + 1)
		Dim STR_BOUNDARY:STR_BOUNDARY  = GUID()
		Dim nFile  
		Dim baBuffer()
		Dim sPostData
		Dim Response

		'--- read file
		Set nFile = fso.GetFile(sFileName)
		With nFile.OpenAsTextStream()
			sPostData = .Read(nFile.Size)
			.Close
		End With


		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"


		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
debug4 "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 
'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub DelayQRClaim_Timer()
	if bOnTheFirstBall AND bBallInPlungerLane then ScorbitClaimQR(True)
	DelayQRClaim.Enabled=False
End Sub

'***************************************************************
' ZQUE: VPIN WORKSHOP ADVANCED QUEUING SYSTEM - 1.2.0
'***************************************************************
' WHAT IS IT?
' The VPin Workshop Advanced Queuing System allows table authors
' to put sub routine calls in a queue without creating a bunch
' of timers. There are many use cases for this: queuing sequences
' for light shows and DMD scenes, delaying solenoids until the
' DMD is finished playing all its sequences (such as holding a
' ball in a scoop), managing what actions take priority over
' others (e.g. an extra ball sequence is probably more important
' than a small jackpot), and many more.
'
' This system uses Scripting.Dictionary, a single timer, and the
' GameTime global to keep track of everything in the queue.
' This allows for better stability and a virtually unlimited
' number of items in the queue. It also allows for greater
' versatility, like pre-delays, queue delays, priorities, and
' even modifying items in the queue.
'
' The VPin Workshop Queuing System can replace vpmTimer as a
' proper queue system (each item depends on the previous)
' whereas vpmTimer is a collection of virtual timers that run
' in parallel. It also adds on other advanced functionality.
' However, this queue system does not have ROM support out of
' the box like vpmTimer does.
'
' I recommend reading all the comments before you implement the
' queuing system into your table.
'
' WHAT YOU NEED to use the queuing system:
' 1) Put this VBS file in your scripts folder, or copy / paste
'    the code into your table script (and skip step 2).
' 2) Include this file via Scripting.FileSystemObject, and
'    ExecuteGlobal it.
' 3) Make one or more queues by constructing the vpwQueueManager:
'    Dim queue : Set queue = New vpwQueueManager
' 4) Create (or use) a timer that is always enabled and
'    preferably has an interval of 1 millisecond. Use a
'    higher number for less time precision but less resource
'    use. You only need one timer even if you
'    have multiple queues.
' 5) For each queue you created, call its Tick routine in
'    the timer's *_timer() routine:
'    queue.Tick
' 6) You're done! Refer to the routines in vpwQueueManager to
'    learn how to use the queuing system.
'
' TUTORIAL: https://youtu.be/kpPYgOiUlxQ
'***************************************************************

'===========================================
' vpwQueueManager
' This class manages a queue of
' vpwQueueItems and executes them.
'===========================================
Class vpwQueueManager
	Public qItems ' A dictionary of vpwQueueItems in the queue (do NOT use native Scripting.Dictionary.Add/Remove; use the vpwQueueManager's Add/Remove methods instead!)
	Public preQItems ' A dictionary of vpwQueueItems pending to be added to qItems
	Public debugOn 'Null = no debug. String = activate debug by using this unique label for the queue. REQUIRES baldgeek's error logs.
	
	'----------------------------------------------------------
	' vpwQueueManager.qCurrentItem
	' This contains a string of the key currently active / at
	' the top of the queue. An empty string means no items are
	' active right now.
	' This is an important property; it should be monitored
	' in another timer or routine whenever you Add a queue item
	' with a -1 (indefinite) preDelay or postDelay. Then, for
	' preDelay, ExecuteCurrentItem should be called to run the
	' queue item. And for postDelay, DoNextItem should be
	' called to move to the next item in the queue.
	'
	' For example, let's say you add a queue item with the
	' key "kickTheBall" and an indefinite preDelay. You want
	' to wait until another timer fires before this queue item
	' executes and kicks the ball out of a scoop. In the other
	' timer, you will monitor qCurrentItem. Once it equals
	' "kickTheBall", call ExecuteCurrentItem, which will run
	' the queue item and presumably kick out the ball.
	'
	' WARNING!: If you do not properly execute one of these
	' callback routines on an indefinite delayed item, then
	' the queue will effectively freeze / stop until you do.
	'---------------------------------------------------------
	Public qCurrentItem
	
	Public preDelayTime ' The GameTime the preDelay for the qCurrentItem was started
	Public postDelayTime ' The GameTime the postDelay for the qCurrentItem was started
	
	Private onQueueEmpty ' A string or object to be called every time the queue empties (use the QueueEmpty property to get/set this)
	Private queueWasEmpty ' Boolean to determine if the queue was already empty when firing DoNextItem
	Private preDelayTransfer ' Number of milliseconds of preDelay to transfer over to the next queue item when doNextItem is called
	
	Private Sub Class_Initialize
		Set qItems = CreateObject("Scripting.Dictionary")
		Set preQItems = CreateObject("Scripting.Dictionary")
		qCurrentItem = ""
		onQueueEmpty = ""
		queueWasEmpty = True
		debugOn = Null
		preDelayTransfer = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Tick
	' This is where all the magic happens! Call this method in
	' your timer's _timer routine to check the queue and
	' execute the necessary methods. We do not iterate over
	' every item in the queue here, which allows for superior
	' performance even if you have hundreds of items in the
	' queue.
	'----------------------------------------------------------
	Public Sub Tick()
		Dim item
		If qItems.Count > 0 Then ' Don't waste precious resources if we have nothing in the queue
			
			' If no items are active, or the currently active item no longer exists, move to the next item in the queue.
			' (This is also a failsafe to ensure the queue continues to work even if an item gets manually deleted from the dictionary).
			If qCurrentItem = "" Or Not qItems.Exists(qCurrentItem) Then
				DoNextItem
			Else ' We are good; do stuff as normal
				Set item = qItems.item(qCurrentItem)
				
				If item.Executed Then
					' If the current item was executed and the post delay passed, go to the next item in the queue
					If item.postDelay >= 0 And GameTime >= (postDelayTime + item.postDelay) Then
						DebugLog qCurrentItem & " - postDelay of " & item.postDelay & " passed."
						DoNextItem
					End If
				Else
					' If the current item expires before it can be executed, go to the next item in the queue
					If item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive) Then
						DebugLog qCurrentItem & " - expired (Time To live). Moving To the Next queue item."
						DoNextItem
					End If
					
					' If the current item was not executed yet and the pre delay passed, then execute it
					If item.preDelay >= 0 And GameTime >= (preDelayTime + item.preDelay) Then
						DebugLog qCurrentItem & " - preDelay of " & item.preDelay & " passed. Executing callback."
						item.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					End If
				End If
			End If
		End If
		
		' Loop through each item in the pre-queue to find any that is ready to be added
		If preQItems.Count > 0 Then
			Dim k, key
			k = preQItems.Keys
			For Each key In k
				Set item = preQItems.Item(key)
				
				' If a queue item was pre-queued and is ready to be considered as actually in the queue, add it
				If GameTime >= (item.queuedOn + item.preQueueDelay) Then
					DebugLog key & " (preQueue) - preQueueDelay of " & item.preQueueDelay & " passed. Item added To the main queue."
					preQItems.Remove key
					Me.Add key, item.Callback, item.priority, 0, item.preDelay, item.postDelay, item.timeToLive, item.executeNow
				End If
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.DoNextItem
	' Goes to the next item in the queue and deletes the
	' currently active one.
	'----------------------------------------------------------
	Public Sub DoNextItem()
		If Not qCurrentItem = "" Then
			If qItems.Exists(qCurrentItem) Then qItems.Remove qCurrentItem ' Remove the current item from the queue if it still exists
			qCurrentItem = ""
		End If
		
		If qItems.Count > 0 Then
			Dim k, key
			Dim nextItem
			Dim nextItemPriority
			Dim item
			nextItemPriority = 0
			nextItem = ""
			
			' Find which item needs to run next based on priority first, queue order second (ignore items with an active preQueueDelay)
			k = qItems.Keys
			For Each key In k
				Set item = qItems.Item(key)
				
				If item.preQueueDelay <= 0 And item.priority > nextItemPriority Then
					nextItem = key
					nextItemPriority = item.priority
				End If
			Next
			
			If qItems.Exists(nextItem) Then
				Set item = qItems.Item(nextItem)
				DebugLog "DoNextItem - checking " & nextItem & " (priority " & item.priority & ")"
				
				' Make sure the item is not expired and not already executed. If it is, remove it and re-call doNextItem
				If (item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive + preDelayTransfer)) Or item.executed = True Then
					DebugLog "DoNextItem - " & nextItem & " expired (Time To live) Or already executed. Removing And going To the Next item."
					qItems.Remove nextItem
					DoNextItem
					Exit Sub
				End If
				
				'Transfer preDelay time when applicable
				If preDelayTransfer > 0 And item.preDelay > -1 Then
					DebugLog "DoNextItem " & nextItem & " - Transferred remaining postDelay of " & preDelayTransfer & " milliseconds from previously overridden queue item To its preDelay And timeToLive"
					qItems.Item(nextItem).preDelay = item.preDelay + preDelayTransfer
					If item.timeToLive > 0 Then qItems.Item(nextItem).timeToLive = item.timeToLive + preDelayTransfer
					preDelayTransfer = 0
				End If
				
				' Set item as current / active, and execute if it has no pre-delay (otherwise Tick will take care of pre-delay)
				qCurrentItem = nextItem
				If item.preDelay = 0 Then
					DebugLog "DoNextItem - " & nextItem & " Now active. It has no preDelay, so executing callback immediately."
					item.Execute
					preDelayTime = 0
					postDelayTime = GameTime
				Else
					DebugLog "DoNextItem - " & nextItem & " Now active. Waiting For a preDelay of " & item.preDelay & " before executing."
					preDelayTime = GameTime
					postDelayTime = 0
				End If
			End If
		ElseIf queueWasEmpty = False Then
			DebugLog "DoNextItem - Queue Is Now Empty; executing queueEmpty callback."
			CallQueueEmpty() ' Call QueueEmpty if this was the last item in the queue
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.ExecuteCurrentItem
	' Helper routine that can be used when the current item is
	' on an indefinite preDelay. Call this when you are ready
	' for that item to execute.
	'----------------------------------------------------------
	Public Sub ExecuteCurrentItem()
		If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
			DebugLog "ExecuteCurrentItem - Executing the callback For " & qCurrentItem & "."
			Dim item
			Set item = qItems.Item(qCurrentItem)
			item.Execute
			preDelayTime = 0
			postDelayTime = GameTime
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Add
	' REQUIRES Class vpwQueueItem
	'
	' Add an item to the queue.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name for this queue item
	' WARNING: Specifying a key that already exists will
	' overwrite the item in the queue. This is by design. Also
	' note the following behaviors:
	' * Tickers / clocks for tracking delay times will NOT be
	' restarted for this item (but the total duration will be
	' updated. For example, if the old preDelay was 3 seconds
	' and 2 seconds elapsed, but Add was called to update
	' preDelay to 5 seconds, then the queue item will now
	' execute in 3 more seconds (new preDelay - time elapsed)).
	' However, timeToLive WILL be restarted.
	' * Items will maintain their same place in the queue.
	' * If key = qCurrentItem (overwriting the currently active
	' item in the queue) and qCurrentItem already executed
	' the callback (but is waiting for a postDelay), then the
	' current queue item's remaining postDelay will be added to
	' the preDelay of the next item, and this item will be
	' added to the bottom of the queue for re-execution.
	' If you do not want it to re-execute, then add an If
	' guard on your call to the Add method checking
	' "If Not vpwQueueManager.qCurrentItem = key".
	'
	' qCallback (object|string) - An object to be called,
	' or string to be executed globally, when this queue item
	' runs. I highly recommend making sub routines for groups
	' of things that should be executed by the queue so that
	' your qCallback string does not get long, and you can
	' easily organize your callbacks. Also, use double
	' double-quotes when the call itself has quotes in it
	' (VBScript escaping).
	' Example: "playsound ""Plunger"""
	'
	' priority (number) - Items in the queue will be executed
	' in order from highest priority to lowest. Items with the
	' same priority will be executed in order according to
	' when they were added to the queue. Use any number
	' greater than 0. My recommendation is to make a plan for
	' your table on how you will prioritize various types of
	' queue items and what priority number each type should
	' have. Also, you should reserve priority 1 (lowest) to
	' items which should wait until everything else in the
	' queue is done (such as ejecting a ball from a scoop).
	'
	' preQueueDelay (number) - The number of
	' milliseconds before the queue actually considers this
	' item as "in the queue" (pretend you started a timer to
	' add this item into the queue after this delay; this
	' logically works in a similar way; the only difference is
	' timeToLive is still considered even when an item is
	' pre-queued.) Set to 0 to add to the queue immediately.
	' NOTE: this should be less than timeToLive.
	'
	' preDelay (number) - The number of milliseconds before
	' the qCallback executes once this item is active (top)
	' in the queue. Set this to 0 to immediately execute the
	' qCallback when this item becomes active.
	' Set this to -1 to have an indefinite delay until
	' vpwQueueManager.ExecuteCurrentItem is called (see the
	' comment for qCurrentItem for more information).
	' NOTE: this should be less than timeToLive. And, if
	' timeToLive runs out before preDelay runs out, the item
	' will be removed and will not execute.
	'
	' postDelay (number) - After the qCallback executes, the
	' number of milliseconds before moving on to the next item
	' in the queue. Set this to -1 to have an indefinite delay
	' until vpwQueueManager.DoNextItem is called (see the
	' comment for qCurrentItem for more information).
	'
	' timeToLive (number) - After this item is added to the
	' queue, the number of milliseconds before this queue item
	' expires / is removed if the qCallback is not executed by
	' then. Set to 0 to never expire. NOTE: If not 0, this
	' should be greater than preDelay + preQueueDelay or the
	' item will expire before the qCallback is executed.
	' Example use case: Maybe a player scored a jackpot, but
	' it would be awkward / irrelevant to play that jackpot
	' sequence if it hasn't played after a few seconds (e.g.
	' other items in the queue took priority).
	'
	' executeNow (boolean) - Specify true if this item
	' should interrupt the queue and run immediately. This
	' will only happen, however, if the currently active item
	' has a priority less than or equal to the item you are
	' adding. Note this does not bypass preQueueDelay nor
	' preDelay if set.
	' Example: If a player scores an extra ball, you might
	' want that to interrupt everything else going on as it
	' is an important milestone.
	'----------------------------------------------------------
	Public Sub Add(key, qCallback, priority, preQueueDelay, preDelay, postDelay, timeToLive, executeNow)
		DebugLog "Adding queue item " & key
		
		'Construct the item class
		Dim newClass
		Set newClass = New vpwQueueItem
		With newClass
			.Callback = qCallback
			.priority = priority
			.preQueueDelay = preQueueDelay
			.preDelay = preDelay
			.postDelay = postDelay
			.timeToLive = timeToLive
			.executeNow = executeNow
		End With
		
		'If we are attempting to overwrite the current queue item which already executed, take the remaining postDelay and add it to the preDelay of the next item. And set us up to immediately go to the next item while re-adding this item to the queue.
		If preQueueDelay <= 0 And qItems.Exists(key) And qCurrentItem = key Then
			If qItems.Item(key).executed = True Then
				DebugLog key & " (Add) - Attempting To overwrite the current queue item which already executed. Immediately re-queuing this item To the bottom of the queue, transferring the remaining postDelay To the Next item, And going To the Next item."
				If qItems.Item(key).postDelay >= 0 Then
					preDelayTransfer = ((postDelayTime + qItems.Item(key).postDelay) - GameTime)
				End If
				
				'Remove current queue item so we can go to the next item, this can be re-queued to the bottom, and the remaining postDelay transferred to the preDelay of the next item
				qItems.Remove qCurrentItem
				qCurrentItem = ""
			End If
		End If
		
		' Determine execution stuff if this item does not have a pre-queue delay
		If preQueueDelay <= 0 Then
			If executeNow = True Then
				' Make sure this item does not immediately execute if the current item has a higher priority
				If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
					Dim item
					Set item = qItems.Item(qCurrentItem)
					If item.priority <= priority Then
						DebugLog key & " (Add) - Execute Now was Set To True And this item's priority (" & priority & ") Is >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). Making it the current active queue item."
						qCurrentItem = key
						If preDelay = 0 And preDelayTransfer = 0 Then
							DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
							newClass.Execute
							preDelayTime = 0
							postDelayTime = GameTime
						Else
							DebugLog key & " (Add) - Waiting For a pre-delay of " & (preDelay + preDelayTransfer) & " before executing the callback."
							preDelayTime = GameTime
							postDelayTime = 0
						End If
					Else
						DebugLog key & " (Add) - Execute Now was Set To True, but this item's priority (" & priority & ") Is Not >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). This item will Not be executed Now And will be added To the queue normally."
					End If
				Else
					DebugLog key & " (Add) - Execute Now was Set To True And no item was active In the queue. Making it the current active queue item."
					qCurrentItem = key
					If preDelay = 0 Then
						DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
						preDelayTransfer = 0 'No preDelay transfer if we are immediately re-executing the same queue item
						newClass.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					Else
						DebugLog key & " (Add) - Waiting For a pre-delay of " & preDelay & " before executing the callback."
						preDelayTime = GameTime
						postDelayTime = 0
					End If
				End If
			End If
			If qItems.Exists(key) Then 'Overwrite existing item in the queue if it exists
				DebugLog key & " (Add) - Already exists In the queue. Updating the item With the new parameters passed In Add."
				Set qItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the queue."
				qItems.Add key, newClass
			End If
			queueWasEmpty = False
		Else
			If preQItems.Exists(key) Then 'Overwrite existing item in the preQueue if it exists
				DebugLog key & " (Add) - Already exists In the preQueue. Updating the item With the new parameters passed In Add."
				Set preQItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the preQueue."
				preQItems.Add key, newClass
			End If
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Remove
	'
	' Removes an item from the queue. It is better to use this
	' than to remove the item from qItems directly as this sub
	' will also call DoNextItem to advance the queue if
	' the item removed was the active item.
	' NOTE: This only removes items from qItems; to remove
	' an item from preQItems, use the standard
	' Scripting.Dictionary Remove method.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name of the queue item to remove.
	'----------------------------------------------------------
	Public Sub Remove(key)
		If qItems.Exists(key) Then
			DebugLog key & " (Remove)"
			qItems.Remove key
			If qCurrentItem = key Or qCurrentItem = "" Then DoNextItem ' Ensure the queue does not get stuck
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.RemoveAll
	'
	' Removes all items from the queue / clears the queue.
	' It is better to call this sub than to remove all items
	' from qItems directly because this sub cleans up the queue
	' to ensure it continues to work properly.
	'
	' PARAMETERS:
	'
	' preQueue (boolean) - Also clear the pre-queue.
	'----------------------------------------------------------
	Public Sub RemoveAll(preQueue)
		DebugLog "Queue was emptied via RemoveAll."
		
		' Loop through each item in the queue and remove it
		Dim k, key
		k = qItems.Keys
		For Each key In k
			qItems.Remove key
		Next
		qCurrentItem = ""
		
		If queueWasEmpty = False Then CallQueueEmpty() ' Queue is now empty, so call our callback if applicable
		
		If preQueue Then
			k = preQItems.Keys
			For Each key In k
				preQItems.Remove key
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' Get vpwQueueManager.QueueEmpty
	' Get the current callback for when the queue is empty.
	'----------------------------------------------------------
	Public Property Get QueueEmpty()
		If IsObject(onQueueEmpty) Then
			Set QueueEmpty = onQueueEmpty
		Else
			QueueEmpty = onQueueEmpty
		End If
	End Property
	
	'----------------------------------------------------------
	' Let vpwQueueManager.QueueEmpty
	' Set the callback to call every time the queue empties.
	' This could be useful for setting a sub routine to be
	' called each time the queue empties for doing things such
	' as ejecting balls from scoops. Unlike using the Add
	' method, this callback is immune from getting removed by
	' higher priority items in the queue and will be called
	' every time the queue is emptied, not just once.
	'
	' PARAMETERS:
	'
	' callback (object|string) - The callback to call every
	' time the queue empties.
	'----------------------------------------------------------
	Public Property Let QueueEmpty(callback)
		If IsObject(callback) Then
			Set onQueueEmpty = callback
		ElseIf VarType(callback) = vbString Then
			onQueueEmpty = callback
		End If
	End Property
	
	'----------------------------------------------------------
	' Get vpwQueueManager.CallQueueEmpty
	' Private method that actually calls the QueueEmpty
	' callback.
	'----------------------------------------------------------
	Private Sub CallQueueEmpty()
		If queueWasEmpty = True Then Exit Sub
		queueWasEmpty = True
		
		If IsObject(onQueueEmpty) Then
			Call onQueueEmpty(0)
		ElseIf VarType(onQueueEmpty) = vbString Then
			If onQueueEmpty > "" Then ExecuteGlobal onQueueEmpty
		End If
	End Sub
	
	'----------------------------------------------------------
	' DebugLog
	' Log something if debugOn is not null.
	' REQUIRES / uses the WriteToLog sub from Baldgeek's
	' error log library.
	'----------------------------------------------------------
	Private Sub DebugLog(message)
		If Not IsNull(debugOn) Then
			WriteToLog "VPW Queue " & debugOn, message
		End If
	End Sub
End Class

'===========================================
' vpwQueueItem
' Represents a single item for the queue
' system. Do NOT use this class directly.
' Instead, use the vpwQueueManager.Add
' routine.

' You can, however, access an individual
' item in the queue via
' vpwQueueManager.qItems and then modify
' its properties while it is still in the
' queue.
'===========================================
Class vpwQueueItem  ' Do not construct this class directly; use vpwQueueManager.Add instead, and vpwQueueManager.qItems.Item(key) to modify an item's properties.
	Public priority ' The item's set priority
	Public timeToLive ' The item's set timeToLive milliseconds requested
	Public preQueueDelay ' The item's pre-queue milliseconds requested
	Public preDelay ' The item's pre delay milliseconds requested
	Public postDelay ' The item's post delay milliseconds requested
	Public executeNow ' Whether the item was set to Execute immediately
	Private qCallback ' The item's callback object or string (use the Callback property on the class to get/set it)
	
	Public executed ' Whether or not this item's qCallback was executed yet
	Public queuedOn ' The game time this item was added to the queue
	Public executedOn ' The game time this item was executed
	
	Private Sub Class_Initialize
		' Defaults
		priority = 0
		timeToLive = 0
		preQueueDelay = 0
		preDelay = 0
		postDelay = 0
		qCallback = ""
		executeNow = False
		
		queuedOn = GameTime
		executedOn = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueItem.Execute
	' Executes the qCallback on this item if it was not yet
	' already executed.
	'----------------------------------------------------------
	Public Sub Execute()
		If executed Then Exit Sub ' Do not allow an item's qCallback to ever Execute more than one time
		
		'Mark as execute before actually executing callback; that way, if callback recursively adds the item back into the queue, then we can properly handle it.
		executed = True
		executedOn = GameTime
		
		' Execute qCallback
		If IsObject(qCallback) Then
			Call qCallback(0)
		ElseIf VarType(qCallback) = vbString Then
			If qCallback > "" Then ExecuteGlobal qCallback
		End If
	End Sub
	
	Public Property Get Callback()
		If IsObject(qCallback) Then
			Set Callback = qCallback
		Else
			Callback = qCallback
		End If
	End Property
	
	Public Property Let Callback(cb)
		If IsObject(cb) Then
			Set qCallback = cb
		ElseIf VarType(cb) = vbString Then
			qCallback = cb
		End If
	End Property
End Class

'***************************************************************
' END VPIN WORKSHOP ADVANCED QUEUING SYSTEM
'***************************************************************


Sub QueueTimer_Timer()
	BallHandlingQueue.Tick
	EOBQueue.Tick
	DMDQueue.Tick
	HSQueue.Tick
	AudioQueue.Tick
End Sub

' ################################  DOF MAPPINGS #################################################### 
'DOF Config by Arngrim
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Left Slingshot Shake
'105 Right Slingshot
'106 Right Slingshot Shake
'107 Bumper Left
'108 Bumper Center
'109 Bumper Right
'110 Ball Release
'111 AutoPlunger
'112 White Flash on Strobe Toy for autoplunger, Chestout kicker, extra ball, party award
'113 Drain Light
'114 Start Button Light, when credit is bigger than 0
'115 Chestout kicker
'116 ChestDoor Drop or Reset
'117 Strobe 250ms
'118 Drop targets left hit
'119 Drop targets center hit
'120 Drop targets right hit
'121 Knocker for extra ball and award special
'122 Triggertop1 Light
'123 Triggertop2 Light
'124 Triggertop3 Light
'125 Strobe 500ms
'126 Beacon when mutliball start
'127 small shaker effect when magnets are ON
'128 beacon effect when ball is on chest hole
'129 Left Bumper Flash
'130 Center Bumper Flash
'131 Right Bumper Flash
'132 Outlane Left Light
'133 Inlanes Left Light
'134 Inlane Riht Light
'135 Outlane Right Light
'136 Chest flasher 2000 ms
'137 Chest flasher 1000 ms
'138 Bumper Left Blink Flasher
'139 Bumper Center Blink Flasher
'140 Bumper Right Blink Flasher
'141 RGB GI White
'142 RGB GI Purple
'143 RGB GI Darkblue
'144 RGB GI Blue
'145 RGB GI Green
'146 RGB GI Darkgreen
'147 RGB GI Yellow
'148 RGB GI Amber
'149 RGB GI Orange
'150 RGB GI Red
'151 Flasher Center Purple Blink
'152 Flasher Left Purple Blink
'153 Flasher Right Purple Blink
'154 Rear Flashers Blink 1000ms
'155 Rear Flashers Blink 500ms
'#####################################################################################################

Sub LoadBG
	Select Case Mode(0)
		Case 2
			PupEvent 810
		Case 4
			PuPEvent 811
		Case 6
			PuPEvent 812
		Case 8
			PuPEvent 813
		Case 10
			PuPEvent 814
		Case Else		
			PuPEvent 815
	End Select
End Sub

Sub pDMDTimer_Timer()
	if pDMDCurPage <> 1 then Exit Sub
	PuPlayer.LabelSet pDMD,"CurrScore",FormatNumber(Score(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pDMD,"Ball","Ball " & Balls,1,"{'mt':2,'color':" & ModeColor & "}"

	PuPlayer.LabelSet pDMD,"Player","Player " &CurrentPlayer,1,"{'mt':2,'color':" & ModeColor & "}"
	PuPlayer.LabelSet pDMD,"Level","Level " &Level(CurrentPlayer),1,"{'mt':2,'color':" & ModeColor & "}"
	
	Select Case Mode(0)
		Case 1,3,5,7
			PuPlayer.LabelSet pDMD,"Mutants","Mutants " &MonsterHits &"/" & MonsterTotal,1,"{'mt':2,'color':" & ModeColor & "}"
		Case Else
			PuPlayer.LabelSet pDMD,"Mutants","",0,""
	End Select
End Sub

Sub BattleTimer_Timer()
	'Debug "LS: " & LordStrength &":" & pDMDCurPage
	if pDMDCurPage <> 5 then Exit Sub
	'pDMDSetPage(5)
	PuPlayer.LabelSet pDMD,"Health",LordStrength,1,""

End Sub