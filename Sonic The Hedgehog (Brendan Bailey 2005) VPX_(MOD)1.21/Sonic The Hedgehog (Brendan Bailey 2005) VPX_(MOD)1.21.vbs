' *********************************************************************
' **                                                                 **
' **                        FUTURE PINBALL                           **
' **                                                                 **
' **                   Sonic The Hedgehog 2 v1.5                     **
' **                                                                 **
' **         2005 Brendan Bailey - AKA Pinwizkid                     **
' **                                                                 **
' ** This script was created using the generic table script as a     **
' ** platform.  Cross your fingers that it works! ^_~                **
' **                                                                 **
' *********************************************************************
' ** V1.10        Added UDMD Implementation in 2019 by:              **
' **                     Derek Crosby                                **
' *********************************************************************
' ** V1.20        Fix Sound Timing & some bugs in 2023 by:           **
' **                     Panasony                                    **
' *********************************************************************

Option Explicit				' Force explicit variable declaration
Randomize

Dim UltraDMD

Const Ballsize = 50
Const BallMass = 1.1

Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3

'Animations Defined Here :
Dim CoinSpin
Dim SonicLaunch
Dim SonicFall
Dim Rings
Dim DualRings
Dim turnonultradmd

' *********************************************************************
'//////////// This is to turn on/off Ultra DMD (0 = Off, 1 = On) \\\\\\\\\
' *********************************************************************
turnonultradmd = 1

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles


Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
	LoadUltraDMD
	SonicTable_InitSleepWhileRendering()
    On Error Goto 0
End Sub


'FPVPX 1.02.vbs
Const BulbOn = "1"  'Luz encendida
Const BulbOff = "0" 'Luz Apagada
Const BulbBlink = "2" 'Luz intermitente

Dim fpTilted:If IsEmpty(Eval("Tilted"))=true Then fpTilted=false Else fpTilted=Tilted 'Falta
Dim nvCredits:If IsEmpty(Eval("Credits"))=true Then nvCredits=0 Else nvCredits=Credits 'Creditos
Dim nvTotalGamesPlayed:If IsEmpty(Eval("TotalGamesPlayed"))=true Then nvTotalGamesPlayed=0 Else nvTotalGamesPlayed=TotalGamesPlayed 'Total de partidas Jugadas
Dim nvScore:If IsEmpty(Eval("Score"))=true Then nvScore=Array(0,0,0,0) Else nvScore=Score 'Marcador
Dim nvJackpot:If IsEmpty(Eval("Jackpot"))=true Then nvJackpot=0 Else nvJackpot=Jackpot 'Jackpot


Const cGameName = "Erizo"

' Define any Constants
Const TableName = "Sonic"
Const myVersion = "1.01"


Dim bAttractMode
Dim ShowDT

if Table1.ShowDT Then
   D1.visible =1
   D2.visible = 1
 Else
   D1.visible = 0
   D2.visible = 0
End If


Dim b2sstep
b2sstep = 0
Dim b2satm
Dim B2SOn		'True/False if want backglass

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

'UDMD Initialization
Sub LoadUltraDMD 

			If turnonultradmd = 0 then exit sub
			Set UltraDMD = CreateObject("UltraDMD.DMDObject")
			If UltraDMD is Nothing Then
				MsgBox "No UltraDMD found.  This table will NOT run without it."
				Exit Sub
			End If 

            Set UltraDMD = CreateObject("UltraDMD.DMDObject")
            UltraDMD.Init

            ' A Major version change indicates the version is no longer backward compatible
			If turnonultradmd = 0 then exit sub
            If Not UltraDMD.GetMajorVersion = 1 Then
                MsgBox "Incompatible Version of UltraDMD found."
                Exit Sub
            End If

            'A Minor version change indicates new features that are all backward compatible
            If UltraDMD.GetMinorVersion < 0 Then
                MsgBox "Incompatible Version of UltraDMD found.  Please update to version 1.0 or newer."
                Exit Sub
            End If

			Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
			Dim curDir:curDir = fso.GetAbsolutePathName(".")

			Dim DirName
			DirName = curDir& "\" &TableName& ".UltraDMD"

			If Not fso.FolderExists(DirName) Then _
					Msgbox "UltraDMD userfiles directory '" & DirName & "' does not exist." & CHR(13) & "No graphic images will be displayed on the DMD"
			UltraDMD.SetProjectFolder DirName

			' wait for the animation to end
			While UltraDMD.IsRendering = True
			WEnd

            UltraDMD.SetVideoStretchMode UltraDMD_VideoMode_Middle            

            Dim imgList

			imgList = "SoniSpinBG_01.png,SoniSpinBG_02.png,SoniSpinBG_03.png,SoniSpinBG_04.png,SoniSpinBG_05.png,SoniSpinBG_06.png,SoniSpinBG_07.png,SoniSpinBG_08.png,SoniSpinBG_09.png,black.png"
            SonicLaunch = UltraDMD.CreateAnimationFromImages(8, false, imgList)

			imgList = "SonicFall_01.png,SonicFall_02.png,SonicFall_03.png,SonicFall_04.png,SonicFall_05.png,SonicFall_06.png,SonicFall_07.png,SonicFall_08.png,SonicFall_09.png,SonicFall_10.png,SonicFall_11.png,SonicFall_12.png,SonicFall_13.png,SonicFall_14.png,SonicFall_15.png,black.png,black.png,black.png,black.png,black.png"
            SonicFall = UltraDMD.CreateAnimationFromImages(10, true, imgList)

			imgList = "Ring_01.png,Ring_02.png,Ring_03.png,Ring_04.png,Ring_05.png,Ring_06.png,Ring_07.png"
            Rings = UltraDMD.CreateAnimationFromImages(15, true, imgList)

			imgList = "DualRings_01.png,DualRings_02.png,DualRings_03.png,DualRings_04.png,DualRings_05.png,DualRings_06.png,DualRings_07.png,DualRings_01.png,DualRings_02.png,DualRings_03.png,DualRings_04.png,DualRings_05.png,DualRings_06.png,DualRings_07.png,DualRings_08.png"
            DualRings = UltraDMD.CreateAnimationFromImages(15, true, imgList)

        End Sub

Sub SonicTable_InitSleepWhileRendering 
		
	Do 
		If Not UltraDMD.IsRendering Then 
			Exit Sub 
		End If 
	WScript.Sleep 500 
	Loop Until False
			
End Sub 


' Define any Constants 
Const constMaxPlayers 		= 1 		' Maximum number of players per game (between 1 and 4)
Const constBallSaverTime	= 5000	' Time in which a free ball is given if it lost very quickly
Const BallsPerGame = 3												
Const constMaxMultiplier	= 5		' Defines the maximum bonus multiplier level

' Define Global Variables
'
Dim PlayersPlayingGame		' number of players playing the current game
Dim CurrentPlayer				' current player (1-4) playing the game
Dim BonusPoints(4)			' Bonus Points for the current player
Dim BonusMultiplier(4)		   ' Bonus Multiplier for the current player
Dim BallsRemaining(4)		   ' Balls remaining to play (inclusive) for each player
Dim ExtraBallsAwards(4)		' number of EB's out-standing (for each player)
Dim Kickback
Dim Gate
Dim Song
Dim Ring
Dim Lock
Dim Lit
Dim Locklit
Dim lockedballs
Dim Multiballlit
Dim Jackpotlit
Dim jackpotscore
Dim alive
Dim Tempstate
Dim i

' Define Game Control Variables
Dim LastSwitchHit				' Id of last switch hit
Dim BallsOnPlayfield			' number of balls on playfield (multiball exclusive)
Dim BallsInLock				' number of balls in multi-ball lock

' Define Game Flags
Dim bFreePlay					' Either in Free Play or Handling Credits
Dim bOnTheFirstBall			' First Ball (player one). Used for Adding New Players
Dim bBallInPlungerLane		' is there a ball in the plunger lane
Dim bBallSaverActive			' is the ball saver active 
Dim bMultiBallMode			' multiball mode active ?
Dim bEnteringAHighScore		' player is entering their name into the high score table
Dim bGameInPlay
Dim Tilt
Dim TiltSensitivity
Dim Tilted

Dim highscoreOn
Dim LevelBall
Dim mLevelMagnet


Dim bootT
Dim BootE

Sub Boot_Timer
on error resume next
 bootT = bootT + 1 
 Select Case bootT
        Case 0: 
   D1.text = "********************************"
   DisplayB2SText "********************************"
   Playsound "fx_sys11_bootup"
        Case 1: 
   D1.text = "////////////////////////////////"
   DisplayB2SText "////////////////////////////////"
   Playsound "fx_sys11_bootup"
        Case 2: 
   D1.text = "********************************"
   DisplayB2SText "********************************"
   Playsound "fx_sys11_bootup"
        Case 3:
   bootT = 0
   BootE = False
   D1.text = " "
   DisplayB2SText " "
   boot.enabled = 0
   playSound "sonicstartgame" 
   If turnonultradmd > 0 then 
	UltraDMD.DisplayScene00ExWithId "SonicIntro", FALSE, SonicLaunch, "Sonic", 15, -1, "The Hedgehog", 15, -1, 6, 2000, 1
   end if

End Select
End Sub



Sub Table1_Init()

    LoadEM
    Dim i
    Randomize


   'Animation Ball Level 
    Set mLevelMagnet = New cvpmMagnet
    With mLevelMagnet
        .InitMagnet LevelMagnet, 5
	.GrabCenter = 0
        .CreateEvents "mLevelMagnet"
    End With
    mLevelMagnet.MagnetOn = 1

    Set LevelBall = BallLevel.Createsizedball(7):LevelBall.Image = "BolaAgua":BallLevel.Kick 0, 0


    ' load saved values, highscore, names, jackpot
    Loadhs


    'Reset HighScore
    'Reseths

    HighScoreReward = 0
    SpecialHighScoreReward = 0

    'Boot Pinball
    BootE = True
    Boot.enabled = 1

    ' freeplay or coins
    bFreePlay = False 'we dont want coins

    Ring = 1

    ' initialse any other flags
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bMultiBallMode = False
    bGameInPlay = False

    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False

    'LightOff
    GIoff


    If Credits > 0 Then DOF 136, DOFOn


	bFreePlay = FALSE

	' kill the last switch hit (this this variable is very usefull for game control)
	'set LastSwitchHit = DummyTrigger

    KickbackPulse.enabled = 1

	' initialse any other flags
    alive = false
	bOnTheFirstBall = FALSE
	bEnteringAHighScore	= FALSE
	BallsOnPlayfield = 0
	BallsInLock	= 0
    'StartTable()
   'tablereset()
  ' startattractmode()
   'EndOfGame()
   'playSound "sonicstartgame"

    popup.Isdropped = 1
	If turnonultradmd > 0 then 
		UltraDMD.SetScoreboardBackgroundImage "ScoreBG.png", 15, 12
	end if
     vpmtimer.addtimer 4200, "startattractmode() '"
End Sub





' *********************************************************************
' **                                                                 **
' **                     User Defined Script Events                  **
' **                                                                 **
' *********************************************************************

' Initialise the Table for a new Game

sub startsound()
    score(currentplayer) = score(currentplayer) - score(currentplayer)
    specialscore(currentplayer) = specialscore(currentplayer) - specialscore(currentplayer)
    If alive = false then
    alive = true
    'stopmusic 1
    playsound "warp"', FALSE
    GIflash()
   ' player1reel.text = ""
'	 player2reel.text = ""
'	 player3reel.text = ""
'	 player4reel.text = ""
   D1.text = ""
   D2.text = ""
   DisplayB2SText " "
    end if
end sub

sub startgametimer_Timer()
    startgametimer.enabled=false
    resetfornewgame()
    B2SLightOff
end sub
'
Sub ResetForNewGame()
	Dim	i
    bGameInPLay = True

	'AddDebugText "ResetForNewGame"
    endattractmode()
	' get Future Pinball to zero out the nvScore (and nvSpecialScore) Variables
	' aswell and ensure the camera is looking in the right direction.
	' this also Sets the fpGameInPlay flag
	'BeginGame()
    StopAllSounds    
	song = 1
    ring = 0
   'Lookatplayfield()
   'lookatplayfieldtimer.enabled=false
   'selectsong()
   lightkickback()
   lightgate()
   invi1.state=bulbblink
   kickbacklamp.state=1
   saver2.state=1
   saver1.state=bulbon
   tablereset()	
   AddScore (0)
   AddRing (0)
	' increment the total number of games played
   nvTotalGamesPlayed = nvTotalGamesPlayed + 1

	' Start with player 1
	CurrentPlayer = 1

	' Single player (for now, more can be added in later)
	PlayersPlayingGame = 1

	' We are on the First Ball (for Player One)
	bOnTheFirstBall = TRUE

	' initialise all the variables which are used for the duration of the game
	' (do all players incase any new ones start a game)
	For i = 1 To constMaxPlayers
		' Bonus Points for the current player
		BonusPoints(i)	= 0
		' Initial Bonus Multiplier
		
		' Balls Per Game
		BallsRemaining(i) = BallsPerGame 
		' Number of EB's out-standing
		ExtraBallsAwards(i) = 0
	Next

	' initialise any other flags
	bMultiBallMode = FALSE




	' you may wish to start some music, play a sound, do whatever at this point

	' set up the start delay to handle any Start of Game Attract Sequence

	FirstBallDelayTimer.Interval = 500
	FirstBallDelayTimer.Enabled = TRUE
End Sub


' This Timer is used to delay the start of a game to allow any attract sequence to 
' complete.  When it expires it creates a ball for the player to start playing with
'
Sub FirstBallDelayTimer_Timer()
	' stop the timer
	FirstBallDelayTimer.Enabled = FALSE

	' reset the table for a new ball
	ResetForNewPlayerBall()
	' create a new ball in the shooters lane
	CreateNewBall()
End Sub


' (Re-)Initialise the Table for a new ball (either a new ball after the player has 
' lost one or we have moved onto the next player (if multiple are playing))
'
Sub ResetForNewPlayerBall()
	If locklit = false then
	song = 1
	selectsong()
	end if	

	' set the current players bonus multiplier back down to 1X
	SetBonusMultiplier(1)

	' reset any drop targets, lights, game modes etc..
	ShootAgainLight.State = 0
End Sub


' Create a new ball on the Playfield
'
Sub CreateNewBall()
	' create a ball in the plunger lane kicker.
	PlungerKicker.CreateBall
    addscore(0)
	' There is a (or another) ball on the playfield
	BallsOnPlayfield = BallsOnPlayfield + 1
    ballinplunger = true
    ballindrain = false
	Tilted = False
	Tilt = 0

	' kick it out..
	PlungerKicker.kick 90, 8
    'Playsound "BallRelease"
	PlaySoundAt "BallRelease", PlungerKicker
    PlaySound SoundFXDOF("BallRelease",122,DOFPulse,DOFContactors), 0, 1, 0.1, 0.1
	'UltraDMD.DisplayScene00ExWithId "NewBall", FALSE, "black.png", "New Ball", 15, -1, "", -1, -1, 14, 500, 14
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Anim", FALSE, SonicLaunch, "" , 15, -1, "" , -1, -1, 14, 1200, 14
	end if

End Sub


' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

dim Bonuson
'
Sub EndOfBall()
	Dim BonusDelayTime
   bonuson = true
   Ballindrain = true
   'effectmusic 1, setvolume, 1.0
	'LookAtBackbox()
  ' player4reel.text = ""
   animatebonus()
   endhurryup()
   endinvincible()
   invincibletimer.enabled=false
   hurryuptimer.enabled=false
   'stopmusic 2
	StopAllSounds
    playsound "Sonicballlost"',  FALSE
	'AddDebugText "EndOfBall"
	lightkickback()
	lightgate()

	' the first ball has been lost. From this point on no new players can join in
	bOnTheFirstBall = FALSE

	' only process any of this if the table is not tilted.  (the tilt recovery 
	' mechanism will handle any extra balls or end of game)
	If (fpTilted = FALSE) Then
		Dim AwardPoints

		' add in any bonus points (multipled by the bunus multiplier)
		AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
		
		'AddDebugText "Bonus Points = " & AwardPoints

		' you may wish to do some sort of display effects which the bonus is 
		' being added to the players score

		' add a bit of a delay to allow for the bonus points to be added up
		BonusDelayTime = 5000
	Else
		' no bonus, so move to the next state quickly
		BonusDelayTime = 5000
      
	End If

	' start the end of ball timer which allows you to add a delay at this point
	EndOfBallTimer.Interval = BonusDelayTime
	EndOfBallTimer.Enabled = TRUE
End Sub

sub quickB()
    if extraball = true then
	 'stopmusic 1
      StopAllSounds
    end if
    endofballtimer_Timer()
end sub

sub Endofballtimer_Timer()
    Endofballtimer.enabled=false
	if addbonustimer.enabled=True Then
		addbonustimer.enabled=false 
		addbonustimer_Timer()
	end if

    If extraball = true then
    DMDUpdate.Enabled = 0
    Playsound "continue"', False
   ' player1reel.FlushQueue()
   ' player2reel.FlushQueue()
   ' player1reel.Text = "Shoot"', seblink, 300
   ' player2reel.Text = "Again"', seblink, 300
    D1.Text = "Shoot Again"
    DisplayB2SText "  SHOOT AGAIN   " & "                "
    shootagaintimer.enabled = true
    end if
    If extraball = false then
    endofballX()
    end if
end sub

sub shootagaintimer_Timer()
    endofballX()
    shootagainlight.state= 0
    shootagaintimer.enabled=false
end sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBallX()
	' disable the timer
	'UltraDMD.DisplayScene00ExWithId "", FALSE, SonicFall, "", 15, -1, "", 15, -1, 6, 2000, 1	
   'Lookatplayfield()
   StopAllSounds
   Selectsong()
   ballreset()

	' if were tilted, reset the internal tilted flag (this will also 
	' set fpTiltWarnings back to zero) which is useful if we are changing player LOL
	fpTilted = FALSE

	' has the player won an extra-ball ? (might be multiple outstanding)
	If (ExtraBallsAwards(CurrentPlayer) <> 0) Then

		'AddDebugText "Extra Ball"

		' yep got to give it to them
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

		' if no more EB's then turn off any shoot again light
		If (ExtraBallsAwards(CurrentPlayer) = 0) Then
			ShootAgainLight.State = 0
		End If

		' You may wish to do a bit of a song and dance at this point

		' Create a new ball in the shooters lane
		CreateNewBall()

	Else	' no extra balls
   
   If extraball = false then
	BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
   end if

		' was that the last ball ?
		If (BallsRemaining(CurrentPlayer) <= 0) Then


			'AddDebugText "No More Balls, High Score Entry"

			' Submit the currentplayers score to the High Score system built into Future Pinball
			' if they have gotten a high score then it will ask them for their initials.  If not
			' it will call the FuturePinball_NameEntryComplete right away
			'bEnteringAHighScore = TRUE
			'EnterHighScore(currentplayer)
         CheckHighscore
         StopAllSounds
         Song = 7
         SelectSong()
			' you may wish to play some music at this point

		Else

			' not the last ball (for that player)
			' if multiple players are playing then move onto the next one
			EndOfBallComplete()

		End If
	End If
End Sub


' This function is called when the end of bonus display
' (or high score entry finished) and it either end the game or
' move onto the next player (or the next ball of the same player)
'

Sub EndOfBallComplete()
  Dim NextPlayer
	'AddDebugText "EndOfBall - Complete"
	' are there multiple players playing this game ?
	If (PlayersPlayingGame > 1) Then
		' then move to the next player
		NextPlayer = CurrentPlayer + 1
		' are we going from the last player back to the first
		' (ie say from player 4 back to player 1)
		If (NextPlayer > PlayersPlayingGame) Then
			NextPlayer = 1
		End If
	Else
		NextPlayer = CurrentPlayer
	End If

	'AddDebugText "Next Player = " & NextPlayer

   ' is it the end of the game ? (all balls been lost for all players)
	If ((BallsRemaining(CurrentPlayer) <= 0) And (BallsRemaining(NextPlayer) <= 0)) Then
		' you may wish to do some sort of Point Match free game award here
		' generally only done when not in free play mode

		' set the machine into game over mode
      EndOfGame()
      DMDUpdate.enabled = 0
      DisplayB2SText " "
		' you may wish to put a Game Over message on the 

	Else
		' set the next player
		CurrentPlayer = NextPlayer

		' make sure the correct display is upto date
		

		' reset the playfield for the new player (or new ball)
		ResetForNewPlayerBall()

		' and create a new ball
        CreateNewBall()
	End If
End Sub



' This frunction is called at the End of the Game, it should reset all
' Drop targets, and eject any 'held' balls, start any attract sequences etc..
Sub EndOfGame()
	'AddDebugText "End Of Game"
	lockflashers.enabled=false
	' let Future Pinball know that the game has finished.  
	' This also clear the fpGameInPlay flag.
	'EndGame()

	' ensure that the flippers are down
    bGameInPLay = False

	' ensure that the flippers are down
    SollFlipper 0
    SolrFlipper 0
    Tablereset()

	Startattractmode()
	' set any lights for the attract mode	
	SetAllLightsForAttractMode()
    GiOff
End Sub


' The tilt recovery timer waits for all the balls to drain before continuing on 
' as per normal
'
Sub TiltRecoveryTimer_Timer()
	' disable the timer
	TiltRecoveryTimer.Enabled	= FALSE
	' if all the balls have been drained then..
	If (BallsOnPlayfield = 0) Then
		' do the normal end of ball thing (this dosn't give a bonus if the table is tilted)
		EndOfBall()
	Else
		' else retry (checks again in another second)
		TiltRecoveryTimer.Interval = 1000
		TiltRecoveryTimer.Enabled = TRUE
	End If
End Sub


' Set any lights for the Attract Mode.
'
Sub SetAllLightsForAttractMode()
	ShootAgainLight.State = 0
End Sub



' *********************************************************************
' **                                                                 **
' **                   Drain / Plunger Functions                     **
' **                                                                 **
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count and test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "", FALSE, SonicFall, "", 15, -1, "", 15, -1, 14, 1800, 14
	end if
	' Destroy the ball
	Drain.DestroyBall
	BallsOnPlayfield = BallsOnPlayfield - 1
	' pretend to knock the ball into the ball storage mech
	PlaySoundAt "Drain", Drain

	' if there is a game in progress and 
	If (bGameInPlay = TRUE) And (Tilted = FALSE) Then

		' is the ball saver active,
		If (bBallSaverActive = TRUE) Then

			' yep, create a new ball in the shooters lane
			CreateNewBall()
         playsound "die2"
			' you may wish to put something on a display or play a sound at this point

		Else

			' cancel any multiball if on last ball (ie. lost all other balls)
			'
			If (BallsOnPlayfield = 1) Then
				' and in a multi-ball??
				If (bMultiBallMode = True) then
					' not in multiball mode any more
					
					' you may wish to change any music over at this point and
					' turn off any multiball specific lights
					endmultiball()
				End If
			End If

			' was that the last ball on the playfield
			If (BallsOnPlayfield = 0) and (bmultiballmode = false) Then
				' handle the end of ball (change player, high score entry etc..)
				EndOfBall()
			End If

         If (Ballsonplayfield = 1) and (lockedballs = 1) and (bmultiballmode = false) then
         Endofball()
         end if
    
         If (ballsonplayfield = 2) and (lockedballs = 2) and (bmultiballmode = false) then
			Endofball()
         end if

		End If
	End If
End Sub


' A ball is pressing down the trigger in the shooters lane

Dim Ballinplunger
Dim Ballindrain
'
Sub PlungerLaneTrigger_Hit()
	addscore(0)
	bBallInPlungerLane = TRUE
	' remember last trigger hit by the ball
	LastSwitchHit = "PlungerLaneTrigger"
End Sub

Sub plungerLanetrigger_unhit()
    playsound "release"', false
	 GIflash()
    Quickflash()
end sub


' A Ball may of rolled into the Plunger Lane Kicker, if so then kick it 
' back out again
'
Sub PlungerKicker_Hit()
	PlungerKicker.kick 90, 10
End Sub


' The Ball has rolled out of the Plunger Lane.  Check to see if a ball saver machanisim
' is needed and if so fire it up.
'
Sub PlungerLaneGateb_Hit()
	' if there is a need for a ball saver, then start off a timer
	' only start if it is currently not running, else it will reset the time period
	If (constBallSaverTime <> 0) And (bBallSaverActive <> TRUE) Then
		' and only if the last trigger hit was the plunger wire. 
		' (ball in the shooters lane)
		If (LastSwitchHit = "PlungerLaneTrigger") Then
			' set our game flag
			bBallSaverActive = TRUE
			' start the timer
			BallSaverTimer.Enabled = FALSE
			BallSaverTimer.Interval = constBallSaverTime
			BallSaverTimer.Enabled = TRUE
			' if you have a ball saver light you might want to turn it on at this 
			' point (or make it flash)
		End If
	End If
End Sub


' The ball saver timer has expired.  Turn it off and reset the game flag
'
Sub BallSaverTimer_Timer()
	' stop the timer from repeating
	BallSaverTimer.Enabled = FALSE
	' clear the flag
	bBallSaverActive = FALSE
	' if you have a ball saver light then turn it off at this point
End Sub



' *********************************************************************
' **                                                                 **
' **                   Supporting Score Functions                    **
' **                                                                 **
' *********************************************************************

' Add points to the score and update the score board

Dim extraballonescored
Dim extraballtwoscored

Dim messageon

Sub AddScore(points)

	If (fpTilted = FALSE) Then
		' add the points to the current players score variable
	  Score(CurrentPlayer) = Score(CurrentPlayer) + points
      If (score(currentplayer) > 750000) and (extraballonescored=false) then
		scoreextraball()
		extraballonescored = true
      end if
      If score(currentplayer) > 1500000 and (extraballtwoscored=false) then
		scoreextraball()
		extraballtwoscored=true
      end if
      If (ballindrain = false) and (messageon = false) then
		D1.text = " "&(score(currentplayer))
		'player2reel.text = ""
		'player3reel.text = ""
		if ballsremaining(currentplayer) = 3 then
			D2.text = "ball 1" 
			DisplayB2SText ((score(currentplayer)) &" " & "              BALL 1 ")
		end if
		if ballsremaining(currentplayer) = 2 then
			D2.text = "ball 2" 
			DisplayB2SText ((score(currentplayer)) &" " & "              BALL 2 ")
		end if
		if ballsremaining(currentplayer) = 1 then
			D2.text = "ball 3"
			DisplayB2SText ((score(currentplayer)) &" " & "              BALL 3 ")
		end if
      end if
	  ' update the score displays
	  ' add the points to the correct display and light the current players display
	  If turnonultradmd > 0 then 
		UltraDMD.DisplayScoreboard currentplayer, 1, score(currentplayer), 2000000, 3000000, 4000000, "Player " & currentplayer, "Credits " & Credits
	  End if
	End if
	' you may wish to check to see if the player has gotten a replay
End Sub


' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
	' Jackpots only generally increment in multiball mode and not tilted
	' but this dosn't have to tbe the case
	If (fpTilted = False) Then

		If (bMultiBallMode = TRUE) Then
			nvJackpot = nvJackpot + points
			' you may wish to limit the jackpot to a upper limit, ie..
			'	If (nvJackpot >= 6000) Then
			'		nvJackpot = 6000
			' 	End if
		End if
	End if
End Sub


' Will increment the Bonus Multiplier to the next level
'
Sub IncrementBonusMultiplier()
	Dim NewBonusLevel
	' if not at the maximum bonus level
	if (BonusMultiplier(CurrentPlayer) < constMaxMultiplier) then
		' then set it the next next one and set the lights
		NewBonusLevel = BonusMultiplier(CurrentPlayer) + 1
		SetBonusMultiplier(NewBonusLevel)
   End if
End Sub


' Set the Bonus Multiplier to the specified level and set any lights accordingly
'
Sub SetBonusMultiplier(Level)
	' Set the multiplier to the specified level
	BonusMultiplier(CurrentPlayer) = Level

	' If the multiplier is 1 then turn off all the bonus lights
	If (BonusMultiplier(CurrentPlayer) = 1) Then
		' insert your own code here
	Else
		' there is a bonus, turn on all the lights upto the current level
		If (BonusMultiplier(CurrentPlayer) >= 2) Then
			' insert your own code here
		End If
		' etc..
	End If
End Sub



Sub AddRing(points)

	SpecialScore(CurrentPlayer) = SpecialScore(CurrentPlayer) + points	
	if (SpecialScore(CurrentPlayer) + points > SpecialScore(CurrentPlayer)) Then
		If turnonultradmd > 0 then 
			UltraDMD.DisplayScene00ExWithId "Rings2", FALSE, DualRings, "Rings Collected", 13, 7, SpecialScore(CurrentPlayer), 13, 7, 14, 1000, 14
		End If
    End If

End sub



' *********************************************************************
' **                                                                 **
' **                     Table Object Script Events                  **
' **                                                                 **
' *********************************************************************

' The Left Slingshot has been Hit, Add Some Points and Flash the Slingshot Lights
'


Dim LStep, RStep

Sub LeftSlingshotRubber_Slingshot()

	If Tilted Then Exit Sub	' Ignore this _Hit() event if the table is tilted.
    PlaySound "LeftSlingShot"
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingshotRubber.TimerEnabled = True
	DOF 140, DOFPulse

	
	' add some points
	AddScore(110)	
    if alive = true then
    PlaySound SoundFXDOF("jump1", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
    flashforms flasher8, 100, 50, 0
    flashforms Flasher8b, 100, 50, 0
    end if

End Sub

Sub LeftSlingshotRubber_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingshotRubber.TimerEnabled = 0
    End Select

    LStep = LStep + 1
End Sub



' The Right Slingshot has been Hit, Add Some Points and Flash the Slingshot Lights
'
Sub RightSlingshotRubber_Slingshot()

	If Tilted Then Exit Sub	' Ignore this _Hit() event if the table is tilted.
    PlaySound "RightSlingShot"
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingshotRubber.TimerEnabled = True
	DOF 140, DOFPulse

	' add some points
	AddScore(110)
   if alive = true then
    PlaySound SoundFXDOF("jump1", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
    flashforms Flasher7, 100, 50, 0
    flashforms Flasher7b, 100, 50, 0
   end if
	
End Sub

Sub RightSlingshotRubber_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShotRubber.TimerEnabled = 0
    End Select

    RStep = RStep + 1
End Sub













dim saver1lit
dim saver2lit
dim saver3lit


' The Left InLane trigger has been Hit
'
Sub LeftInLaneTrigger_Hit()
	' add some points
	addscore(3510)
    addbonus(150)
    AddRing(2)
   If alive = true then
   
	playsoundring
   end if
	' remember last trigger hit by the ball
	LastSwitchHit = "LeftInLaneTrigger"
   if (saver1lit = false) and (gate = false) then
	saver1lit = true
	saver1.state=bulbblink
	saver1timer.enabled = true
   end if
End Sub


' The Right InLane trigger has been Hit
'
Sub RightInLaneTrigger_Hit()
	' add some points
	addscore(3510)
    addbonus(150)
    AddRing(2)
	if alive = true then
	playsoundring
   end if
	' remember last trigger hit by the ball
	LastSwitchHit = "RightInLaneTrigger"
   if (saver2lit = false) and (kickback = false) then
	saver2lit = true
	saver2.state=2
	saver2timer.enabled = true
   end if
End Sub


' The Left OutLane trigger has been Hit
'
Sub LeftOutLaneTrigger_Hit()
	' add some points
   if kickback = true then
	KickbackPulse.enabled = 1
    kickbacktimer.interval = 800
	kickbacktimer.enabled = true
	addscore(1000)
    addbonus(100)
    AddRing(2)
   flashforms flasher8, 100, 50, 0
   flashforms Flasher8b, 100, 50, 0
   flashforms kickbacklamp, 1500, 150, 0
   end if
   if invincibleon = false then
   kickbacktimer.enabled=true
   PlaySoundAt "spring", Ytarget
Else
   	'KickbackPulse.enabled = 0
   playdiesound()
   addscore(8000)
   addbonus(800)
   AddRing(1)
   quickflash()
   end if
	' remember last trigger hit by the ball
   LastSwitchHit = "LeftOutLaneTrigger"
End Sub

sub kickbacktimer_Timer()
	'kickback = false
    'saver2lit = false
    saver2.state=0
    'kickbacktimer.enabled=false


    KickbackPulse.enabled = 0
	kickbacktimer.enabled = false
	kickback = false
	kickbacklamp.state = bulboff

end sub


Sub KickbackPulse_Hit()
   	KickbackPulse.enabled = 0
    DMDUpdate.interval = 2000
    DMDUpdate.Enabled = 1
    KickbackPulse.kick 0, 30
    LaserKickP1.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP1.TransY = 0 '"
    Playsound "bumper_retro"
End sub

' The Right OutLane trigger has been Hit
'
Sub RightOutLaneTrigger_Hit()
	' add some points
   
	' remember last trigger hit by the ball
	LastSwitchHit = "RightOutLaneTrigger"
   If Gate = true then
    addscore(1000)
    addbonus(100)
    AddRing(2)
   if invincibleon = false then
   closegate.enabled=true
   flashforms gatelight, 500, 50, 0
   end if
   end if
   If gate = false then
   playdiesound()
   addscore(8000)
   addbonus(800)
   AddRing(1)
   quickflash()
   end if
End Sub

sub closegate_Timer()
    gate = false
    closegate.enabled=false 
    gateone.RotateToStart
    saver1lit = false
	saver1.state=bulboff
    playsound"bubble"
end sub

Sub Bumper1_Hit()
    stopsound "bumper"
    playsound "bumper"', FALSE
    FlashForMs Bumper1L, 400, 100, Bulbon
    FlashForMs flasher9, 100, 50, 0
    flashforms Flasher9b, 100, 50, 0
    addscore(2000)
End Sub

Sub Bumper2_Hit()
    stopsound "bumper"
    playsound "bumper"', FALSE
    FlashForMs Bumper2L, 400, 100, Bulbon
    FlashForMs flasher3, 100, 50, 0
    flashforms Flasher3b, 100, 50, 0
    addscore(2000)
End Sub

Sub Bumper3_Hit()
    stopsound "bumper"
    playsound "bumper"', FALSE
    FlashForMs Bumper3l, 400, 100, Bulbon
    FlashForMs flasher5, 100, 50, 0
    flashforms Flasher5b, 100, 50, 0
    addscore(2000)
End Sub

Sub PlungerLaneGate1_Hit()
    if alive = true then
    playsoundring
    end if
    addscore(5000)
    addbonus(250)
    if postlit = false then
    postlit = true
    saver3.state = bulbon
    end if
End Sub

dim ballonelocked
dim balltwolocked

Sub Kicker1_Hit()
	 If Ballonelocked = false then
    If locklit = false then
	 Kicker1Timer.Enabled = True
    addscore(2500)
    addbonus(250)
    end if
    If locklit = true then
    PlaySoundAt "spin", Kicker1
    locklight1.state=bulbon
	 droptarget7.Isdropped = 0
	 lockballtimer.enabled=true
    ballonelocked = true
    checklockedballs()
    addscore(5000)
    addbonus(250)
    end if
    if hurryuplit = true then
    scorehurryup()
    endhurryup()
    end if
	end if
End Sub

Sub Kicker1_UnHit
    PlaySoundAt "LeftEject", Kicker1 
End Sub

Sub Kicker1Timer_Timer()
	 kicker1timer.Enabled = False
	 Kicker1.kick 250, 15
    If alive = true then
    PlaySoundAt "spring", Kicker1 
    end if
End Sub

Sub Kicker2_Hit()
    If Balltwolocked = false then
	 If locklit = false then
	 Kicker2Timer.Enabled = True
    addscore(2500)
    addbonus(250)
    end if
    If locklit = true then
    PlaySoundAt "spring", Kicker2
    locklight2.state=bulbon
	lockballtimer.enabled=true
    balltwolocked = true
    checklockedballs()
    addscore(5000)
    addbonus(250)
    end if
    if hurryuplit = true then
    scorehurryup()
    endhurryup()
    end if
    End if
End Sub

Sub Kicker2_UnHit
    PlaySoundAt "LeftEject", Kicker2 
End Sub

Sub Kicker2Timer_Timer()
	 Kicker2Timer.Enabled = False
	 Kicker2.kick 180, 15
    if alive = true then
    PlaySoundAt "spring", Kicker2
    end if
End Sub

sub kicker3_Hit()
	If multiballlit = false then
   playsound "warp"', false
 '  kicker4timer.enabled = true
   GIflash()
   addscore(5000)
	addbonus(500)
   AddRing(5)
	kicker3.kick 120, 10
   end if
   If multiballlit = true then
   startmultiball()
   addscore(10000)
    addbonus(500)
   end if
   If Jackpotlit = true then
   scorejackpot() 
   end if
   if hurryuplit = true then
    scorehurryup()
    endhurryup()
    end if
end sub

Sub Kicker3fake_hit
    PlaySoundAt "fx_BallDrop", Kicker3fake
End Sub

Sub kicker4timer_Timer()
	 'Kicker4.createball()
     kicker3.kick 120, 10
	 kicker4timer.enabled=false
End sub

sub kicker4_Hit()
     kicker4.kick 0, 25, 1.5
     PlaySoundAt "salidadebola", Kicker4
	' kicker4.destroyball
 '   kicker4timer.enabled = true
end sub

Sub SelectSong()

    If Song = 1 then
    PlaySong "mu_end"
    PlaySong "SonicMaintheme"', TRUE
	 End If
	 If Song = 2 then
    PlaySong "mu_end"
    PlaySong "SonicLockLit"', TRUE
	 End If
    If Song = 3 then
    PlaySong "mu_end"
    PlaySong "SonicMultiball"', TRUE
	 End If
	 If Song = 4 then
   ' PlaySong "mu_end"
    PlaySound "SonicInvincible"', TRUE
	 End If
	 If Song = 5 then
   ' PlaySong "mu_end"
    PlaySound "SonicHurryUp"', FALSE
	 End If
	 If Song = 6 then
   ' PlaySong "mu_end"
    PlaySound "SonicBallLost"', FALSE
	 End If
	 If Song = 7 then
   ' PlaySong "mu_end"
    PlaySound "SonicHiScore"', FALSE
	 End If
    If Song = 8 then
   ' PlaySong "mu_end"
    PlaySound "SonicGameover"', FALSE
	 End If
    If Song = 9 then
   ' PlaySong "mu_end"
    PlaySong "SonicJackpot"', TRUE
    End if
    If Song = 10 then
    PlaySong "mu_end"
    PlaySong "Soniclocklit2"', TRUE
    End If
End Sub

Sub StopAllSounds
    'song = 0
    PlaySong "mu_end"
    StopSound SongMS:SongMS = ""
    StopSound "sonicstartgame" 
    Stopsound "SonicMaintheme"
    Stopsound "SonicLockLit"
    Stopsound "SonicMultiball"
    Stopsound "SonicInvincible"
    Stopsound "SonicHurryUp"
    Stopsound "SonicBallLost"
    Stopsound "SonicHiScore"
    Stopsound "SonicGameover"
    Stopsound "SonicJackpot"
    Stopsound "Soniclocklit2"    
	Stopsound "Sonicballlost"
End Sub


Sub playsoundring()

   ' ring = ring + 1
    If Ring = 1 then 
    playsound "ringleft"
    Ring = 2   
    end if
    If Ring = 2 then
    playsound "ringright"
    'ring = ring - ring
    ring = 1
	 end if
End Sub

Sub Lookatplayfieldtimer_Timer()
	 lookatplayfieldtimer.enabled=false
	' Lookatplayfield()
end sub

dim restand1
dim restand2
dim restand3
dim restand4
dim restand5
dim restand6

Sub Droptarget1_hit()
    If restand1 = false then
    targetsAtimer.enabled=true
	 Tlight1.state=bulbon
    flashforms flasher6, 100, 50, bulboff
    flashforms Flasher6b, 100, 50, bulboff
    PlaySoundAt "jumpon", DropTarget1
    addscore(1000)
    addbonus(50)
    end if
    If restand1 = true then
    flashforms flasher6, 100, 50, bulboff
    flashforms Flasher6b, 100, 50, bulboff
    PlaySoundAt "jumpon", DropTarget1
	 restand1timer.enabled=true
    restand1 = false
    addscore(5000)
    addbonus(200)
    end if
End Sub

Sub Droptarget2_hit()
    If restand2 = false then
    targetsAtimer.enabled=true
	 Tlight2.state=bulbon
    flashforms flasher6, 100, 50, bulboff
    flashforms Flasher6b, 100, 50, bulboff
    PlaySoundAt "jumpon", DropTarget2
    addscore(1000)
    addbonus(50)
    end if
    If restand2 = true then
    flashforms flasher6, 100, 50, bulboff
    flashforms Flasher6b, 100, 50, bulboff
    PlaySoundAt "jumpon", DropTarget2
	 restand2timer.enabled=true
    restand2 = false
    addscore(5000)
    addbonus(200)
    end if
End Sub

Sub Droptarget3_hit()
    If restand3 = false then
    targetsAtimer.enabled=true
    Tlight3.state=bulbon
    flashforms flasher6, 100, 50, bulboff
    flashforms Flasher6b, 100, 50, bulboff
    PlaySoundAt "jumpon", DropTarget3
    addscore(1000)
    addbonus(50)
    end if
    If restand3 = true then
    flashforms flasher6, 100, 50, bulboff
    flashforms Flasher6b, 100, 50, bulboff
    PlaySoundAt "jumpon", DropTarget3
	 restand3timer.enabled=true
    restand3 = false
    addscore(5000)
    addbonus(200)
    end if
End Sub

Sub targetsAtimer_Timer()
    ChecktargetsA()
    targetsAtimer.enabled=false
End Sub

Sub ChecktargetsA()
    If (Droptarget1.Isdropped = 1) and (Droptarget2.Isdropped = 1) and (Droptarget3.Isdropped = 1) then
		droptarget1.Isdropped = 0
		droptarget2.Isdropped = 0
		droptarget3.Isdropped = 0
		Tlight1.state=bulboff
		Tlight2.state=bulboff
		Tlight3.state=bulboff
		addscore(5000)
		addbonus(500)
		Lock = true
		Checklock()
    end if
End sub

Sub Checklock()
    If (Lock = False) or (Lit = False) then 
    playsound"checkpoint"
    end if
    If (Lock = true) and (Lit = True) then
    Lightlock()
    end if
    If Locklit = true then
    playsound"checkpoint"
    end if
End sub


Sub Droptarget4_hit()
    If restand4 = false then
    targetsBtimer.enabled=true
    Tlight4.state=bulbon
    flashforms flasher9, 100, 50, bulboff
    flashforms Flasher9b, 100, 50, 0
    PlaySoundAt "jumpon", DropTarget4
    addscore(1000)
    addbonus(50)
    end if
    If restand4 = true then
    flashforms flasher9, 100, 50, bulboff
    flashforms Flasher9b, 100, 50, 0
    PlaySoundAt "jumpon", DropTarget4
	 restand4timer.enabled=true
    restand4 = false
    addscore(5000)
    addbonus(200)
    end if
End Sub

Sub Droptarget5_hit()
    If restand5 = false then
    targetsBtimer.enabled=true
    Tlight5.state=bulbon
    flashforms flasher9, 100, 50, bulboff
    flashforms flasher9b, 100, 50, 0
    PlaySoundAt "jumpon", DropTarget5
    addscore(1000)
    addbonus(50)
    end if
    If restand5 = true then
    flashforms flasher9, 100, 50, bulboff
    flashforms Flasher9b, 100, 50, 0
    PlaySoundAt "jumpon", DropTarget5
	 restand5timer.enabled=true
    restand5 = false
    addscore(5000)
    addbonus(200)
    end if
End Sub

Sub Droptarget6_hit()
    If restand6 = false then
    targetsBtimer.enabled=true
    Tlight6.state=bulbon
    flashforms flasher9, 100, 50, bulboff
    flashforms Flasher9b, 100, 50, 0
    PlaySoundAt "jumpon", DropTarget6
    addscore(1000)
    addbonus(50)
    end if
    If restand6 = true then
    flashforms flasher9, 100, 50, bulboff
    flashforms Flasher9b, 100, 50, 0
    PlaySoundAt "jumpon", DropTarget6
	 restand6timer.enabled=true
    restand6 = false
    addscore(5000)
    addbonus(200)
    end if
End Sub

Sub targetsBtimer_Timer()
    ChecktargetsB()
    targetsBtimer.enabled=false
End Sub

Sub ChecktargetsB()
    If (Droptarget4.Isdropped = 1) and (Droptarget5.Isdropped = 1) and (Droptarget6.Isdropped = 1) then
		droptarget4.Isdropped = 0
		droptarget5.Isdropped = 0
		droptarget6.Isdropped = 0
		Tlight4.state=bulboff
		Tlight5.state=bulboff
		Tlight6.state=bulboff
		Lit = true
		Checklock()
		addscore(5000)
		addbonus(500)
    end if
End sub

Sub Lightlock()
    If (locklit = false) and (bmultiballmode = false) then
		Locklit = true
		message = 1
		showmessage()
		addscore(10000)
		addbonus(250)
		playsound "crash"', false
		song = 2
		if invincibleon = false then
			StopAllSounds
			selectsong()
		end if
		if hurryuplit = true then
			'effectmusic 1, setvolume, 0.0
			StopAllSounds
			selectsong()
		end if
		locklight1.state=bulbblink
		locklight2.state=bulbblink
    end if
end sub

sub spinner1_Spin()
	stopsound "spin"
    playsound "spin"
    flashforms flasher7, 100, 50, 0
    flashforms Flasher7b, 100, 50, 0
    addscore(500)
end sub
 
sub gate4_hit
    droptarget7.Isdropped = 0
end sub

sub droptarget7_hit
    addscore(4000)
    addbonus(400)
    PlaySoundAt "crash", DropTarget7
    flashforms flasher4, 500, 50, bulboff
    flashforms Flasher4b, 100, 50, 0
    if hurryuplit = true then
    scorehurryup()
    endhurryup()
    end if
    if ballonelocked = true then
    resetdrop7.enabled=true
    end if
end sub

sub SongStop_Timer()
    SongStop.enabled=false
	SongStopSub()
end sub

sub SongStopSub()
	StopAllSounds
	if	invincibleon = True then 
		playsound "sonicinvincible"', True
	else
		if hurryuplit = True then
			Playsound "Sonichurryup"' , False
		Else
				SelectSong()
		end if
	end if
end Sub

sub resetdrop7_Timer()
    resetdrop7.enabled=false
    droptarget7.Isdropped = 0
end sub

sub lockballtimer_Timer()
    lockballtimer.enabled=false
	 createnewball()
end sub

sub checklockedballs()
    lockedballs = lockedballs + 1
    GIflash()
    If lockedballs = 2 then
    multiballlit = true
    lockflashers.enabled=true
    locklight3.state=bulbblink
    end if
end sub

sub startmultiball()
    playsound "spin"', false
    message = 2
    showmessage()
	 locklit = false
    lockflashers.enabled=false
    locklight3.state=1
    jackpotlittimer.enabled=true
    jackpotscore=false
    jackpotlight.state = 2
    GIoff()
    bmultiballmode = true
    multiballlit = false
    mainseq.play seqblinking, , 20,100
    StopAllSounds
    song = 3
    selectsong()
    multiballshow()
    lockedballs = lockedballs - lockedballs
    ballonelocked = false
    balltwolocked = false
end sub

sub scorejackpot()
    addscore(250000)
    message = 6
    showmessage()
    StopAllSounds
    jackpotscore = true
    jackpotlight.state=1
    'jackpotlit = false
   ' effectmusic 1, setvolume , 0.1
   ' effectmusic 1, fadevolume, 1.0, 2000
    StopAllSounds
    playsound "fx_knocker"
    playsound "sonicchaosemerald"', False
    song = 9
	SongStop.Interval = 3600 
	SongStop.Enabled = True
end sub

sub jackpotlittimer_Timer
    jackpotlittimer.enabled=false
	jackpotlit = true
end sub

sub endmultiball()
    bMultiBallMode = False
    StopAllSounds
    song = 10
    selectsong()
    restand1=true
	 restand2=true
	 restand3=true
	 restand4=true
	 restand5=true
	 restand6=true
    resetingametable()
    lock = false
    lit = false
end sub

sub resetingametable()
	mballflashers.enabled=false
    droptarget1.Isdropped = 0
    droptarget2.Isdropped = 0
    droptarget3.Isdropped = 0
    Tlight1.state=bulboff
    Tlight2.state=bulboff
    Tlight3.state=bulboff
    droptarget4.Isdropped = 0
    droptarget5.Isdropped = 0
    droptarget6.Isdropped = 0
    Tlight4.state=bulboff
    Tlight5.state=bulboff
    Tlight6.state=bulboff
    locklight1.state=bulboff
    locklight2.state=bulboff
    locklight3.state=bulboff
	mballshow = mballshow - mballshow
	mballshowtwo = mballshowtwo - mballshowtwo
    jackpotlit = false
    jackpotscore = false
    jackpotlight.state=0
end sub

sub multiballshow()
    kicker1timer.interval = 7000
    kicker2timer.interval = 9000
    kicker4timer.interval = 8000

	 mballshowtimer.enabled = true
    mballshowendtimer.enabled=true
    kicker1timer.enabled = true
    kicker2timer.enabled = true
    kicker4timer.enabled = true
end sub

Dim mballshow
Dim mballshowtwo

sub mballshowtimer_Timer()
    mballshow = mballshow + 1
    if mballshow = 1 then 
    mballshowtimer.interval = 500
    flashforms flasher1, 200, 50, 0
    flashforms flasher2, 200, 50, 0
    flashforms flasher9, 200, 50, 0
    flashforms Flasher1b, 200, 50, 0
    flashforms Flasher2b, 200, 50, 0
    flashforms Flasher9b, 200, 50, 0
    end if
    if mballshow = 2 then
    mballshowtimer.interval = 500
    flashforms flasher3, 200, 50, 0
    flashforms flasher4, 200, 50, 0
    flashforms flasher5, 200, 50, 0
    flashforms Flasher3b, 200, 50, 0
    flashforms Flasher4b, 200, 50, 0
    flashforms Flasher5b, 200, 50, 0
    end if
    if mballshow = 3 then
    mballshowtimer.interval = 500
    flashforms flasher6, 200, 50, 0
    flashforms flasher7, 200, 50, 0
    flashforms flasher8, 200, 50, 0
    flashforms Flasher6b, 200, 50, 0
    flashforms Flasher7b, 200, 50, 0
    flashforms Flasher8b, 200, 50, 0
    end if
    if mballshow = 4 then
    mballshowtimer.interval = 500
    flashforms flasher1, 200, 50, 0
    flashforms flasher2, 200, 50, 0
    flashforms flasher9, 200, 50, 0
    flashforms Flasher1b, 200, 50, 0
    flashforms Flasher2b, 200, 50, 0
    flashforms Flasher9b, 200, 50, 0
    end if
    if mballshow = 5 then
    mballshowtimer.interval = 500
    flashforms flasher3, 200, 50, 0
    flashforms flasher4, 200, 50, 0
    flashforms flasher5, 200, 50, 0
    flashforms Flasher3b, 200, 50, 0
    flashforms Flasher4b, 200, 50, 0
    flashforms Flasher5b, 200, 50, 0
    end if
    if mballshow = 6 then
    mballshowtimer.interval = 500
    flashforms flasher6, 200, 50, 0
    flashforms flasher7, 200, 50, 0
    flashforms flasher8, 200, 50, 0
    flashforms Flasher6b, 200, 50, 0
    flashforms Flasher7b, 200, 50, 0
    flashforms Flasher8b, 200, 50, 0
    end if
    if mballshow = 7 then
    mballshowtimer.interval = 500
    flashforms flasher1, 200, 50, 0
    flashforms flasher2, 200, 50, 0
    flashforms flasher9, 200, 50, 0
    flashforms Flasher1b, 200, 50, 0
    flashforms Flasher2b, 200, 50, 0
    flashforms Flasher9b, 200, 50, 0
    end if
    if mballshow = 8 then
    mballshowtimer.interval = 500
    flashforms flasher3, 200, 50, 0
    flashforms flasher4, 200, 50, 0
    flashforms flasher5, 200, 50, 0
    flashforms Flasher3b, 200, 50, 0
    flashforms Flasher4b, 200, 50, 0
    flashforms Flasher5b, 200, 50, 0
    end if
    if mballshow = 9 then
    mballshowtimer.interval = 250
    flashforms flasher1, 100, 50, 0
    flashforms flasher2, 100, 50, 0
    flashforms flasher9, 100, 50, 0
    flashforms Flasher1b, 100, 50, 0
    flashforms Flasher2b, 100, 50, 0
    flashforms Flasher9b, 100, 50, 0
    end if
	 if mballshow = 10 then
    mballshowtimer.interval = 250
    flashforms flasher1, 100, 50, 0
    flashforms flasher4, 100, 50, 0
    flashforms flasher5, 100, 50, 0
    flashforms Flasher1b, 100, 50, 0
    flashforms Flasher4b, 100, 50, 0
    flashforms Flasher5b, 100, 50, 0
    end if
    if mballshow = 11 then
    mballshowtimer.interval = 250
    flashforms flasher6, 100, 50, 0
    flashforms flasher7, 100, 50, 0
    flashforms flasher8, 100, 50, 0
    flashforms Flasher6b, 100, 50, 0
    flashforms Flasher7b, 100, 50, 0
    flashforms Flasher8b, 100, 50, 0
    end if
     if mballshow = 12 then
    mballshowtimer.interval = 250
    flashforms flasher2, 100, 50, 0
    flashforms flasher3, 100, 50, 0
    flashforms flasher9, 100, 50, 0
    flashforms Flasher2b, 100, 50, 0
    flashforms Flasher3b, 100, 50, 0
    flashforms Flasher9b, 100, 50, 0
    end if
	 if mballshow = 13 then
    mballshowtimer.interval = 250
    flashforms flasher1, 100, 50, 0
    flashforms flasher4, 100, 50, 0
    flashforms flasher5, 100, 50, 0
    flashforms Flasher1b, 100, 50, 0
    flashforms Flasher4b, 100, 50, 0
    flashforms Flasher5b, 100, 50, 0
    end if
    if mballshow = 14 then
    mballshowtimer.interval = 250
    flashforms flasher6, 100, 50, 0
    flashforms flasher7, 100, 50, 0
    flashforms flasher8, 100, 50, 0
    flashforms Flasher6b, 100, 50, 0
    flashforms Flasher7b, 100, 50, 0
    flashforms Flasher8b, 100, 50, 0
    mballshow = mballshow - mballshow
    end if
    
end sub

sub mballshowendtimer_Timer()
    mballshowendtimer.enabled=false
    mballshowtimer.enabled=false
    mballflashers.enabled=true
    kicker1timer.interval = 800
    kicker2timer.interval = 800
    kicker4timer.interval = 800
    GIflash()
end sub

sub mballflashers_Timer()
    mballshowtwo = mballshowtwo + 1
    if mballshowtwo = 1 then
    flashforms flasher8, 100, 50, 0
    flashforms Flasher8b, 100, 50, 0
    end if
	 if mballshowtwo = 2 then
    flashforms flasher1, 100, 50, 0
    flashforms Flasher1b, 100, 50, 0
    end if
    if mballshowtwo = 3 then
    flashforms flasher2, 100, 50, 0
    flashforms Flasher2b, 100, 50, 0
    end if
	 if mballshowtwo = 4 then
    flashforms flasher9, 100, 50, 0
    flashforms Flasher9b, 100, 50, 0
    end if
    if mballshowtwo = 5 then
    flashforms flasher3, 100, 50, 0
    flashforms Flasher3b, 100, 50, 0
    end if
    if mballshowtwo = 6 then
    flashforms  flasher5, 100, 50, 0
    flashforms Flasher5b, 100, 50, 0
    end if
    if mballshowtwo = 7 then
    flashforms flasher4, 100, 50, 0
    flashforms Flasher4b, 100, 50, 0
    end if
    if mballshowtwo = 8 then
    flashforms flasher7, 100, 50, 0
    flashforms flasher7b, 100, 50, 0
    end if
    if mballshowtwo = 9 then
    flashforms flasher6, 100, 50, 0
    flashforms Flasher6b, 100, 50, 0
    mballflashers.interval = 25
    end if
    if mballshowtwo = 10 then
    flashforms flasher8, 100, 50, 0
    flashforms Flasher8b, 100, 50, 0
    end if
	 if mballshowtwo = 11 then
    flashforms flasher1, 100, 50, 0
    flashforms Flasher1b, 100, 50, 0
    end if
    if mballshowtwo = 12 then
    flashforms flasher2, 100, 50, 0
    flashforms Flasher2b, 100, 50, 0
    end if
	 if mballshowtwo = 13 then
    flashforms flasher9, 100, 50, 0
    flashforms Flasher9b, 100, 50, 0
    end if
    if mballshowtwo = 14 then
    flashforms flasher3, 100, 50, 0
    flashforms Flasher3b, 100, 50, 0
    end if
    if mballshowtwo = 15 then
    flashforms flasher5, 100, 50, 0
    flashforms Flasher5b, 100, 50, 0
    end if
    if mballshowtwo = 16 then
    flashforms flasher4, 100, 50, 0
    flashforms Flasher4b, 100, 50, 0
    end if
    if mballshowtwo = 17 then
    flashforms flasher7, 100, 50, 0
    flashforms Flasher7b, 100, 50, 0
    end if
    if mballshowtwo = 18 then
    flashforms flasher6, 100, 50, 0
    flashforms Flasher6b, 100, 50, 0
    mballshowtwo = mballshowtwo - mballshowtwo
    mballflashers.interval = 50
    end if
end sub

Sub GIflash()
    GiOn
    flashforms rightslingshotbulb1, 1000, 100, bulbon
    flashforms rightslingshotbulb2, 1000, 100, bulbon
 '   bulb1.flashforms 1000, 100, bulbon
'	 bulb2.flashforms 1000, 100, bulbon'
'	 bulb3.flashforms 1000, 100, bulbon
'	 bulb4.flashforms 1000, 100, bulbon
'	 bulb5.flashforms 1000, 100, bulbon
'	 bulb6.flashforms 1000, 100, bulbon
'	 bulb7.flashforms 1000, 100, bulbon
'	 bulb8.flashforms 1000, 100, bulbon
'	 bulb9.flashforms 1000, 100, bulbon
'	 bulb10.flashforms 1000, 100, bulbon
'	 bulb11.flashforms 1000, 100, bulbon
'	 bulb12.flashforms 1000, 100, bulbon
'	 bulb13.flashforms 1000, 100, bulbon
'	 bulb14.flashforms 1000, 100, bulbon
'	 bulb15.flashforms 1000, 100, bulbon
'	 bulb16.flashforms 1000, 100, bulbon
'	 bulb17.flashforms 1000, 100, bulbon
'	 bulb18.flashforms 1000, 100, bulbon
'	 bulb19.flashforms 1000, 100, bulbon
'	 bulb20.flashforms 1000, 100, bulbon
'	 bulb21.flashforms 1000, 100, bulbon
'	 bulb22.flashforms 1000, 100, bulbon
'	 bulb23.flashforms 1000, 100, bulbon
'	 bulb24.flashforms 1000, 100, bulbon
'	 bulb25.flashforms 1000, 100, bulbon
'	 bulb26.flashforms 1000, 100, bulbon
'	 bulb27.flashforms 1000, 100, bulbon

end sub
    
Sub GIoff2()
    GIoff
    rightslingshotbulb1.state=bulboff
    rightslingshotbulb2.state=bulboff
 '   bulb1.state=bulboff
	' bulb2.state=bulboff
'	 bulb3.state=bulboff
'	 bulb4.state=bulboff
'	 bulb5.state=bulboff
'	 bulb6.state=bulboff
'	 bulb7.state=bulboff
'	 bulb8.state=bulboff
'	 bulb9.state=bulboff
'	 bulb10.state=bulboff
'	 bulb11.state=bulboff
'	 bulb12.state=bulboff
'	 bulb13.state=bulboff
'	 bulb14.state=bulboff
'	 bulb15.state=bulboff
'	 bulb16.state=bulboff
'	 bulb17.state=bulboff
'	 bulb18.state=bulboff
'	 bulb19.state=bulboff
'	 bulb20.state=bulboff
'	 bulb21.state=bulboff
'	 bulb22.state=bulboff
'	 bulb23.state=bulboff
'	 bulb24.state=bulboff
'	 bulb25.state=bulboff
'	 bulb26.state=bulboff
'	 bulb27.state=bulboff
end sub

sub timer1_Timer()
    timer1.enabled=false
    StopAllSounds
    song = 3 
    selectsong()
    multiballshow()
    GIoff
end sub

dim lockflash

sub lockflashers_Timer()
    lockflash = lockflash + 1
    if lockflash = 1 then
    flashforms flasher1, 100, 50, 0
    flashforms Flasher1b, 100, 50, 0
    end if
    if lockflash = 2 then
    flashforms flasher2, 100, 50, 0
    flashforms Flasher2b, 100, 50, 0
    end if
    if lockflash = 3 then
    flashforms flasher9, 100, 50, 0
    flashforms Flasher9b, 100, 50, 0
    lockflash = lockflash - lockflash
    end if
end sub

Dim H
Dim U
Dim R
Dim Rtwo
Dim Y

sub Htarget_hit()
    addscore(1000)
    addbonus(50)
    flashforms Targetlight1, 500, 50, bulbon
    PlaySoundAt "hit", Htarget
    If H = false then
    H = true
    checkHurryUp()
    end if
end sub

sub Utarget_hit()
    addscore(1000)
    addbonus(50)
    flashforms Targetlight2, 500, 50, bulbon
    PlaySoundAt "hit", Utarget
    If U = false then
    U = true
    checkHurryUp()
    end if
end sub

sub Utarget1_hit()
    addscore(1000)
    addbonus(50)
    flashforms Targetlight3, 500, 50, bulbon
    PlaySoundAt "hit", Utarget1
    If R = false then
    R = true
    checkHurryUp()
    end if
end sub

sub R2target_hit()
    addscore(1000)
    addbonus(50)
    flashforms Targetlight4, 500, 50, bulbon
    PlaySoundAt "hit", R2target
    If Rtwo = false then
    Rtwo = true
    checkHurryUp()
    end if
    If saver2lit = true then
    lightkickback()
	 sheildsound.enabled=true
    saver2timer.enabled=false
    end if
end sub

sub Ytarget_hit()
    addscore(1000)
    addbonus(50)
    flashforms Targetlight5, 500, 50, bulbon
    PlaySoundAt "hit", Ytarget
    If Y = false then
    Y = true
    checkHurryUp()
    end if
    If saver1lit = true then
    lightgate()
    sheildsound.enabled=true
	 saver1timer.enabled=false
    end if
end sub

sub checkhurryup()
    If (H = True) and (U = True) and (R = True) and (Rtwo = True) and (Y = True) then
    starthurryup()
    addscore(5000)
    addbonus(500)
    end if
end sub 

dim hurryuplit

sub starthurryup()
    If bmultiballmode = false then
		hurryuplight1.state=bulbblink
		If balltwolocked = false then
			hurryuplight2.state=bulbblink
		end if
		message = 3
		showmessage()
		hurryuplight3.state=bulbblink
		StopAllSounds
		Playsound "Sonichurryup"', False
		mballflashers.enabled=true
		hurryuplit = true
		hurryuptimer.enabled=true
    end if
    If bmultiballmode = true then
		endhurryup()
    end if
end sub

sub hurryuptimer_Timer()
    endhurryup()
end sub

sub endhurryup()
    hurryuptimer.enabled=false
    hurryuplit = false
	SongStopSub()
    if bmultiballmode = false then
		mballflashers.enabled=false
    end if
    if bmultiballmode = true then
		mballflashers.enabled=true
    end if
    mballshowtwo = mballshowtwo - mballshowtwo
    H = false
    U = false
    R = false
    Rtwo = false
    Y = false
	hurryuplight1.state=bulboff
    hurryuplight2.state=bulboff
	hurryuplight3.state=bulboff
    Targetlight1.state=bulboff
    Targetlight2.state=bulboff
    Targetlight3.state=bulboff
    Targetlight4.state=bulboff
    Targetlight5.state=bulboff
   ' effectmusic 1, fadevolume, 1.0, 1000
end sub

sub scorehurryup()
   ' stopmusic 2
   ' StopAllSounds
    playsound "bubble"
    addscore(50000)
    addbonus(5000)
end sub

Sub quickflash()
    quickflashtimer.enabled=true
    mballflashers.enabled=true
end sub

sub quickflashtimer_Timer()
    quickflashtimer.enabled=false
    if (bmultiballmode = false) and (hurryuplit = false) then
    mballflashers.enabled=false
    end if
    mballshowtwo = mballshowtwo - mballshowtwo
end sub

sub restand1timer_Timer()
    restand1timer.enabled=false
    droptarget1.Isdropped = 0
    playsound "die2"
end sub

sub restand2timer_Timer()
    restand2timer.enabled=false
    droptarget2.Isdropped = 0
    playsound "die2"
end sub

sub restand3timer_Timer()
    restand3timer.enabled=false
    droptarget3.Isdropped = 0
    playsound "die2"
end sub

sub restand4timer_Timer()
    restand4timer.enabled=false
    droptarget4.Isdropped = 0
    playsound "die2"
end sub

sub restand5timer_Timer()
    restand5timer.enabled=false
    droptarget5.Isdropped = 0
    playsound "die2"
end sub

sub restand6timer_Timer()
    restand6timer.enabled=false
    droptarget6.Isdropped = 0
    playsound "die2"
end sub

sub saver1timer_Timer()
    saver1timer.enabled=false
    saver1lit = false
    saver1.state=bulboff
end sub

sub saver2timer_Timer()
    saver2timer.enabled=false
    saver2lit = false
    saver2.state=0
end sub

sub lightkickback()
	kickbacktimer.enabled = false
	saver2timer.enabled = False
    kickback = true
   	KickbackPulse.enabled = 1
    kickbacklamp.state=1
    saver2.state = 1
    saver2lit = false
end sub

sub lightgate()
	closegate.enabled=false
	saver1timer.enabled = False
    gateone.RotateToEnd
    gate = true
    gatelight.state = bulbon
    saver1.state = bulbon
    saver1lit = false
end sub

sub lightpost()

  ' stay up for 20 seconds...

 '   if invincibleon = false then
    'popup.solenoidpulse(15000)
 '   popup.Isdropped = 0
 '   Popup1 = 1
 '   Popupsolenoidpulse.Enabled = 1
 '   vpmtimer.addtimer 15000, "popup.Isdropped = 1 '"
 '  flashforms postlight, 15000, 150, bulboff
    posttimer.enabled=true
 '   sheildsound.enabled=true
 '   end if
 '   if invincibleon = true then
 '   'popup.solenoidpulse(20000)
    'popup.Isdropped = 0
 '   Popup2 = 1
    'Popupsolenoidpulse.Enabled = 1
    PopupAnimUp 0, 48
    'Popupsolenoidpulse2.Interval = 20000
    'Popupsolenoidpulse2.Enabled = 1
    'vpmtimer.addtimer 20000, "PopupAnimDown 48, 0 '"
    ResetSoundPopop.Interval = 20100
    ResetSoundPopop.Enabled = True
	flashforms postlight, 30000, 150, 2
	flashforms BulbPost, 30000, 150, 2
 '   end if
end sub 

Sub ResetSoundPopop_Timer
    ResetSoundPopop.Enabled = False
   	If locklit = false then
	'song = 1
	'SelectSong()
    Else
    If lockedballs = 1 Then
    'Song = 10
    'SelectSong()
   ' PlaySong "soniclocklit2"
	end if
    If lockedballs = 0 Then
    'Song = 2
    'SelectSong()
   ' PlaySong "soniclocklit"
	end if
	end if
End Sub

 Dim HPos, HPosEnd
 Sub Popupsolenoidpulse_timer() 
    PopupP.Z = HPos
	If Hpos < HposEnd Then
     HPos = HPos + 1
	Else
	 Popupsolenoidpulse.enabled = 0
	End If
end Sub 

 Sub Popupsolenoidpulse2_timer() 
    PopupP.Z = HPos
	If Hpos > HposEnd Then
     HPos = HPos - 1
	Else
	 Popupsolenoidpulse2.enabled = 0
	End If
end Sub 

Sub PopupAnimUp(FrameStart, FrameEnd)
    popup.Isdropped = 0
    HPos = FrameStart         
    HPosEnd = FrameEnd
	Popupsolenoidpulse.enabled = 1
End Sub

Sub PopupAnimDown(FrameStart, FrameEnd)
    popup.Isdropped = 1
    HPos = FrameStart         
    HPosEnd = FrameEnd
	Popupsolenoidpulse2.enabled = 1
End Sub



dim diesound

Sub playdiesound()
    diesound = diesound + 1
    If (alive = true) and (ballindrain = false) then
    If diesound = 1 then
	 playsound "loserings"', False
    end if
    If diesound = 2 then
	 playsound "drown"', False
    end if
    If diesound = 3 then
	 playsound "die1"
    diesound = diesound - diesound
    end if
    end if
end sub

dim postlit

sub gate2b_hit()
    addscore(7000)
    addbonus(350)
    If (postlit = true) and (invincibleon = false) then
    lightpost()
    end if
end sub

sub sheildsound_Timer()
    playsound "sheild"
    sheildsound.enabled=false
end sub
  
sub posttimer_Timer()
    postlit = false
    saver3.state=0
    posttimer.enabled=false
	if invincibleon = false then
		PopupAnimDown 48, 0 
		postlight.state = bulboff
		BulbPost.state = bulboff		
		playdiesound()
	end If
end sub

sub plungerlanegate_hit()
    plungershoottimer.enabled=true
    if extraball =true then
    extraball = false
    end if
end sub

sub plungershoottimer_Timer()
    plungershoottimer.enabled=false
    ballinplunger = false
end sub

dim invincible
dim invincibleon

sub gate1b_hit()
    advanceinvincible()
end sub

sub advanceinvincible()
    if ballinplunger = false then
		stopsound "spin"
		playsound "spin"', false
		addscore(10000)
		addbonus(500)
		quickflash()
		invincible = invincible + 1
        if invincible = 1 then
			invi1.state=1
			invi2.state=2
        end if
        if invincible = 2 then
			invi2.state=1
			invi3.state=2
        end if
        if invincible = 3 then
			invi3.state=1
			startinvincible()
        end if
      end if
end sub

sub startinvincible()
    addscore(5000)
    addbonus(250)
    invincibleon = true
    invincibletimer.enabled = true
    lightkickback()
    lightgate()
    lightpost()
    message = 4
    showmessage()
'    if bmultiballmode = false then
	  StopAllSounds
	  playsound "sonicinvincible"', True
'    end if
    if hurryuplit = true then
    'effectmusic 1, setvolume, 0.0
    end if
end sub

sub invincibletimer_Timer()
    invincibletimer.enabled=false
    endinvincible()
end sub

sub endinvincible()
'    if (bmultiballmode = false) and (ballindrain = false) then
    invincibleon = false
     if (ballindrain = false) then
		SongStopSub()
		playdiesound()
    end if
    Popupsolenoidpulse.Enabled = 1
    popup.Isdropped = 1
    invincible = invincible - invincible
    invi1.state=2
    invi2.state=0
    invi3.state=0
	PopupAnimDown 48, 0
	postlight.state = bulboff
	BulbPost.state = bulboff
	playdiesound()
end sub

sub rubber3_hit()
    playsound"die2"
    addscore(500)
    addbonus(10)
end sub

sub rubber25_hit()
    playsound"die2"
    addscore(500)
    addbonus(10)
end sub

sub trigger1_hit()
    AddRing(2)
    playsound "ringleft"
    addscore(1000)
    addbonus(100)
    if ALight.state = 0 then
    ALight.state=1
    end if
    checklanes()
end sub

sub trigger2_hit()
    AddRing(2)
    playsound "ringleft"
    addscore(1000)
    addbonus(100)
    if BLight.state = 0 then
    BLight.state=1
    end if
    checklanes()
end sub

sub trigger3_hit()
    AddRing(2)
    playsound "ringleft"
    addscore(1000)
    addbonus(100)
    if CLight.state = 0 then
    CLight.state=1
    end if
    checklanes()
end sub

Sub checklanes()
    If (ALight.state=1) and (BLight.state=1) and (CLight.state=1) then
    advancebonusmultiplier()
    addscore(7500)
    addbonus(100)
    end if
end sub

Dim bonusmulti

sub advancebonusmultiplier()
    playsound "checkpoint"
    'stopmusic 4
   ' StopAllSounds
    quickflash()
    ALight.state=0
    BLight.state=0
    CLight.state=0
    Bonusmulti = Bonusmulti + 1
    If Bonusmulti = 1 then
    twoX.state=1
    end if
    If Bonusmulti = 2 then
    threeX.state=1
    end if
    If Bonusmulti = 3 then
    fourX.state=1
    end if
    If Bonusmulti = 4 then
    fiveX.state=1
    end if
    If Bonusmulti = 5 then
    Bonusmulti = Bonusmulti - 1
    end if
end sub

dim bonus

sub addbonus(points)
    bonus = bonus + points
end sub

sub multiplybonus()
    If bonusmulti = 0 then

    end if
    If Bonusmulti = 1 then
       addbonus(bonus)
    end if
    If Bonusmulti = 2 then
       addbonus(bonus)
       addbonus(bonus)
    end if
    If Bonusmulti = 3 then
       addbonus(bonus)
       addbonus(bonus)
       addbonus(bonus)
    end if
    If Bonusmulti = 4 then
	    addbonus(bonus)
       addbonus(bonus)
       addbonus(bonus)
       addbonus(bonus)
    end if
end sub

sub animatebonus()
    DMDUpdate.Enabled = 0
    multiplybonus()
    addbonustimer.enabled=true
    D1.Text = "BONUS"', sescrollright, 500
    DisplayB2SText "     BONUS      " & "                "  
    If Bonusmulti = 0 then
    D1.Text = "BONUS 1X"', sescrollleft, 500, 20
    DisplayB2SText "     BONUS X1   " & "                "'&(Bonus)
    end if
	 If Bonusmulti = 1 then
    D1.Text = "BONUS 2X"', sescrollleft, 500, 20
    DisplayB2SText "     BONUS X2   " & "                "'&(Bonus)
    end if
    If Bonusmulti = 2 then
    D1.Text = "BONUS 3X"', sescrollleft, 500, 20
    DisplayB2SText "     BONUS X3   " & "                "'&(Bonus)
    end if
    If Bonusmulti = 3 then
    D1.Text = "BONUS 4X"', sescrollleft, 500, 20
    DisplayB2SText "     BONUS X4   " & "                "'&(Bonus)
    end if
    If Bonusmulti = 4 then
    D1.Text = "BONUS 5X"', sescrollleft, 500, 20
    DisplayB2SText "     BONUS X5   " & "                "'&(Bonus)
    end if
  
    'player3reel.Text = "="', sescrollright, 500, 40

    D2.Text = (Bonus)', sescrollleft, 500, 60
    DisplayB2SText D1.Text & "          " & "TOTAL " & " "&(Bonus)
end sub

sub addbonustimer_Timer()
    DMDUpdate.Enabled = 0
    addbonustimer.enabled=false
    playsound "bonus"
    bonuson = false
'    bulb50.flashforms 300, 150
'    bulb29.flashforms 300, 150
'    bulb28.flashforms 300, 150
'    bulb37.flashforms 300, 150
    addscore(bonus)
    bonus = bonus - bonus
    D1.Text = "Score ="', sescrollright, 200
    D2.Text = (score(currentplayer))', sescrollleft, 200
    'player3reel.Text = ""', sescrollright, 200
    'player4reel.text = ""
    DisplayB2SText "SCORE>           " & "    "&(score(currentplayer))
end sub

dim extraball
 
sub scoreextraball()
    playsound "fx_knocker"
    message = 5
    showmessage()
   ' effectmusic 1, setvolume, 0.0
   ' effectmusic 2, setvolume, 0.0
   ' effectmusic 4, setvolume, 0.0
    If hurryuplit = false then
   ' effectmusic 1, fadevolume, 1.0, 2000
    end if
   ' effectmusic 2, fadevolume, 1.0, 2000
   ' effectmusic 4, fadevolume, 1.0, 2000

	StopAllSounds()
    playsound "sonicextraball"', False
	SongStop.Interval = 3600 
	SongStop.Enabled = True
    quickflash
	 GIflash
    shootagainlight.state=1
    extraball = true
end sub

sub startattractmode()

    bAttractMode = true
    PFattract = PFattract - PFattract
    BGattract = BGattract - BGattract
    attractmessage = attractmessage - attractmessage
    playfieldattract.enabled=true
    backglassattract.enabled=true
    messagetimer.enabled=true
end sub



dim BGattract

sub backglassattract_Timer()
 If B2SOn Then
    BGattract = BGattract + 1 
    If BGattract = 1 then
    'BG4.flashforms 400, 200
    Startb2s(4)
    'BG9.flashforms 400, 200
    Startb2s(9)
    end if   
    If BGattract = 2 then
    'BG3.flashforms 400, 200
    Startb2s(3)
    'BG5.flashforms 400, 200
    Startb2s(5)
    'BG10.flashforms 400, 200
    Startb2s(10)
    end if
    If BGattract = 3 then
    'BG2.flashforms 400, 200
    Startb2s(2)
	'BG6.flashforms 400, 200
    Startb2s(6)
    'BG12.flashforms 400, 200
    Startb2s(12)
    end if
    If BGattract = 4 then
    'BG1.flashforms 400, 200
    Startb2s(1)
    'BG7.flashforms 400, 200
    Startb2s(7)
    'BG13.flashforms 400, 200
    Startb2s(13)
    end if
    If BGattract = 5 then
    'BG2.flashforms 400, 200 
    Startb2s(2)
    'BG6.flashforms 400, 200
    Startb2s(6)
    'BG11.flashforms 400, 200
    Startb2s(11)
    end if
    If BGattract = 6 then
    BGattract = BGattract - BGattract
    'BG3.flashforms 400, 200
    Startb2s(3)
    'BG5.flashforms 400, 200
    Startb2s(5)
    'BG8.flashforms 400, 200
    Startb2s(8)
    end if
 End If
end sub

dim PFattract

sub playfieldattract_Timer()
    PFattract = PFattract + 1
    If PFattract = 1 then
    flashforms twoX, 200, 100, 0
    flashforms Tlight1, 200, 100, 0
    flashforms Tlight6, 200, 100, 0
    flashforms invi1, 200, 100, 0
    flashforms shootagainlight, 200, 100, 0
    flashforms kickbacklamp, 200, 100, 0
    flashforms saver1, 200, 100, 0
    flashforms hurryuplight3, 200, 100, 0
    flashforms hurryuplight1, 200, 100, 0
    flashforms ALight, 200, 100, 0
    flashforms targetlight3, 200, 100, 0
    End If
    If PFattract = 2 then
    flashforms threeX, 200, 100, 0
    flashforms Tlight2, 200, 100, 0
    flashforms Tlight5, 200, 100, 0
    flashforms invi2, 200, 100, 0
    flashforms gatelight, 200, 100, 0
    flashforms targetlight5, 200, 100, 0
    flashforms locklight3, 200, 100, 0
    flashforms hurryuplight2, 200, 100, 0
    flashforms BLight, 200, 100, 0
    flashforms targetlight2, 200, 100, 0
    flashforms Postlight, 200, 100, 0
    flashforms BulbPost, 200, 100, 0
    End If
    If PFattract = 3 then
    flashforms fourX, 200, 200, 0
    flashforms Tlight3, 200, 100, 0
    flashforms Tlight4, 200, 100, 0
    flashforms invi3, 200, 100, 0
    flashforms shootagainlight, 200, 100, 0
    flashforms kickbacklamp, 200, 100, 0
    flashforms saver2, 200, 100, 0
    flashforms jackpotlight, 200, 100, 0
    flashforms locklight1, 200, 100, 0
    flashforms CLight, 200, 100, 0
    flashforms targetlight1, 200, 100, 0
    End If
    If PFattract = 4 then
    PFattract = PFattract - PFattract
    flashforms fiveX, 200, 200, 0
    flashforms Tlight2, 200, 100, 0
    flashforms Tlight5, 200, 100, 0
    flashforms invi2, 200, 100, 0
	flashforms gatelight, 200, 100, 0
    flashforms targetlight4, 200, 100, 0
    flashforms locklight3, 200, 100, 0
    flashforms locklight2, 200, 100, 0
    flashforms BLight, 200, 100, 0
    flashforms saver3, 200, 100, 0
    flashforms Postlight, 200, 100, 0
    flashforms BulbPost, 200, 100, 0
    End If
end sub    

sub setlights()

    kickbacklamp.state=0
    gatelight.state=bulboff
    shootagainlight.state=0
    Postlight.state=bulboff
    BulbPost.state = bulboff
    twoX.state=bulboff
    threeX.state=bulboff
    fourX.state=bulboff
    fiveX.state=bulboff
	 Tlight1.state=bulboff
    Tlight2.state=bulboff
    Tlight3.state=bulboff
    Tlight4.state=bulboff
    Tlight5.state=bulboff
    Tlight6.state=bulboff
    Invi1.state=bulboff
    Invi2.state=bulboff
    Invi3.state=bulboff
    saver1.state=bulboff
    saver2.state=0
    saver3.state=bulboff
    Targetlight1.state=bulboff
    Targetlight2.state=bulboff
    Targetlight3.state=bulboff
    Targetlight4.state=bulboff
    Targetlight5.state=bulboff
    Locklight1.state=bulboff
    Locklight2.state=bulboff
    Locklight3.state=bulboff
    Hurryuplight1.state=bulboff
    Hurryuplight2.state=bulboff
    Hurryuplight3.state=bulboff
    ALight.state=bulboff
    BLight.state=0
    CLight.state=0
    Jackpotlight.state=0
 If B2SOn Then
    Controller.B2SSetData 1, 1
    Controller.B2SSetData 2, 1
    Controller.B2SSetData 3, 1
    Controller.B2SSetData 4, 1
    Controller.B2SSetData 5, 1
    Controller.B2SSetData 6, 1
    Controller.B2SSetData 7, 1
    Controller.B2SSetData 8, 1
    Controller.B2SSetData 9, 1
    Controller.B2SSetData 10, 1
    Controller.B2SSetData 11, 1
    Controller.B2SSetData 12, 1
    Controller.B2SSetData 13, 1
 End If
'    BG1.state=bulbon
'	 BG2.state=bulbon
'	 BG3.state=bulbon
'	 BG4.state=bulbon
'	 BG5.state=bulbon
'	 BG6.state=bulbon
'	 BG7.state=bulbon
'	 BG8.state=bulbon
'	 BG9.state=bulbon
'	 BG10.state=bulbon
'	 BG11.state=bulbon
'	 BG12.state=bulbon
'	 BG13.state=bulbon
end sub

sub endattractmode()
    bAttractMode = False
    playfieldattract.enabled=false
    backglassattract.enabled=false
    messagetimer.enabled=false
    ShowHighScores.Enabled = 0
    addscore(0)
    setlights()
    B2SLightOff
end sub

Sub B2SLightOff
 If B2SOn Then
    Controller.B2SSetData 1, 0
    Controller.B2SSetData 2, 0
    Controller.B2SSetData 3, 0
    Controller.B2SSetData 4, 0
    Controller.B2SSetData 5, 0
    Controller.B2SSetData 6, 0
    Controller.B2SSetData 7, 0
    Controller.B2SSetData 8, 0
    Controller.B2SSetData 9, 0
    Controller.B2SSetData 10, 0
    Controller.B2SSetData 11, 0
    Controller.B2SSetData 12, 0
    Controller.B2SSetData 13, 0
    Controller.B2SSetData 28, 0
    Controller.B2SSetData 50, 0
 End If
End Sub


dim attractmessage

sub messagetimer_Timer()
    attractmessage = attractmessage + 1
    If attractmessage = 1 then
    D1.text = "Game Over"
    'player2reel.text = "Over"
    D2.text = "score  "&(score(currentplayer))
    'player4reel.text = (score(currentplayer))
    DisplayB2SText2 "   GAME OVER   " & "  SCORE   "&(score(currentplayer))
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene01 "Winners4", "blank.png", "GAME OVER "& score(CurrentPlayer)&" SCORE", 14, 5, 0, 8500, 1	
	End if
    end if
    If attractmessage = 2 then
    if Credits = 0 then
    D1.text = "Credits 0"
    D2.text = "Insert Coin"
   ' player2reel.text = "Coin"
   ' player3reel.text = ""
   ' player4reel.text = ""
    DisplayB2SText2 "   CREDITS 0    " & "   INSERT COIN  "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Credits0", FALSE, "black.png", "CRREDITS "&Credits, 15, -1, "INSERT COIN", 15, -1, 7, 2000, 5
	End if
    Else
    D1.text = "Credits: "&Credits
    'player2reel.text = " "&Credits
    D2.text = "Press Start"
   ' player4reel.text = "Start"
    DisplayB2SText2 "   CREDITS "&(Credits) &"     " & " PRESS START  "  
	If turnonultradmd > 0 then 
		UltraDMD.ScrollingCredits "blank.png", "Sonic Pinball Table||Created By:|BRENDAN BAILEY||MOD By:|Javier1515||UDMD||Created By:|Derek Crosby", 6, 10, 1000, 14  
	End If
    End If
    end if
    If attractmessage = 3 then
    D1.text = "Beat Dr"
   ' player2reel.text = "Dr"
    D2.text = "Robot nik"
    'player4reel.text = "nik"
    DisplayB2SText2 "    BEAT DR     " & "   ROBOT NICK   "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene01 "Winners4", "blank.png", "BEAT DR ROBOTNICK", 14, 5, 0, 7500, 1	
	End if
    end if
    If attractmessage = 4 then
    D1.text = "play the"
    'player2reel.text = "the"
    D2.text = "sonic pinball"
    'player4reel.text = "pinball"
    DisplayB2SText2 "    PLAY THE    " & "  SONIC PINBALL "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene01 "Winners3", "blank.png", "PLAY SONIC PINBALL", 14, 5, 0, 7500, 1	
	End if
    end if
    If attractmessage = 5 then
    D1.text = "sonic the"
    'player2reel.text = "the"
    D2.text = "hedge hog 2"
    'player4reel.text = "hog 2"
    DisplayB2SText2 "    SONIC THE   " & "   HEDGE HOG 2  "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene01 "[no name]", "blank.png", "Sonic Pinball Party", 14, 5, 0, 6000, 1	
	End if
    end if

    If attractmessage = 6 then
    'attractmessage = attractmessage - attractmessage
    ShowHighScores.Enabled = 1
    end if

    If attractmessage = 7 then
    D1.text = "extra ball at"
    'player2reel.text = "ball at"
    D2.text = "750000 / 1500000"
    'player4reel.text = "1500000"
    DisplayB2SText2 " EXTRABALLS AT: " & "750000 / 1500000"
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene01 "Winners2", "blank.png", "EXTRABALLS AT 750000 / 1500000", 14, 5, 0, 7500, 1	
	End if
    end if
    If attractmessage = 8 then
    D1.text = "collect rings"
    'player2reel.text = "by"
    D2.text = "everywhere"
    'player4reel.text = "Bailey"
    DisplayB2SText2 " COLLECT RINGS  " & "   EVERYWHERE   "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene01 "Winners", "blank.png", "Collect Rings Everywhere", 14, 5, 0, 6500, 1	
	End if
    end if

    If attractmessage = 9 then
    D1.text = "Created by"
    'player2reel.text = "by"
    D2.text = "Brendan Bailey"
    'player4reel.text = "Bailey"
    DisplayB2SText2 "   CREATED BY   " & " BRENDAN BAILEY "
    end if

    If attractmessage = 10 then
    D1.text = "Winners Don't"
    'player2reel.text = "Don't"
    D2.text = "Use Drugs"
   ' player4reel.text = "Drugs"
    DisplayB2SText2 "  WINNERS DONT  " & "   USE DRUGS    "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Winners", FALSE, "blank.png", "WINNERS DON'T", 15, -1, "USE DRUGS", 15, -1, 7, 2000, 5
	End if
    end if

    If attractmessage = 11 then
    if Credits = 0 then
    D1.text = "Credits 0"
    D2.text = "Insert Coin"
   ' player2reel.text = "Coin"
   ' player3reel.text = ""
   ' player4reel.text = ""
    DisplayB2SText2 "   CREDITS 0    " & "   INSERT COIN  "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Credits0", FALSE, "black.png", "CRREDITS 0", 15, -1, "INSERT COIN", 15, -1, 7, 2000, 5
	End if
    Else
    D1.text = "Credits: "&Credits
    'player2reel.text = " "&Credits
    D2.text = "Press Start"
   ' player4reel.text = "Start"
    DisplayB2SText2 "   CREDITS "&(Credits) &"     " & " PRESS START  "  
    End If
    End If

    If attractmessage = 12 then
    D1.text = "SONIC THE"
    D2.text = "HEDGE HOG 2"
    DisplayB2SText2 "    SONIC THE   " & "   HEDGE HOG 2  "
    End If

    If attractmessage = 13 then
    D1.text = "COMVERSION BY"
    'player2reel.text = " "&Credits
    D2.text = "JAVIER"
   ' player4reel.text = "Start"
    DisplayB2SText2 " COMVERSION BY  " & "   JAVIER       "  
    End If

    If attractmessage = 14 then
    D1.text = "REMEMBER"
    D2.text = "THE CHILDHOOD"
    DisplayB2SText2 "    REMEMBER    " & "                "
    end if

    If attractmessage = 15 then
    D1.text = "THE CHILDHOOD"
    D2.text = "NEVER LOSES"
    DisplayB2SText2 "  THE CHILDHOOD " & "  NEVER LOSES   "
    end if

    If attractmessage = 16 then
    D1.text = " ENJOY AND BE  "
    D2.text = " CHILD IN YOUR WAY"
    DisplayB2SText2 " ENJOY AND BE A" & "KID ON YOUR WAY"
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Enjoy", FALSE, "black.png", "ENJOY AND BE A KID", 15, -1, "ON YOUR TERMS", 15, -1, 7, 2000, 5
	End if
    attractmessage = attractmessage - attractmessage
    end if

	
end sub


Dim HSmessage
Sub ShowHighScores_Timer
    messagetimer.enabled = 0
	HSmessage = HSmessage + 1

    Select Case HSmessage
    Case 1:
	ShowHighScores.interval = 3000

	D1.text = " 1> " & HighScoreName(0) & "  " & HighScore(0)', seScrollRightOver
	D2.text = " 2> " & HighScoreName(1) & "  " & HighScore(1)', seScrollRightOver, , 20

    DisplayB2SText2 "    HIGHSCORES   "& "1> " & HighScoreName(0) & ":" & HighScore(0)' & " 

    Case 2:               

    DisplayB2SText2 "    HIGHSCORES   "& "2> " & HighScoreName(1) & ":" & HighScore(1)' & "                "

	D1.text = " 1> " & HighScoreName(0) & "  " & HighScore(0)', seScrollRightOver
	D2.text = " 2> " & HighScoreName(1) & "  " & HighScore(1)', seScrollRightOver, , 20


    Case 3:


	D1.text = " 3> " & HighScoreName(2) & "  " & HighScore(2)', seScrollRightOver
	D2.text = " 4> " & HighScoreName(3) & "  " & HighScore(3)', seScrollRightOver, , 20

    DisplayB2SText2 "    HIGHSCORES   "& "3> " & HighScoreName(2) & ":" & HighScore(2)' & "                "

    Case 4:

    DisplayB2SText2 "    HIGHSCORES   "& "4> " & HighScoreName(3) & ":" & HighScore(3)' & "                "

	D1.text = " 3> " & HighScoreName(2) & "  " & HighScore(2)', seScrollRightOver
	D2.text = " 4> " & HighScoreName(3) & "  " & HighScore(3)', seScrollRightOver, , 20

	Case 5:

	D1.text = "THE RING MASTER" 
	D2.text = " 1> " & SpecialHighScoreName(0) & ":  " & SpecialHighScore(0)

    DisplayB2SText2 "THE RING MASTER"& " 1> " & SpecialHighScoreName(0) & ":" & SpecialHighScore(0)
	Case 6:

	D1.text = "THE RING MASTER" 
	D2.text = " 2> " & SpecialHighScoreName(1) & ":  " & SpecialHighScore(1)

    DisplayB2SText2 "THE RING MASTER"& " 2> " & SpecialHighScoreName(1) & ":" & SpecialHighScore(1)
	Case 7:

	D1.text = "THE RING MASTER"
	D2.text = " 3> " & SpecialHighScoreName(2) & ":  " & SpecialHighScore(2) 

    DisplayB2SText2 "THE RING MASTER"& " 3> " & SpecialHighScoreName(2) & ":" & SpecialHighScore(2)

	Case 8:

	D1.text = "THE RING MASTER"
 	D2.text = " 4> " & SpecialHighScoreName(3) & ":  " & SpecialHighScore(3)

    DisplayB2SText2 "THE RING MASTER"& " 4> " & SpecialHighScoreName(3) & ":" & SpecialHighScore(3)

	Case 9
     HSmessage = 0
     attractmessage = 7
     ShowHighScores.Enabled = 0
    messagetimer.Interval = 2000
    messagetimer.enabled = 1
    End Select
End Sub



sub ballreset()
    bonusmulti = bonusmulti - bonusmulti
    bonus = bonus - bonus
    twoX.state=0
    threeX.state=0
    fourX.state=0
    fiveX.state=0
    ALight.state = 0
    BLight.state = 0
    CLight.state = 0
    droptarget7.Isdropped = 0
    targetlight1.state=0
    targetlight2.state=0
    targetlight3.state=0
    targetlight4.state=0
    targetlight5.state=0
    H=false
    U=false
    R=false
    Rtwo=false
    Y=false
end sub

sub tablereset()
    'score(currentplayer) = score(currentplayer) - score(currentplayer)
    'specialscore(currentplayer) = specialscore(currentplayer) - specialscore(currentplayer)
    bonus = bonus - bonus
	bonusmulti = bonusmulti - bonusmulti
    droptarget7.Isdropped = 0
    H=false
    U=false
    R=false
    Rtwo=false
    Y=false
    lock = false
    lit = false
    locklit = false
    multiballlit = false
    jackpotlit = false
    jackpotscore = false
    hurryuplit = false
    postlit = false
    restand1 = false
    restand2 = false
    restand3 = false
    restand4 = false
    restand5 = false
    restand6 = false
    extraball = false
    lockedballs = lockedballs - lockedballs
    ballonelocked = false
    balltwolocked = false
    mballshow = mballshow - mballshow
    mballshowtwo = mballshowtwo - mballshowtwo
    droptarget1.Isdropped = 0
    droptarget2.Isdropped = 0
    droptarget3.Isdropped = 0
    droptarget4.Isdropped = 0
    droptarget5.Isdropped = 0
    droptarget6.Isdropped = 0
    saver1lit = false
    saver2lit = false
    saver3lit = false
    invincible = invincible - invincible
    diesound = diesound - diesound
    extraballonescored=false
    extraballtwoscored=false
    mballshowtimer.enabled=false
    Kicker1.kick 250, 15
	Kicker2.kick 180, 15
end sub

Dim Message

sub showmessage()
    DMDUpdate.Enabled = 0
    Messageon = true
    Messageontimer.enabled=true
    If Message = 1 then
    D1.text = "Lock Is Lit"
	 'player2reel.text = "Lit"
	 'player3reel.text = ""
	 'player4reel.text = ""
    DisplayB2SText "  LOCK IS LIT   " & "                "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "LockIsLit", FALSE, "black.png", "Lock Is Lit !", 15, -1, "", -1, -1, 6, 2000, 1
	End if
    End If
    If Message = 2 then
    D1.text = "*MultiBall*"
	 'player2reel.text = "*Ball*"
	 'player3reel.text = "*Multi*"
	 'player4reel.text = "*Ball*"
    DisplayB2SText "  *MULTIBALL*   " & "                "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Multiball", FALSE, "black.png", "MULTIBALL", 15, -1, "", -1, -1, 10, 3000, 9
	End if
    End If
    If Message = 3 then
    D1.text = "*Hurry Up*"
	 'player2reel.text = "*Up*"
	D2.text = "Shoot Holes"
	 'player4reel.text = "Holes"
    DisplayB2SText "   HURRY UP     " & "  SHOOT HOLES   "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Hurry", FALSE, "black.png", "HURRY UP", 15, -1, "Shoot Holes", 15, -1, 7, 2000, 5
	End if
    End If
    If Message = 4 then
    D1.text = "You Are"
	D2.text = "Are InvinCible"
	 'player3reel.text = "Invin"
	 'player4reel.text = "Cible"
    DisplayB2SText "    YOU ARE     " & "   INVINCIBLE   "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Invincible", FALSE, "black.png", "You Are", 15, -1, "Invincible", 15, -1, 2, 2000, 3
	End if
    End If
    If Message = 5 then
    D1.text = "**ExtraBall**"
	 'player2reel.text = "Ball**"
	 'player3reel.text = ""
	 'player4reel.text = ""
    DisplayB2SText "  *EXTRABALL*   " & "                "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "ExtraBall", FALSE, "black.png", "-EXTRA BALL-", 15, -1, "", -1, -1, 11, 1000, 9
	End if
    End If
    If Message = 6 then
    D1.text = "***JackPot****"
	 'player2reel.text = "Pot****"
	 D2.text = "*250000*"
	 'player4reel.text = "250000*"
    DisplayB2SText "   *JACKPOT*    " & " **>250000<**   "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Jackpot", FALSE, "black.png", "!JACKPOT!", 15, -1, "", -1, -1, 2, 2000, 14
	End if
    End If
end sub

sub Messageontimer_Timer()
    Messageontimer.enabled = false
    Messageon = false
    addscore(0)
end sub


'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

  If BootE = True Then Exit Sub
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
		DOF 126, DOFOn
        playsoundring()
        If(Tilted = False) Then
            PlaySound "fx_coin"
          	'startB2S (31)
            DisplayB2SText2 " CREDITS " &credits
            D1.Text = " CREDITS " &credits

			FlashForMs LeftSlingshotBulb1, 200, 100, BulbOff
			FlashForMs LeftSlingshotBulb2,  200, 100, BulbOff
			FlashForMs RightSlingshotBulb1, 200, 100, BulbOff
			FlashForMs RightSlingshotBulb2, 200, 100, BulbOff
         GIflash()
       '     If NOT bGameInPlay Then ShowTableInfo:
        End If
    End If


    If keycode = PlungerKey Then PlaySound "fx_plungerpull" :Plunger.Pullback
    if ballinplunger = true then  playsound "spin2"  end if

    If bBallInPlungerLane Then DOF 125, DOFPulse: DOF 114, DOFPulse


	If bGameInPlay Then
        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then LevelAnim :Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
	end If

	If bGameInPlay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1


        If keycode = StartGameKey And bAttractMode = True Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then
                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
				If (BallsOnPlayfield = 0) Then
					' A Game is now in progress, reset the table for a new Game
					startgametimer.enabled=true
                    startsound()
                    Endattractmode
				End If
                Else

                    If(Credits > 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						If Credits < 1 Then DOF 126, DOFOff
					If (BallsOnPlayfield = 0) Then
						' remove a credit
						nvCredits = nvCredits - 1
						' much the same as above
						startgametimer.enabled=true
                        startsound()
						Endattractmode()
					End If
                    Else
                        ' Not Enough Credits to start a game.

                        'DMD CenterLine(0, "CREDITS " & Credits), CenterLine(1, "INSERT COIN"), 0, eNone, eBlink, eNone, 500, True, ""
                    End If
                End If
            End If
        End If
    Else ' If (GameInPlay)

             If keycode = StartGameKey And bAttractMode = True Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                       startgametimer.enabled=true
                        startsound()
						Endattractmode()
                    End If
                Else
                    If(Credits > 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
							If Credits < 1 Then DOF 126, DOFOff
							   playsound"warp" 
                               startgametimer.enabled=true
                               startsound()
						       Endattractmode()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        D1.Text = " CREDITS " &credits&"   INSERT COIN"
                        DisplayB2SText2 "   CREDITS " &credits &"      INSERT COIN "
						If turnonultradmd > 0 then 
							UltraDMD.DisplayScene00ExWithId "Credits", FALSE, "black.png", "Credits "&credits, 15, -1," Insert Coin", -1, -1, 14, 0, 14
						End if
                        
                    End If
                End If
            End If
    End If ' If (GameInPlay)

    If hsbModeActive Then EnterHighScoreKey(keycode)
    If SpecialhsbModeActive Then EnterSpecialHighScoreKey(keycode)

' Table specific
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If
    If keycode = PlungerKey Then PlaySound "fx_plunger": Plunger.Fire
End Sub


Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    bInstantInfo = True
'    DMDFlush
'    UltraDMDTimer.Enabled = 1
End Sub

Sub InstantInfo
     D1.Text = " POW " &POWBonusCount
 '   Jackpot = 1000000 + Round(Score(CurrentPlayer) / 10, 0)
 '   DMD "black.jpg", "", "INSTANT INFO", 500
 '   DMD "black.jpg", "JACKPOT", Jackpot, 800
 '   DMD "black.jpg", "LEVEL", Level(CurrentPlayer), 800
 '   DMD "black.jpg", "BONUS MULT", BonusMultiplier(CurrentPlayer), 800
 '   DMD "black.jpg", "ORBIT BONUS", OrbitHits, 800
  '  DMD "black.jpg", "LANE BONUS", LaneBonus, 800
   ' DMD "black.jpg", "TARGET BONUS", TargetBonus, 800
  '  DMD "black.jpg", "RAMP BONUS", RampBonus, 800
 '   DMD "black.jpg", "MONSTERS KILLED", Monsters(CurrentPlayer), 800
End Sub


'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
    If B2SOn Then Controller.Stop
End Sub







'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
'	startB2S(4)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToEnd
        Flipper1.RotateToEnd
        lanesleftUp

    Else
        PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToStart
        Flipper1.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
'	startB2S(4)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), 0, 1, 0.05, 0.15
        RightFlipper.RotateToEnd
        Flipper2.RotateToEnd
        Flipper3.RotateToEnd
        lanesRightUp 
    
    Else
        PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), 0, 1, 0.05, 0.15
        RightFlipper.RotateToStart
        Flipper2.RotateToStart
        Flipper3.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.05, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.05, 0.25
End Sub

sub lanesleftUp()
    TempState = ALight.State
	ALight.State = BLight.State
	BLight.State = CLight.State
	CLight.State = TempState
    'If bonuson = true then QuickB() End If
End Sub

sub lanesRightUp()
	TempState = CLight.State
	CLight.State = BLight.State
	BLight.State = ALight.State
	ALight.State = TempState
    'If bonuson = TRUE then QuickB() end if
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
	If Tilted Then Exit Sub
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
       'If B2Son = False Then 
		On Error Resume Next
		D1.text = "CAREFUL!"
		DisplayB2SText "CAREFUL!"
		If turnonultradmd > 0 then 
			UltraDMD.DisplayScene00ExWithId "Carefull", FALSE, "black.png", "Careful!", 15, -1, "", -1, -1, 2, 1000, 14
		End if
		Playsound "tilt"
        'End If
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
		'If B2SON = False Then 
		Tilted = True
		StopAllSounds
		Playsound "tilt"
		'display Tilt
		DMDUpdate.Enabled = 0
		D1.text = "TILT!"
		DisplayB2SText "      TILT!     "
		If turnonultradmd > 0 then 
			UltraDMD.DisplayScene00ExWithId "Tilt", FALSE, "black.png", "TILT", 15, -1, "", -1, -1, 14, 500, 14
		End if
		DisableTable True
		TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
		'End If
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
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
        Flipper1.RotateToStart
        Flipper2.RotateToStart
        Flipper3.RotateToStart
		DOF 101, DOFOff
		DOF 102, DOFOff
        Bumper1.Force = 0
        Bumper2.Force = 0
        Bumper3.Force = 0
        LeftSlingshotRubber.Disabled = 1
        RightSlingshotRubber.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        'Bumper1.Force = 6
        LeftSlingshotRubber.Disabled = 0
        RightSlingshotRubber.Disabled = 0
        'clean up the buffer display
    '    DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
		DisableTable False
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
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
        If UBound(tmp) = 0 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 127, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next

 If bAttractMode = False Then
    GiGameInPlay.state = 1
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
  Else
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSatDark"
    GiGameInPlay.state = 0
 End If
End Sub

Sub GiOff
    DOF 127, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next

    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSatDark"
    GiGameInPlay.state = 0

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


'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    BallShadowUpdate   
	'leftdiverterP.objRotZ = leftdiverter.CurrentAngle
    'rightdiverterP.objRotZ = rightdiverter.CurrentAngle
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




' *********************************************************************
'               Funciones para los sonidos de la mesa
' *********************************************************************

Function Vol(ball) ' Calcula el volumen del sonido basado en la velocidad de la bola
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculala posición estéreo de la bola (izquierda a derecha)
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calcula el tono según la velocidad de la bola
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calcula la velocidad de la bola
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'Solo para VPX 10.4 y siguentes: calcula la posición de arriba/abajo de la bola, para mesas con el sonido dolby
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) ' Hace sonar un sonido en la posición de un objeto, como bumpers y flippers
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' hace sonar un sonido en la posición de la bola
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*****************************************
'      Los sonidos de la bola/s rodando
'*****************************************

Const tnob = 6 ' número total de bola
Const lob = 1  'número de bola encerradas
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' para el sonido de bolas perdidas
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' sale de la rutina si no hay más bolas en la mesa
    If UBound(BOT) = lob - 1 Then Exit Sub

    ' hace sonar el sonido de la bola rodando para cada bola
    For b = lob to UBound(BOT)
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 20000 'Aumenta el tono del sonido si la bola está sobre una rampa
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'*****************************
' Sonido de las bolas chocando
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub





'*********** BALL SHADOW *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
	' render the shadow for each ball
    For b = 0 to Ubound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) - 10
		End If
	    ballShadow(b).Y = BOT(b).Y + 15
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub



'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
'Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_plastichit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aDropTargets_Hit(idx):PlaySound "fx_target", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub


Sub aRubber_Bands_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "fx_rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "fx_rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "fx_rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub KickerHelpRight_Hit()
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub




' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'

Sub StopGameOverSong
    PlaySong "mu_end"
    StopSound Song:Song = ""
    StopAllSounds
	If turnonultradmd > 0 then 
		UltraDMD.ScrollingCredits "blank.png", "Sonic Pinball Table||Created By:|BRENDAN BAILEY||UDMD|Created By:|Derek Crosby", 15, 10, 1000, 14  
	End if
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 13000 End If

    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If

    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 12000 End If

    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If

    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 11000 End If

    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If

    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 10000 End If

    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If



    x = LoadValue(TableName, "SpecialHighScore1")
    If(x <> "") Then SpecialHighScore(0) = CDbl(x) Else SpecialHighScore(0) = 4 End If

    x = LoadValue(TableName, "SpecialHighScore1Name")
    If(x <> "") Then SpecialHighScoreName(0) = x Else SpecialHighScoreName(0) = "AAA" End If

    x = LoadValue(TableName, "SpecialHighScore2")
    If(x <> "") then SpecialHighScore(1) = CDbl(x) Else SpecialHighScore(1) = 3 End If

    x = LoadValue(TableName, "SpecialHighScore2Name")
    If(x <> "") then SpecialHighScoreName(1) = x Else SpecialHighScoreName(1) = "BBB" End If

    x = LoadValue(TableName, "SpecialHighScore3")
    If(x <> "") then SpecialHighScore(2) = CDbl(x) Else SpecialHighScore(2) = 2 End If

    x = LoadValue(TableName, "SpecialHighScore3Name")
    If(x <> "") then SpecialHighScoreName(2) = x Else SpecialHighScoreName(2) = "CCC" End If

    x = LoadValue(TableName, "SpecialHighScore4")
    If(x <> "") then SpecialHighScore(3) = CDbl(x) Else SpecialHighScore(3) = 1 End If

    x = LoadValue(TableName, "SpecialHighScore4Name")
    If(x <> "") then SpecialHighScoreName(3) = x Else SpecialHighScoreName(3) = "DDD" End If



    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If

  '  x = LoadValue(TableName, "Score")
  '  If(x <> "") then Score(CurrentPlayer) = CInt(x) Else Score(CurrentPlayer) = 0 End If
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


    SaveValue TableName, "SpecialHighScore1", SpecialHighScore(0)
    SaveValue TableName, "SpecialHighScore1Name", SpecialHighScoreName(0)
    SaveValue TableName, "SpecialHighScore2", SpecialHighScore(1)
    SaveValue TableName, "SpecialHighScore2Name", SpecialHighScoreName(1)
    SaveValue TableName, "SpecialHighScore3", SpecialHighScore(2)
    SaveValue TableName, "SpecialHighScore3Name", SpecialHighScoreName(2)
    SaveValue TableName, "SpecialHighScore4", SpecialHighScore(3)
    SaveValue TableName, "SpecialHighScore4Name", SpecialHighScoreName(3)

    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
    SaveValue TableName, "Score(CurrentPlayer)", Score(CurrentPlayer)

End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 900000
    HighScore(1) = 850000
    HighScore(2) = 800000
    HighScore(3) = 750000

    SpecialHighScoreName(0) = "AAA"
    SpecialHighScoreName(1) = "BBB"
    SpecialHighScoreName(2) = "CCC"
    SpecialHighScoreName(3) = "DDD"
    SpecialHighScore(0) = 4
    SpecialHighScore(1) = 3
    SpecialHighScore(2) = 2
    SpecialHighScore(3) = 1
    Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive, SpecialhsbModeActive
Dim hsEnteredName, hsSpecialEnteredName
Dim hsEnteredDigits(3)
Dim hsSpecialEnteredDigits(3)
Dim hsCurrentDigit, hsSpecialCurrentDigit
Dim hsValidLetters, hsSpecialValidLetters
Dim hsCurrentLetter, hsSpecialCurrentLetter
Dim hsLetterFlash, hsSpecialLetterFlash
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim specialhighscorename(4)
Dim specialhighscore(4)
Dim specialscore(4)
Dim HighScoreReward
Dim SpecialHighScoreReward



Sub CheckHighscore()
   DMDUpdate.enabled = 0

    D1.Text = " "
    D2.Text = " "
    DisplayB2SText "     SCORE       " & "            "&Score(CurrentPlayer)
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "SCORE", FALSE, "", "SCORE "&Score(CurrentPlayer), 15, -1, "", -1, -1, 14, 0, 14
	End if
    Dim tmp
    tmp = Score(1)

    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(0) Then 'add 1 credit for beating the highscore
       DisplayB2SText " "
       HighScoreReward = 1
    End If

    If tmp> HighScore(3) Then
        PlaySound "spchkittenme"
        HighScore(3) = tmp
        D1.text = "  GREAT SCORE   " & "       "
        DisplayB2SText "  GREAT SCORE   " & "       " 
        'enter player's name
        vpmtimer.addtimer 2000, "HighScoreEntryInit '"
       ' HighScoreEntryInit()
    Else
       ' EndOfBallComplete()
        CheckSpecialHighscore
   End If
End Sub



Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "mu_jychighscore"
    hsLetterFlash = 0

    hsEnteredDigits(0) = "A"
    hsEnteredDigits(1) = "-"
    hsEnteredDigits(2) = "-"
    hsCurrentDigit = 0

    hsValidLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
    hsCurrentLetter = 1
   ' DMDFlush
    D1.text = "YOUR NAME:" & " "
    'DMDId "hsc", "", "YOUR NAME:", " ", 999999
    HighScoreDisplayName()
End Sub


Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "jycsfx12"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "jycsfx12"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "jycsfx9"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    DMDUpdate.enabled = 0
    Dim i, TempStr
    D1.text = TempStr
    DisplayB2SText "" & TempStr
    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
            DisplayB2SText TempStr & "_"
            D2.TEXT = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
            DisplayB2SText TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
            D2.TEXT = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
   ' DMDMod "hsc", "YOUR NAME:", Mid(TempStr, 2, 5), 999999
    DisplayB2SText "ENTER HIGHSCORE NAME " & Mid(TempStr, 2, 5)
    D1.TEXT = "ENTER HIGHSCORE NAME "
    D2.TEXT = "    " & Mid(TempStr, 2, 5)
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False
    'PlaySong "m_end"
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
   ' DMDFlush
   ' DMDUpdate.enabled = 1
    DisplayB2SText " CONGRATULATIONS " & "                "&Score(CurrentPlayer)

    LightEffect 2
    FlashEffect 2
    'EndOfBallComplete()
    'vpmtimer.addtimer 2500, "UnCreditoMas() '"
    vpmtimer.addtimer 2500, "CheckSpecialHighscore() '"
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
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




'********************
' Special HighScore
'********************


Sub CheckSpecialHighscore()

    DMDUpdate.enabled = 0
    D1.Text = " "
    D2.Text = " "
    DisplayB2SText "RINGS COLLECTED " & "                "&SpecialScore(CurrentPlayer)
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Rings", FALSE, Rings, "Rings Collected", 15, -1, SpecialScore(CurrentPlayer), 15, -1, 6, 2000, 1
	End if
    Dim tmp
    tmp = SpecialScore(1)

    If SpecialScore(2)> tmp Then tmp = SpecialScore(2)
    If SpecialScore(3)> tmp Then tmp = SpecialScore(3)
    If SpecialScore(4)> tmp Then tmp = SpecialScore(4)

    If tmp> SpecialHighScore(0) Then 'add 1 credit for beating the highscore
       SpecialHighScoreReward = 1
       DisplayB2SText " "
    End If

    If tmp> SpecialHighscore(3) Then
        SpecialHighscore(3) = tmp
        'enter player's name
        PlaySound "spchnotoveryet"
        D1.text = "  GOOD GAME  "
        D2.text = "    "
        DisplayB2SText "    GOOD GAME   " & "               " 
        vpmtimer.addtimer 2000, "SpecialHighScoreEntryInit '"  
        'SpecialHighScoreEntryInit()
    Else
        CreditsHighScoreReward.Enabled = 1
        'EndOfBallComplete()
    End If
End Sub


Sub SpecialHighScoreEntryInit()
    SpecialhsbModeActive = True
    StopSound "mu_jychighscore"
    PlaySound "mu_jychighscore"
    hsSpecialLetterFlash = 0


    hsSpecialEnteredDigits(0) = "A"
    hsSpecialEnteredDigits(1) = "-"
    hsSpecialEnteredDigits(2) = "-"
    hsSpecialCurrentDigit = 0

    hsSpecialValidLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
    hsSpecialCurrentLetter = 1
   ' DMDFlush
    'display1.text = "YOUR NAME:" & " "
    'DMDId "hsc", "", "YOUR NAME:", " ", 999999
    SpecialHighScoreDisplayName()
End Sub


Sub EnterSpecialHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "jycsfx12"
        hsSpecialCurrentLetter = hsSpecialCurrentLetter - 1
        if(hsSpecialCurrentLetter = 0) then
            hsSpecialCurrentLetter = len(hsSpecialValidLetters)
        end if
        SpecialHighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "jycsfx12"
        hsSpecialCurrentLetter = hsSpecialCurrentLetter + 1
        if(hsSpecialCurrentLetter> len(hsSpecialValidLetters) ) then
            hsSpecialCurrentLetter = 1
        end if
        SpecialHighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsSpecialValidLetters, hsSpecialCurrentLetter, 1) <> "<") then
            playsound "jycsfx9"
            hsSpecialEnteredDigits(hsSpecialCurrentDigit) = mid(hsSpecialValidLetters, hsSpecialCurrentLetter, 1)
            hsSpecialCurrentDigit = hsSpecialCurrentDigit + 1
            if(hsSpecialCurrentDigit = 3) then
                SpecialHighScoreCommitName()
            else
                SpecialHighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
            hsSpecialEnteredDigits(hsSpecialCurrentDigit) = " "
            if(hsSpecialCurrentDigit> 0) then
                hsSpecialCurrentDigit = hsSpecialCurrentDigit - 1
            end if
            SpecialHighScoreDisplayName()
        end if
    end if
End Sub

Sub SpecialHighScoreDisplayName()
    DMDUpdate.enabled = 0
    Dim i, TempStr
    D2.text = TempStr
    DisplayB2SText "" & TempStr
    TempStr = " >"
    if(hsSpecialCurrentDigit> 0) then TempStr = TempStr & hsSpecialEnteredDigits(0)
    if(hsSpecialCurrentDigit> 1) then TempStr = TempStr & hsSpecialEnteredDigits(1)
    if(hsSpecialCurrentDigit> 2) then TempStr = TempStr & hsSpecialEnteredDigits(2)

    if(hsSpecialCurrentDigit <> 3) then
        if(hsSpecialLetterFlash <> 0) then
            TempStr = TempStr & "_"
            DisplayB2SText TempStr & "_"
            D2.TEXT = TempStr & "_"
        else
            TempStr = TempStr & mid(hsSpecialValidLetters, hsSpecialCurrentLetter, 1)
            DisplayB2SText TempStr & mid(hsSpecialValidLetters, hsSpecialCurrentLetter, 1)
            D2.TEXT = TempStr & mid(hsSpecialValidLetters, hsSpecialCurrentLetter, 1)
        end if
    end if

    if(hsSpecialCurrentDigit <1) then TempStr = TempStr & hsSpecialEnteredDigits(1)
    if(hsSpecialCurrentDigit <2) then TempStr = TempStr & hsSpecialEnteredDigits(2)

    TempStr = TempStr & "< "
   ' DMDMod "hsc", "YOUR NAME:", Mid(TempStr, 2, 5), 999999
    DisplayB2SText "RING MASTER NAME" & Mid(TempStr, 2, 5)
    D1.TEXT = "RING MASTER NAME"
    D2.TEXT = "    " & Mid(TempStr, 2, 5)
End Sub

Sub SpecialHighScoreCommitName()
    SpecialhsbModeActive = False
    'PlaySong "m_end"
    hsSpecialEnteredName = hsSpecialEnteredDigits(0) & hsSpecialEnteredDigits(1) & hsSpecialEnteredDigits(2)
    if(hsSpecialEnteredName = "   ") then
        hsSpecialEnteredName = "YOU"
    end if

    SpecialHighScoreName(3) = hsSpecialEnteredName
    SortSpecialHighscore
    DisplayB2SText " CONGRATULATIONS " & SpecialScore(CurrentPlayer)&"   RINGS         "
    LightEffect 2
    FlashEffect 2
    RingReward = True
    vpmtimer.addtimer 2500, "CreditsHighScoreReward.Enabled = True '" 
    'CreditsHighScoreReward.Enabled = True
    'EndOfBallComplete()
End Sub

Sub SortSpecialHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If SpecialHighScore(j) <SpecialHighScore(j + 1) Then
                tmp = SpecialHighScore(j + 1)
                tmp2 = SpecialHighScoreName(j + 1)
                SpecialHighScore(j + 1) = SpecialHighScore(j)
                SpecialHighScoreName(j + 1) = SpecialHighScoreName(j)
                SpecialHighScore(j) = tmp
                SpecialHighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub



Dim RingReward
Sub CreditsHighScoreReward_Timer()
    CreditsHighScoreReward.Enabled = False

 If RingReward = True Then 
    RingReward = False              

   If HighScoreReward = 1 And SpecialHighScoreReward = 1  Then
      Credits = Credits + 1
      PlaySound SoundFXDOF("fx_knocker",124,DOFPulse,DOFKnocker)
      D1.text = "     CREDITS" & "  "&(Credits) &"  " & "      "
      HighScoreReward = 0: SpecialHighScoreReward = 0
      DisplayB2SText "     CREDITS" & "  "&(Credits) &"  " & "      "
      vpmtimer.addtimer 1500, "UnCreditoMas '"
   End If

   If HighScoreReward = 1 And SpecialHighScoreReward = 0 Then
      Credits = Credits + 1
      PlaySound SoundFXDOF("fx_knocker",124,DOFPulse,DOFKnocker)
      D1.text = "     CREDITS" & "  "&(Credits) &"  " & "      "
      HighScoreReward = 0:   DisplayB2SText "     CREDITS" & "  "&(Credits) &"  " & "      "
      DisplayB2SText "     CREDITS" & "  "&(Credits) &"  " & "      "
		If turnonultradmd > 0 then 
			UltraDMD.DisplayScene00ExWithId "Credits", FALSE, "black.png", "+1 Credit", 15, -1, "", -1, -1, 14, 0, 14
		End if
      'vpmtimer.addtimer 2500, "EndOfBallComplete() '"
   End If

   If HighScoreReward = 0 And SpecialHighScoreReward = 1 Then
      Credits = Credits + 1
      PlaySound SoundFXDOF("fx_knocker",124,DOFPulse,DOFKnocker)
      D1.text = "     CREDITS" & "  "&(Credits) &"  " & "      "
      SpecialHighScoreReward = 0:   DisplayB2SText "     CREDITS" & "  "&(Credits) &"  " & "      "
      DisplayB2SText "     CREDITS" & "  "&(Credits) &"  " & "      "
	  If turnonultradmd > 0 then 
			UltraDMD.DisplayScene00ExWithId "Credits", FALSE, "black.png", "+1 Credit", 15, -1, "", -1, -1, 14, 0, 14
	  End if
      'vpmtimer.addtimer 2500, "EndOfBallComplete() '" 
   End If

 ' If HighScoreReward = 0 And SpecialHighScoreReward = 0 Then
 ' End If

 End If

 EndOfBallComplete()

End Sub

Sub UnCreditoMas()	
    DMDUpdate.enabled = 0
    Credits = Credits + 1: PlaySound "fx_knocker"
    D1.text = "     CREDITS" & "  "&(Credits) &"  " & "      "
    DisplayB2SText "     CREDITS" & "  "&(Credits) &"  " & "      "
	If turnonultradmd > 0 then 
		UltraDMD.DisplayScene00ExWithId "Credits", FALSE, "black.png", "+2 Credit", 15, -1, "", -1, -1, 14, 0, 14
	End if
    'vpmtimer.addtimer 2500, "EndOfBallComplete() '" 
End Sub




'********************
' Music as wav sounds
'********************





Dim bMusicOn
Dim SongMS
SongMS = ""

Sub PlaySong(name)
    If bMusicOn Then
        If SongMS <> name Then
            StopSound SongMS
            SongMS = name
            If SongMS = "mu_end" Then
                'PlaySound SongMS, 0, 0.1  'this last number is the volume, from 0 to 1
            Else
                PlaySound SongMS, -1, 0.5 'this last number is the volume, from 0 to 1
            End If
        End If
    End If
End Sub






'-----------------------------
'-----  FS Display Code  -----
'-----------------------------

'If You want to hide a display, set the reel value of every reel to 44. This picture is transparent
'This is best done using collection:
'
'	If HideDisplay then 
'		For Each obj in ReelsCollection:obj.setvalue(44):next
'	end if
 

 Dim Char(32),TempText                    'increase dimension if You need larger displays



'-----------------------------------------------
'-----  B2S section, not used in the demo  -----
'-----------------------------------------------

Sub DisplayB2SText(TextPar)							'Procedure to display Text on a 28 digit B2S LED reel. Assuming that it is display 1 with internal digit numbers 1-32
  If B2SOn Then
	TempText = TextPar		
	for i = 1 to 32
		if i <= len(TextPar) then
			Char(i) = left(TempText,1)
			TempText = right(Temptext,len(TempText)-1)		
		else
			Char(i) = " "
		end if
	next
	if B2SOn Then
	for i = 1 to 32
		controller.B2SSetLED i,B2SLEDValue(Char(i))
	next
	end if
  End If
End Sub


Sub DisplayB2SText2(TextPar)							'Procedure to display Text on a 28 digit B2S LED reel. Assuming that it is display 1 with internal digit numbers 1-32
 If B2SOn Then
	TempText = TextPar		
	for i = 1 to 32
		if i <= len(TextPar) then
			Char(i) = left(TempText,1)
			TempText = right(Temptext,len(TempText)-1)		
		else
			Char(i) = " "
		end if
	next

	for i = 1 to 32
		controller.B2SSetLED i,B2SLEDValue(Char(i))

	next
End If

DMDUpdate.interval = 2000
DMDUpdate.enabled = 1

End Sub



Function B2SLEDValue(CharPar)						'to be used with dB2S 15-segments-LED used in Herweh's Designer
	B2SLEDValue = 0									'default for unknown characters
	select case CharPar
		Case "","":	B2SLEDValue = 0
		Case "0":	B2SLEDValue = 63	
		Case "1":	B2SLEDValue = 8704
		Case "2":	B2SLEDValue = 2139
		Case "3":	B2SLEDValue = 2127	
		Case "4":	B2SLEDValue = 2150
		Case "5":	B2SLEDValue = 2157
		Case "6":	B2SLEDValue = 2172
		Case "7":	B2SLEDValue = 7
		Case "8":	B2SLEDValue = 2175
		Case "9":	B2SLEDValue = 2159
		Case "A":	B2SLEDValue = 2167
		Case "B":	B2SLEDValue = 10767
		Case "C":	B2SLEDValue = 57
		Case "D":	B2SLEDValue = 8719
		Case "E":	B2SLEDValue = 121
		Case "F":	B2SLEDValue = 2161
		Case "G":	B2SLEDValue = 2109
		Case "H":	B2SLEDValue = 2166
		Case "I":	B2SLEDValue = 8713
		Case "J":	B2SLEDValue = 31
		Case "K":	B2SLEDValue = 5232
		Case "L":	B2SLEDValue = 56
		Case "M":	B2SLEDValue = 1334
		Case "N":	B2SLEDValue = 4406
		Case "O":	B2SLEDValue = 63
		Case "P":	B2SLEDValue = 2163
		Case "Q":	B2SLEDValue = 4287
		Case "R":	B2SLEDValue = 6259
		Case "S":	B2SLEDValue = 2157
		Case "T":	B2SLEDValue = 8705
		Case "U":	B2SLEDValue = 62
		Case "V":	B2SLEDValue = 17456
		Case "W":	B2SLEDValue = 20534
		Case "X":	B2SLEDValue = 21760
		Case "Y":	B2SLEDValue = 9472
		Case "Z":	B2SLEDValue = 17417
		Case "<":	B2SLEDValue = 5120
		Case ">":	B2SLEDValue = 16640
		Case "^":	B2SLEDValue = 17414
		Case ".":	B2SLEDValue = 8
		Case "!":	B2SLEDValue = 0
		Case ".":	B2SLEDValue = 128
		Case "*":	B2SLEDValue = 32576
		Case "/":	B2SLEDValue = 17408
		Case "\":	B2SLEDValue = 4352
		Case "|":	B2SLEDValue = 8704
		Case "=":	B2SLEDValue = 2120
		Case "+":	B2SLEDValue = 10816
		Case "-":	B2SLEDValue = 2112
	end select			
	B2SLEDValue = cint(B2SLEDValue)
End Function





Dim Ball

Sub DMDScore
	 DisplayB2SText Score(1) & "" & "                  BALL " & Ball & " "
     D1.text = Score(CurrentPlayer) &"                "                   
     D2.text = "           BALL " & Ball 
	 'UltraDMD.DisplayScoreboard "Player "&CurrentPlayer, 1, Score(CurrentPlayer), 2000000, 3000000, 4000000, "Player "&CurrentPlayer,"BALL "&Ball
	 'UltraDMD.DisplayScoreboard "Player "CurrentPlayer, 1, Score(1), 2000000, 3000000, 4000000, "Player "&CurrentPlayer,"BALL "&Ball
	 'UltraDMD.DisplayScoreboard 1, 1, Score(CurrentPlayer), 2000000, 3000000, 4000000, CurrentPlayer, "FREE PLAY"
     DMDUpdate.enabled = 1
End Sub





Sub DMDUpdate_Timer
    DMDUpdate.enabled = 0
 If PlayersPlayingGame = 1 And BallsOnPlayfield >= 1 Then
    DisplayB2SText " "
    D1.text = " "
    D2.text = " "
    DMDScore 
    AddScore (0)
    DMDUpdate.interval = 1000	
    DMDUpdate.enabled = 1
  Else
    DisplayB2SText " "
    D1.text = " "
    D2.text = " "
    DMDUpdate.interval = 1500	
    DMDUpdate.enabled = 1
 End If
End Sub




Dim LevelB
Sub LevelAnim()
    LevelB = True
 If LevelB = True Then
    mLevelMagnet.MagnetOn = 0
    vpmtimer.addtimer 500, "mLevelMagnet.MagnetOn = 1 '" 
 end If
End Sub

Sub BallLevelKick_Hit
    BallLevelKick.kick 0, 3
End Sub







