package com.example.atggamefetcher;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.Month;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class GameTest {

    private static final String EXAMPLE_V_64_GAME = "Example V64 game";
    private static final String EXAMPLE_V_86_GAME = "Example V86 game";
    private static final String EXAMPLE_V_75_GAME = "Example V75 game";
    private static final String EXAMPLE_GS_75_GAME = "Example GS75 game";

    private Game game;
    private List<Game> mockedGames;

    @BeforeEach
    void setUp() {
        mockedGames = Arrays.asList(
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now(), false), // Today 1
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().minusHours(1), false), // Today 2

            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(1), false), // Wednesday 3
            new Game(EXAMPLE_V_86_GAME, "V86", LocalDateTime.now().plusDays(2), true), // Wednesday 4
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(3), false), // Thursday 5
            new Game(EXAMPLE_V_86_GAME, "V86", LocalDateTime.now().plusDays(4), false), // Friday 6
            new Game(EXAMPLE_V_75_GAME, "V75", LocalDateTime.now().plusDays(5), true), // Saturday 7
            new Game(EXAMPLE_GS_75_GAME, "GS75", LocalDateTime.now().plusDays(6), true), // Sunday 8

            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(7), false), // Monday 9
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(8), false), // Tuesday 10
            new Game(EXAMPLE_V_86_GAME, "V86", LocalDateTime.now().plusDays(9), true), // Wednesday 11
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(10), false), // Thursday 12
            new Game(EXAMPLE_GS_75_GAME, "GS75", LocalDateTime.now().plusDays(11), false), // Friday 13
            new Game(EXAMPLE_V_75_GAME, "V75", LocalDateTime.now().plusDays(12), true), // Saturday 14
            new Game(EXAMPLE_GS_75_GAME, "GS75", LocalDateTime.now().plusDays(13), true) // Sunday 15
        );
        game = new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.of(2023, Month.OCTOBER, 28, 19, 30, 40), false);


    }

    @Test
    void compareTo() {
        Game testGame = new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.of(2023, Month.OCTOBER, 28, 19, 30, 40), false);
        assertEquals(0,game.compareTo(testGame));
    }

    @Test
    void getStart() {
        LocalDateTime testTime = LocalDateTime.of(2023, Month.OCTOBER, 28, 19, 30, 40);
        assertEquals(testTime,game.getStart());
    }

    @Test
    void getBigGame() {
        assertEquals(false,game.getBigGame());
    }

    @Test
    void testEquals() {
        Game testGame = new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.of(2023, Month.OCTOBER, 28, 19, 30, 40), false);
        assertEquals(true,game.equals(testGame));
    }

    @Test
    void testHashCode() {
        Game testGame = new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.of(2023, Month.OCTOBER, 28, 19, 30, 40), false);
        assertEquals(testGame.hashCode(),game.hashCode());
    }
}