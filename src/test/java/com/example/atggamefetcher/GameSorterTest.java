// Copyright AB Trav och Galopp (556180-4161)
package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.Game;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

class GameSorterTest {
    private static final String EXAMPLE_V_64_GAME = "Example V64 game";
    private static final String EXAMPLE_V_86_GAME = "Example V86 game";
    private static final String EXAMPLE_V_75_GAME = "Example V75 game";
    private static final String EXAMPLE_GS_75_GAME = "Example GS75 game";

    private GameSorter gameSorter;
    private List<Game> mockedGames;

    @BeforeEach
    void setUp() {
        mockedGames = Arrays.asList(
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now(), false),
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().minusHours(1), false),

            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(1), false),
            new Game(EXAMPLE_V_86_GAME, "V86", LocalDateTime.now().plusDays(2), true),
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(3), false),
            new Game(EXAMPLE_V_86_GAME, "V86", LocalDateTime.now().plusDays(4), false),
            new Game(EXAMPLE_V_75_GAME, "V75", LocalDateTime.now().plusDays(5), true),
            new Game(EXAMPLE_GS_75_GAME, "GS75", LocalDateTime.now().plusDays(6), true),

            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(7), false),
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(8), false),
            new Game(EXAMPLE_V_86_GAME, "V86", LocalDateTime.now().plusDays(9), true),
            new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.now().plusDays(10), false),
            new Game(EXAMPLE_GS_75_GAME, "GS75", LocalDateTime.now().plusDays(11), false),
            new Game(EXAMPLE_V_75_GAME, "V75", LocalDateTime.now().plusDays(12), true),
            new Game(EXAMPLE_GS_75_GAME, "GS75", LocalDateTime.now().plusDays(13), true)
        );
        gameSorter = new GameSorter();
    }

    @Test
    void verifyThereAreOnlyTwoGamesAddForToday() {
        List<Game> addTodayGames = gameSorter.addTodayGames(mockedGames);
        assertEquals(2, addTodayGames.size());
    }

    @Test
    void verifyThatThereAreOnlyTwoGamesToday() {
        List<Game> gamesForToday = gameSorter.getGamesForToday(mockedGames);
        assertEquals(15, gamesForToday.size());
    }

    @Test
    void verifyThatSecondGameIsToday() {
        List<Game> gamesForToday = gameSorter.getGamesForToday(mockedGames);
        assertEquals(LocalDateTime.now().getDayOfYear(), gamesForToday.get(1).getStart().getDayOfYear());
    }
    @Test
    void verifyThatSecondGameTodayIsNotBig() {
        List<Game> gamesForToday = gameSorter.getGamesForToday(mockedGames);
        assertEquals(false, gamesForToday.get(1).getBigGame());
    }

    @Test
    void verifyThatFourthGameIsBig() {
        List<Game> gamesForToday = gameSorter.getGamesForToday(mockedGames);
        assertEquals(true, gamesForToday.get(3).getBigGame());
    }

    @Test
    void verifyThatTheTenthGameIsNotToday() {
        List<Game> gamesForToday = gameSorter.getGamesForToday(mockedGames);
        assertNotEquals(LocalDateTime.now().getDayOfYear(), gamesForToday.get(9).getStart().getDayOfYear());
    }

    @Test
    void verifyThatTheTenthGameIsNotBig() {
        List<Game> gamesForToday = gameSorter.getGamesForToday(mockedGames);
        assertEquals(false, gamesForToday.get(9).getBigGame());
    }

    @Test
    void verifyThatThereAreOnlySixBigGamesToday() {
        List<Game> bigGames = gameSorter.addBigGames(mockedGames);
        assertEquals(6, bigGames.size());
    }

    @Test
    void verifyThatTheSecondGameIsReallyBig() {
        List<Game> bigGames = gameSorter.addBigGames(mockedGames);
        assertEquals(true, bigGames.get(1).getBigGame());
    }

    @Test
    void verifyThatThereAreOnlySixGamesLeftOver() {
        List<Game> leftOverGames = gameSorter.addLeftOverGames(mockedGames);
        assertEquals(7, leftOverGames.size());
    }

    @Test
    void verifyThatTheThirdGameIsNotToday() {
        List<Game> leftOverGames = gameSorter.addLeftOverGames(mockedGames);
        assertNotEquals(LocalDateTime.now().getDayOfYear(), leftOverGames.get(2).getStart().getDayOfYear());
    }

    @Test
    void verifyThatTheThirdGameIsNotBig() {
        List<Game> leftOverGames = gameSorter.addLeftOverGames(mockedGames);
        assertEquals(false, leftOverGames.get(2).getBigGame());
    }
}