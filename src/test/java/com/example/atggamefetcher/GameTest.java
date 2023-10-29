package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.Game;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.Month;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GameTest {
    private static final String EXAMPLE_V_64_GAME = "Example V64 game";
    private Game game;

    @BeforeEach
    void setUp() {
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
        assertEquals(game, testGame);
    }

    @Test
    void testHashCode() {
        Game testGame = new Game(EXAMPLE_V_64_GAME, "V64", LocalDateTime.of(2023, Month.OCTOBER, 28, 19, 30, 40), false);
        assertEquals(testGame.hashCode(),game.hashCode());
    }
}