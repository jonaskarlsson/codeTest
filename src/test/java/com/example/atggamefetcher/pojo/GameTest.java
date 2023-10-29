package com.example.atggamefetcher.pojo;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.*;

class GameTest {

    @Test
    void getName() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        assertEquals("Example V64 game", game.getName());
    }

    @Test
    void setName() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        game.setName("Example V86 game");
    }

    @Test
    void getType() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        assertEquals("V64", game.getType());
    }

    @Test
    void setType() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        game.setType("V86");
    }

    @Test
    void getStart() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        assertEquals(LocalDateTime.now().getDayOfYear(), game.getStart().getDayOfYear());
    }

    @Test
    void setStart() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        game.setStart(LocalDateTime.now().plusDays(1));
        assertEquals(LocalDateTime.now().plusDays(1).getDayOfYear(), game.getStart().getDayOfYear());
    }

    @Test
    void getBigGame() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        assertEquals(false, game.getBigGame());
    }

    @Test
    void setBigGame() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now(), false);
        game.setBigGame(true);
        assertEquals(true, game.getBigGame());
    }

    @Test
    void compareTo() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now().toLocalDate().atStartOfDay(), false);
        Game testGame = new Game("Example V64 game", "V64", LocalDateTime.now().toLocalDate().atStartOfDay(), false);
        assertEquals(0,game.compareTo(testGame));
    }

    @Test
    void testEquals() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now().toLocalDate().atStartOfDay(), false);
        Game testGame = new Game("Example V64 game", "V64", LocalDateTime.now().toLocalDate().atStartOfDay(), false);
        assertEquals(game, testGame);

    }

    @Test
    void testHashCode() {
        Game game = new Game("Example V64 game", "V64", LocalDateTime.now().toLocalDate().atStartOfDay(), false);
        Game testGame = new Game("Example V64 game", "V64", LocalDateTime.now().toLocalDate().atStartOfDay(), false);
        assertEquals(testGame.hashCode(),game.hashCode());
    }
}