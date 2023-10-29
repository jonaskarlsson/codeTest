package com.example.atggamefetcher.pojo;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.*;

class GameForJsonTest {
    GameForJson gameForJson;

    @BeforeEach
    void setUp() {
        gameForJson = new GameForJson();
        gameForJson.setName("Example V64 game");
        gameForJson.setType("V64");
        gameForJson.setStart(LocalDateTime.now().toLocalDate().atStartOfDay());
    }

    @Test
    void getName() {
        assertEquals("Example V64 game", gameForJson.getName());
    }

    @Test
    void setName() {
        gameForJson.setName("Example V86 game");
        assertEquals("Example V86 game", gameForJson.getName());
    }

    @Test
    void getType() {
        assertEquals("V64", gameForJson.getType());
    }

    @Test
    void setType() {
        gameForJson.setType("V86");
        assertEquals("V86", gameForJson.getType());
    }

    @Test
    void getStart() {
        assertEquals(LocalDateTime.now().toLocalDate().atStartOfDay(), gameForJson.getStart());
    }

    @Test
    void setStart() {
        gameForJson.setStart(LocalDateTime.now().plusDays(1).toLocalDate().atStartOfDay());
        assertEquals(LocalDateTime.now().plusDays(1).toLocalDate().atStartOfDay(), gameForJson.getStart());
    }

    @Test
    void compareTo() {
        gameForJson.setStart(LocalDateTime.now().plusDays(1).toLocalDate().atStartOfDay());
        GameForJson gameForJson2 = new GameForJson();
        gameForJson2.setStart(LocalDateTime.now().plusDays(2).toLocalDate().atStartOfDay());
        assertEquals(-1, gameForJson.compareTo(gameForJson2));
    }

    @Test
    void testEquals() {
        GameForJson gameForJson2 = new GameForJson();
        gameForJson2.setName("Example V64 game");
        gameForJson2.setType("V64");
        gameForJson2.setStart(LocalDateTime.now().toLocalDate().atStartOfDay());
        assertEquals(true, gameForJson.equals(gameForJson2));
    }

    @Test
    void testHashCode() {
        GameForJson gameForJson2 = new GameForJson();
        gameForJson2.setName("Example V64 game");
        gameForJson2.setType("V64");
        gameForJson2.setStart(LocalDateTime.now().toLocalDate().atStartOfDay());
        assertEquals(gameForJson.hashCode(), gameForJson2.hashCode());
    }
}