package com.example.atggamefetcher;

import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class GameListCreatorTest {

    @Test
    void getGamesIncludingBigGames() {
    }

    @Test
    void readBigGameDates() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        gameListCreator.readBigGameDates();
    }

    @Test
    void readBigGameTypes() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        gameListCreator.readBigGameTypes();
    }

    @Test
    void addTodaysGames() {
    }

    @Test
    void addBigGames() {
    }

    @Test
    void addLeftOverGames() {
    }
}