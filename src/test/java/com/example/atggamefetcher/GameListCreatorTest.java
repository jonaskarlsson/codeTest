package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.Game;
import com.example.atggamefetcher.pojo.GameForJson;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

class GameListCreatorTest {

    private ArrayList<GameForJson> gameList;

    @BeforeEach
    void setUp() {
        String json = """
            [
              {
                "name": "Example Game 1",
                "type": "V64",
                "start": "2023-12-23T16:30:00"
              },
              {
                "name": "Example Game 2",
                "type": "V64",
                "start": "2023-12-04T16:30:00"
              },
              {
                "name": "Example Game 3",
                "type": "V86",
                "start": "2023-10-29T16:30:00"
              },
              {
                "name": "Example Game 4",
                "type": "V86",
                "start": "2023-12-07T16:30:00"
              },
              {
                "name": "Example Game 5",
                "type": "V86",
                "start": "2023-12-08T16:30:00"
              }
            ]""";

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());

        try {
            GameForJson[] games = objectMapper.readValue(json, GameForJson[].class);
            gameList = new ArrayList<>();
            gameList.addAll(Arrays.asList(games));
        } catch (IOException e) {
            //Call to 'printStackTrace()' should probably be replaced with more robust logging
            e.printStackTrace();
        }
    }

    @Test
    void getGamesIncludingBigGamesShouldContainOnlyOneElementWithBigGame() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        List<Game> actualGameList = gameListCreator.getGamesIncludingBigGames(gameList);
        int numberOfBigGames = 0;
        for (Game game: actualGameList) {
              {
                if (game.getBigGame()) {
                    numberOfBigGames++;
                }
              }
        }
        assertEquals(1, numberOfBigGames);
    }

    @Test
    void firstGameIncludingBigGamesShouldBeBigGame() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        List<Game> actualGameList = gameListCreator.getGamesIncludingBigGames(gameList);
        Game actualGame = actualGameList.get(0);
        assertEquals(true, actualGame.getBigGame());
    }

    @Test
    void addGameToTodayGames() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        GameForJson gameForJson = new GameForJson();
        gameForJson.setStart(LocalDateTime.now());
        gameForJson.setName("Test");
        gameForJson.setType("V64");
        gameListCreator.addGameToTodayGames(gameForJson,false);
        assertEquals(1, gameListCreator.getTodayGames().size());
    }

    @Test
    void isBigGameV86() throws IOException {
        GameForJson gameForJson = new GameForJson();
        gameForJson.setStart(LocalDateTime.of(2023,10,25,0,0));
        gameForJson.setName("Example Game 5 V86");
        gameForJson.setType("V86");
        GameListCreator gameListCreator = new GameListCreator();
        assertEquals(true,gameListCreator.isBigGameV86(gameForJson));
    }

    @Test
    void isBigGameV75() throws IOException {
        GameForJson gameForJson = new GameForJson();
        gameForJson.setStart(LocalDateTime.of(2023,10,28,0,0));
        gameForJson.setName("Example Game V75");
        gameForJson.setType("V75");
        GameListCreator gameListCreator = new GameListCreator();
        assertEquals(true,gameListCreator.isBigGameV75(gameForJson));
    }

    @Test
    void isBigGameGS75() throws IOException {
        GameForJson gameForJson = new GameForJson();
        gameForJson.setStart(LocalDateTime.of(2023,10,29,0,0));
        gameForJson.setName("Example Game GS75");
        gameForJson.setType("GS75");
        GameListCreator gameListCreator = new GameListCreator();
        assertEquals(true,gameListCreator.isBigGameGS75(gameForJson));
    }

    @Test
    void isBigGameDay() throws IOException {
        GameForJson gameForJson = new GameForJson();
        gameForJson.setStart(LocalDateTime.of(2023,12,23,0,0));
        gameForJson.setName("Example Game GS75");
        gameForJson.setType("GS75");
        GameListCreator gameListCreator = new GameListCreator();
        assertEquals(true,gameListCreator.isBigGameDay(gameForJson));
    }

    @Test
    void isBigGameType() throws IOException {
        GameForJson gameForJson = new GameForJson();
        gameForJson.setStart(LocalDateTime.of(2023,12,23,0,0));
        gameForJson.setName("Example Game GS75");
        gameForJson.setType("GS75");
        GameListCreator gameListCreator = new GameListCreator();
        assertEquals(true,gameListCreator.isBigGameType(gameForJson));
    }

    @Test
    void readBigGameDatesFromConfigShouldWork() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        List<LocalDateTime> bigGameDates = gameListCreator.readBigGameDates();
        LocalDateTime actualDate = bigGameDates.get(0);
        LocalDateTime expectedDate = LocalDateTime.of(2023,12,23,0,0);
        assertEquals(expectedDate, actualDate);
    }

    @Test
    void bigGameTypesFromConfigShouldBeTree() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        List<String> expectedBigGameTypes = gameListCreator.readBigGameTypes();
        assertEquals(3, expectedBigGameTypes.size());
    }

    @Test
    void firstBigGameTypesFromConfigShouldBeV75() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        List<String> expectedBigGameTypes = gameListCreator.readBigGameTypes();
        assertEquals("V75", expectedBigGameTypes.get(0));
    }

    @Test
    void readCommaSeperatedFileShouldNotThrowError() throws IOException {
        GameListCreator gameListCreator = new GameListCreator();
        assertDoesNotThrow(() -> gameListCreator.readCommaSeperatedFile("src/main/resources/bigGameTypes.csv"));
    }
}