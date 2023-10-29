package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.GameForJson;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class AtgGameFetcherApplicationTest {

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
    void sayHello() {
        AtgGameFetcherApplication atgGameFetcherApplication = new AtgGameFetcherApplication();
        assertEquals("Hello friend!", atgGameFetcherApplication.sayHello("friend"));
    }

    @Test
    void postGames() {
        AtgGameFetcherApplication atgGameFetcherApplication = new AtgGameFetcherApplication();
        assertDoesNotThrow(() -> atgGameFetcherApplication.postGames(gameList));
    }
}