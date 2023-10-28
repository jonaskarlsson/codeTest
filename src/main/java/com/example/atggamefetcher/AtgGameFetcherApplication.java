package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.GameForJson;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@SpringBootApplication
@RestController
public class AtgGameFetcherApplication {

    public AtgGameFetcherApplication() {

        GameSorter gameSorter = new GameSorter();
        gameSorter.getGamesForToday(new ArrayList<>());
    }

    public static void main(String[] args) {
        SpringApplication.run(AtgGameFetcherApplication.class, args);
    }

    @GetMapping("/hello")
    public String sayHello(@RequestParam(value = "myName", defaultValue = "World") String name) {
        return String.format("Hello %s!", name);
    }

    @RequestMapping(value = "/games", method = RequestMethod.POST)
    public String postGames(@RequestBody List<GameForJson> games) throws IOException {

        String json = null;

        List<Game> gamesForToday;
        GameSorter gameSorter = new GameSorter();

        GameListCreator gameListCreator = new GameListCreator();

        List<Game> gamesWithBigGames = gameListCreator.getGamesIncludingBigGames(games);

        gamesForToday = gameSorter.getGamesForToday(gamesWithBigGames);
        ObjectMapper objectMapper = new ObjectMapper();
        // Needed to handle JSR310 Java 8 Date/Time API
        objectMapper.registerModule(new JavaTimeModule());
        try {
            json = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(gamesForToday);
        } catch (Exception e) {
            // Would normally log this and handle it using a logger
            e.printStackTrace();
        }

        return json;
    }


}
