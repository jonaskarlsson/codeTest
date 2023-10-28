package com.example.atggamefetcher;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.util.ArrayList;

@SpringBootApplication
public class AtgGameFetcherApplication {

    public AtgGameFetcherApplication() {
        GameSorter gameSorter = new GameSorter();
        gameSorter.getGamesForToday(new ArrayList<>());
    }

    public static void main(String[] args) {
        SpringApplication.run(AtgGameFetcherApplication.class, args);
    }

}
