// Copyright AB Trav och Galopp (556180-4161)
package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.Game;
import com.example.atggamefetcher.pojo.GameForJson;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class GameListCreator {

    private List<Game> todayGames;
    private final List<LocalDateTime> bigGameDates;
    private final List<String> bigGameTypes;

    public GameListCreator() throws IOException {
        todayGames = new ArrayList<>();
        bigGameDates = readBigGameDates();
        bigGameTypes = readBigGameTypes();
    }

    public List<Game> getTodayGames() {
        return todayGames;
    }

    public List<Game> getGamesIncludingBigGames(List<GameForJson> gameForJsons) {
        todayGames = new ArrayList<>();
        for (GameForJson game : gameForJsons) {
            if (Boolean.TRUE.equals(isBigGameDay(game))) {
                addGameToTodayGames(game, true);
            } else if (Boolean.TRUE.equals(isBigGameType(game))) {
                if (Boolean.TRUE.equals(isBigGameV86(game))) {
                    addGameToTodayGames(game, true);
                } else if (Boolean.TRUE.equals(isBigGameV75(game))) {
                    addGameToTodayGames(game, true);
                } else addGameToTodayGames(game, Boolean.TRUE.equals(isBigGameGS75(game)));
            } else {
                addGameToTodayGames(game, false);
            }
        }
        return todayGames;
    }

    public void addGameToTodayGames(GameForJson gameForJson, Boolean bigGame) {
        todayGames.add(new Game(gameForJson.getName(), gameForJson.getType(), gameForJson.getStart(), bigGame));
    }

    public Boolean isBigGameV86(GameForJson game) {
        return game.getType().equalsIgnoreCase("V86") && game.getStart().getDayOfWeek() == DayOfWeek.WEDNESDAY;
    }

    public Boolean isBigGameV75(GameForJson game) {
        return game.getType().equalsIgnoreCase("V75") && game.getStart().getDayOfWeek() == DayOfWeek.SATURDAY;
    }

    public Boolean isBigGameGS75(GameForJson game) {
        return game.getType().equalsIgnoreCase("GS75") && game.getStart().getDayOfWeek() == DayOfWeek.SUNDAY;
    }

    public Boolean isBigGameDay(GameForJson game) {
        GameForJson gameForJson = new GameForJson();
        gameForJson.setName(game.getName());
        gameForJson.setType(game.getType());
        gameForJson.setStart(game.getStart().toLocalDate().atStartOfDay());
        return bigGameDates.contains(gameForJson.getStart());
    }

    public Boolean isBigGameType(GameForJson game) {
        return bigGameTypes.contains(game.getType());
    }

    public List<LocalDateTime> readBigGameDates() throws IOException {
        String[] bigGameDatesFromConfig = readCommaSeperatedFile("src/main/resources/bigGameDates.csv");
        List<LocalDateTime> bigGameDatesList = new ArrayList<>();
        for (String bigGameDate : bigGameDatesFromConfig) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("uuuu-MM-dd");
            LocalDateTime date = LocalDate.parse(bigGameDate, formatter).atStartOfDay();
            bigGameDatesList.add(date);
        }
        return bigGameDatesList;
    }

    public List<String> readBigGameTypes() throws IOException {
        String[] bigGameTypesFromConfig = readCommaSeperatedFile("src/main/resources/bigGameTypes.csv");
        List<String> bigGameTypesList = new ArrayList<>();
        Collections.addAll(bigGameTypesList, bigGameTypesFromConfig);
        return bigGameTypesList;
    }

    public String[] readCommaSeperatedFile(String path) throws IOException {
        Path filePath = Path.of(path);
        String content = Files.readString(filePath);
        return content.split(",");
    }
}