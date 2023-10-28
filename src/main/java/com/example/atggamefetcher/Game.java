package com.example.atggamefetcher;

import java.time.LocalDateTime;
import java.util.Objects;

public class Game implements Comparable<Game> {
    private String name;
    private String type;
    private LocalDateTime start;
    private final Boolean bigGame;

    public Game(String name, String type, LocalDateTime start, Boolean bigGame) {
        this.name = name;
        this.type = type;
        this.start = start;
        this.bigGame = bigGame;
    }

    @Override
    public int compareTo(Game games) {
        return start.compareTo(games.start);
    }

    public LocalDateTime getStart() {
        return start;
    }

    public Boolean getBigGame() {
        return bigGame;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Game game = (Game) o;

        if (!Objects.equals(name, game.name)) return false;
        return Objects.equals(type, game.type);

    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        return result;
    }
}
