import java.io.*;
import java.net.Socket;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import processing.core.PApplet;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.util.logging.Logger;
import java.util.logging.Level;


public class Client extends PApplet {
    public static class Reader implements Runnable {
        Variables vars;

        public Reader(Variables vars) {
            this.vars = vars;
        }

        public void run() {
            try {
                while (true) {
                    String response = vars.in.readLine();
                    System.out.println("Received: " + response);
                    response = response.replace("\\\"", "\"");
                    response = response.replaceFirst("\"", "");
                    response = response.replaceFirst("\"$", "");
                    System.out.println(response);
                    InputStream inputStream = new ByteArrayInputStream(response.getBytes());

                    // Parse the XML
                    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder builder = factory.newDocumentBuilder();
                    Document document = builder.parse(inputStream);

                    // Normalize the document
                    document.getDocumentElement().normalize();

                    // Get the root element
                    Element root = document.getDocumentElement();
                    System.out.println("Root element: " + root.getTagName());
                    switch (root.getTagName()) {
                        case "reply":
                            String replyText = root.getAttribute("text");
                            System.out.println("Reply: " + replyText);
                            switch (replyText) {
                                case "Login successful":
                                    System.out.println("Login successful");
                                    vars.out.println("/Lv");
                                    vars.out.flush();
                                    break;
                                case "Account created successfully", "Account closed successfully",
                                     "Logged out successfully":
                                    vars.currentScene = "Menu";
                                    vars.username = "";
                                    vars.password = "";
                                    break;
                                case "Username already exists":
                                    vars.username = "";
                                    vars.password = "";
                                    System.out.println("Username already exists");
                                    break;
                                case "Account closure failed":
                                    vars.username = "";
                                    vars.password = "";
                                    System.out.println("Account closure failed");
                                    break;
                                case "Searching for a match...":
                                    vars.searching = true;
                                    break;
                                case "Match found!":
                                    System.out.println("Match found!2");
                                    vars.searching = false;
                                    vars.currentScene = "GamePage";
                                    break;
                                case "Invalid command":
                                    System.out.println("Invalid command");
                                    break;
                                default:
                                    System.out.println(replyText);
                                    break;
                            }
                            break;
                        case "checkLV":
                            String level = root.getAttribute("level");
                            vars.Lvl = level;
                            vars.currentScene = "MatchPage";
                            System.out.println("Level: " + level);
                        case "gamedata":
                            NodeList players = document.getElementsByTagName("player1");
                            if (players.getLength() > 0) {
                                Element player1 = (Element) players.item(0);
                                vars.px1 = Float.parseFloat(player1.getAttribute("x"));
                                vars.py1 = Float.parseFloat(player1.getAttribute("y"));
                            }

                            players = document.getElementsByTagName("player2");
                            if (players.getLength() > 0) {
                                Element player2 = (Element) players.item(0);
                                vars.px2 = Float.parseFloat(player2.getAttribute("x"));
                                vars.py2 = Float.parseFloat(player2.getAttribute("y"));
                            }
                            System.out.println("Player1: (" + vars.px1 + ", " + vars.py1 + ")");
                            System.out.println("Player2: (" + vars.px2 + ", " + vars.py2 + ")");
                            break;
                        default:
                            break;

                    }
                }
            } catch (IOException e) {
                logger.log(Level.SEVERE, "Erro ao ler do servidor", e);
            } catch (ParserConfigurationException | SAXException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public static class Variables {
        String currentScene;
        String username;
        String password;
        boolean typingUsername;
        boolean typingPassword;
        boolean ignoreFirstClick;
        boolean searching;
        Socket socket;
        PrintWriter out;
        BufferedReader in;
        String Lvl;
        float px1, py1;
        float px2, py2;

        public Variables() {
            this.currentScene = "Menu";
            this.username = "";
            this.password = "";
            this.typingUsername = true;
            this.typingPassword = false;
            this.ignoreFirstClick = false;
            this.searching = false;
            this.Lvl = "";
            this.px1 = 0;
            this.py1 = 0;
            this.px2 = 0;
            this.py2 = 0;
            try {
                this.socket = new Socket("127.0.0.1", Integer.parseInt("8000"));
                this.in = new BufferedReader(new InputStreamReader(this.socket.getInputStream()));
                this.out = new PrintWriter(this.socket.getOutputStream(), true);

            } catch (IOException e) {
                logger.log(Level.SEVERE, "Erro na comunicação com o servidor", e);
            }
        }
    }

    Variables vars = new Variables();

    public void settings() {
        size(800, 800);
    }

    public void setup() {
        smooth();
        textSize(16);
        textAlign(CENTER, CENTER);
        Thread readerThread = new Thread(new Reader(vars));
        readerThread.start();
    }

    public void draw() {
        background(0);

        switch (vars.currentScene) {
            case "Login":
                drawLogin();
                break;
            case "CreateAccount":
                drawCreateAccount();
                break;
            case "MatchPage":
                drawMatchPage();
                break;
            case "GamePage":
                drawGamePage();
                break;

            default:
                drawMenu();
                break;
        }
    }

    private void drawMenu() {
        fill(255);
        textSize(32);
        text("Menu", (float) width / 2, (float) height / 2 - 70);
        textSize(16);

        // Botão de Login
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 - 20, 100, 30, 10);
        fill(0);
        text("Login", (float) width / 2, (float) height / 2 - 6);

        // Botão de Criar Conta
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 40, 100, 30, 10);
        fill(0);
        text("Criar Conta", (float) width / 2, (float) height / 2 + 55);
    }

    private void drawLogin() {
        fill(255);
        textSize(32);
        text("Login", (float) width / 2, (float) height / 2 - 130);
        textSize(16);

        // Caixa de username
        fill(vars.typingUsername ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 70, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        text(vars.username, (float) width / 2 - 95, (float) height / 2 - 55);
        textAlign(CENTER, CENTER);

        // Label para username
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Username:", (float) width / 2 - 100, (float) height / 2 - 75);
        textAlign(CENTER, CENTER);

        // Caixa de password
        fill(vars.typingPassword ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 10, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        // Mostrar asteriscos para a senha
        text("*".repeat(vars.password.length()), (float) width / 2 - 95, (float) height / 2 + 5);
        textAlign(CENTER, CENTER);

        // Label para password
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Password:", (float) width / 2 - 100, (float) height / 2 - 15);
        textAlign(CENTER, CENTER);

        // Botão de login
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 40, 100, 30, 10);
        fill(0);
        text("Login", (float) width / 2, (float) height / 2 + 55);

        // Botão de voltar
        fill(200, 100, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 80, 100, 30, 10);
        fill(0);
        text("Voltar", (float) width / 2, (float) height / 2 + 95);


    }

    private void drawCreateAccount() {
        fill(255);
        textSize(32);
        text("Criar Conta", (float) width / 2, (float) height / 2 - 130);
        textSize(16);

        // Caixa de username
        fill(vars.typingUsername ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 70, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        text(vars.username, (float) width / 2 - 95, (float) height / 2 - 55);
        textAlign(CENTER, CENTER);

        // Label para username
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Username:", (float) width / 2 - 100, (float) height / 2 - 75);
        textAlign(CENTER, CENTER);

        // Caixa de password
        fill(vars.typingPassword ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 10, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        // Mostrar asteriscos para a senha
        text("*".repeat(vars.password.length()), (float) width / 2 - 95, (float) height / 2 + 5);
        textAlign(CENTER, CENTER);

        // Label para password
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Password:", (float) width / 2 - 100, (float) height / 2 - 15);
        textAlign(CENTER, CENTER);

        // Botão de criar conta
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 40, 100, 30, 10);
        fill(0);
        text("Criar Conta", (float) width / 2, (float) height / 2 + 55);

        // Botão de voltar
        fill(200, 100, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 80, 100, 30, 10);
        fill(0);
        text("Voltar", (float) width / 2, (float) height / 2 + 95);

    }

    private void drawMatchPage() {
        fill(0);
        textSize(32);
        fill(255);
        text("User: " + vars.username, (float) width / 2, (float) height / 2 - 100);

        textSize(16);
        text("Level: " + vars.Lvl, (float) width / 2, (float) height / 2 - 60);
        text("Vitorias: 0", (float) width / 2, (float) height / 2 - 30);
        text("Derrotas: 0", (float) width / 2, (float) height / 2);
        text("Streak de vitorias : 0", (float) width / 2, (float) height / 2 + 30);

        // Botão de Logout
        fill(200, 100, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 95, 100, 30, 10);
        fill(0);
        text("Logout", (float) width / 2, (float) height / 2 + 110);

        // Botão de Match
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 55, 100, 30, 10);
        fill(0);
        if (!vars.searching) {
            text("Match", (float) width / 2, (float) height / 2 + 70);
        } else {
            text("Searching", (float) width / 2, (float) height / 2 + 70);
        }
    }

    private void drawGamePage(){
        fill(255);
        ellipse(vars.px1, vars.py1, 50,50);
        ellipse(vars.px2 + 1, vars.py2, 50,50);
    }


    public void mousePressed() {
        if (vars.currentScene.equals("Menu")) {
            // Verifica clique no botão "Login"
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 - 20 && mouseY < height / 2 + 10) {
                vars.currentScene = "Login";
                vars.typingUsername = false;
                vars.typingPassword = false;
                vars.ignoreFirstClick = true;
            }

            // Verifica clique no botão "Criar Conta"
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                vars.currentScene = "CreateAccount";
                vars.typingUsername = false;
                vars.typingPassword = false;
                vars.ignoreFirstClick = true;
            }
        }

        if (vars.currentScene.equals("Login")) {
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false; // Ignora o clique inicial
                return;
            }
            vars.typingUsername = false;
            vars.typingPassword = false;
            // Botão voltar
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 80 && mouseY < height / 2 + 110) {
                vars.currentScene = "Menu";
                vars.username = "";
                vars.password = "";
            }
            // Botão login
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                // Lógica de login aqui
                if (!vars.username.isEmpty() && !vars.password.isEmpty()) {
                    vars.out.println("/l " + vars.username + " " + vars.password);
                    vars.out.flush();
                }
            }
            // Caixa de username
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 120 &&
                    mouseY > height / 2 - 60 && mouseY < height / 2 - 30) {
                vars.typingUsername = true;
                println("Caixa de username ativada");
            }
            // Caixa de password
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 120 &&
                    mouseY > height / 2 - 10 && mouseY < height / 2 + 20) {
                vars.typingPassword = true;
                println("Caixa de password ativada");
            }
        }

        if (vars.currentScene.equals("CreateAccount")) {
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false; // Ignora o clique inicial
                return;
            }
            vars.typingUsername = false;
            vars.typingPassword = false;
            // Botão voltar
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 80 && mouseY < height / 2 + 110) {
                vars.currentScene = "Menu";
                vars.username = "";
                vars.password = "";
            }
            // Botão criar conta
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                if (!vars.username.isEmpty() && !vars.password.isEmpty()) {
                    vars.out.println("/cr " + vars.username + " " + vars.password);
                    vars.out.flush();
                }
            }
            // Caixa de username
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 100 &&
                    mouseY > height / 2 - 60 && mouseY < height / 2 - 30) {
                vars.typingUsername = true;
                println("Caixa de username ativada");
            }
            // Caixa de password
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 100 &&
                    mouseY > height / 2 - 10 && mouseY < height / 2 + 20) {
                vars.typingPassword = true;
                println("Caixa de password ativada");
            }
        }

        if (vars.currentScene.equals("MatchPage")) {
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false; // Ignora o clique inicial
                return;
            }
            // Botão de Logout
            if (mouseX > width / 2 - 50  && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 95 && mouseY < height / 2 + 125) {
                vars.out.println("/q");
                vars.out.flush();
            }
            // Botão de Match
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 55 && mouseY < height / 2 + 85) {
                System.out.println("Botão Match clicado");
                vars.out.println("/f");
                vars.out.flush();
            }
        }
    }

    public void keyPressed() {
        if (vars.currentScene.equals("Login") || vars.currentScene.equals("CreateAccount")) {
            if (vars.typingUsername) {
                if (key == BACKSPACE && !vars.username.isEmpty()) {
                    vars.username = vars.username.substring(0, vars.username.length() - 1);
                } else if (key != BACKSPACE && key != ENTER && key != TAB && key != CODED) {
                    vars.username += key;
                } else if (key == TAB) {
                    vars.typingUsername = false;
                    vars.typingPassword = true;
                }
                println("Username: " + vars.username);
            } else {
                if (key == BACKSPACE && !vars.password.isEmpty()) {
                    vars.password = vars.password.substring(0, vars.password.length() - 1);
                } else if (key != BACKSPACE && key != ENTER && key != TAB && key != CODED) {
                    vars.password += key;
                } else if (key == TAB) {
                    vars.typingPassword = false;
                    vars.typingUsername = true;
                }
                println("Password: " + vars.password);
            }
        }
    }

    private static final Logger logger = Logger.getLogger(Client.class.getName());

    public static void main(String[] args) {
        PApplet.main("Client");
    }
}