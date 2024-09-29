import java.io.*;
import java.util.*;

public class ExtractScript {
	public static String stringPool = "";
	public static Map<String, String> translated = new HashMap<>();
	
	private static String fixCase(String str) {
		String out="";
		boolean shouldUpperCase = true;
		for (int i=0; i<str.length(); i++) {
			char ch = str.charAt(i);
			if (ch == '.')
				shouldUpperCase = true;
			if (shouldUpperCase && Character.isLetter(ch)) {
				out += Character.toUpperCase(ch);
				shouldUpperCase = false;
				continue;
			}
			out += ch;
		}
		return out;
	}

	public static void extract(PrintStream out, int levelIndex, Inf inf) throws IOException {
		HashMap<Integer, String> address2Label = new HashMap<Integer, String>();
		HashMap<Integer, Trigger> address2Trigger = new HashMap<Integer, Trigger>();

		AsmUtil.dumpName(out, String.format("lvl%dTriggers", levelIndex));
		int i=0;
		for (Trigger trigger : inf.script.triggers) {
			int address = trigger.address - inf.scriptOffset;
			String label = String.format("_%d_%d", levelIndex,address);
			i++;
			address2Label.put(address, label);
			address2Trigger.put(address, trigger);
			out.printf("\t.byte <$%04x, >$%04x, $%02x, <%s, >%s ;[%d,%d]\n", trigger.rawPos, trigger.rawPos, trigger.flags, label, label, trigger.pos.x, trigger.pos.y);
		}
		out.printf("\t.byte $ff,$ff\n");

		// Emit all the script data
		Trigger refTrigger=null;
		int lastRefX = -1;
		int lastRefY = -1;
		for (AbstractToken token : inf.script.tokens) {		
			Trigger trigger = address2Trigger.get(token.offset);
			if (trigger!=null) {
				refTrigger = trigger;	
				lastRefX = refTrigger.pos.x;
				lastRefY = refTrigger.pos.y;		
				out.printf("; Referenced by trigger! Pos:[%d,%d] Flags:%%", refTrigger.pos.x, refTrigger.pos.y);
				for (int b=7;b>=0;b--) {
					int v = ((1<<b)&refTrigger.flags)==0?0:1;
					out.printf("%d",v);
				}
				out.println();
			}
			out.printf("; %s\n", token.getClass().getName());

			AsmUtil.dumpLocalName(out,String.format("_%d_%d", levelIndex,token.offset));

			// Optimize MessageTokens using a StringPool
			if (token instanceof MessageToken) {
				int vgaColors[]={0,6,5,3,2,4,9,0xf,0xb,0xe,0xd,3,0xa,4,7,1};
	
				MessageToken mt = (MessageToken)token;
				String message = fixCase(mt.message);
				String m2 = translated.get(message);
				if (m2 != null) {
					message = m2;
				}
				int messageOffset = stringPool.indexOf(message);
				if (messageOffset==-1) {
					messageOffset = stringPool.length();
					
					stringPool += message;
				}
				out.printf(".byte $%02x, $%02x, <(messagePool+$%04x), >(messagePool+$%04x), $%02x ;\"%s\"\n", AbstractToken.TOKEN_MESSAGE, message.length(), messageOffset, messageOffset, vgaColors[mt.color], message);
			}

			// Resolve address of JumpTokens
			else if (token instanceof JumpToken) {
				JumpToken jt = (JumpToken)token;
				out.printf("\t.byte $%02x, <_%d_%d, >_%d_%d\n", AbstractToken.TOKEN_JUMP, levelIndex, jt.address- inf.scriptOffset, levelIndex, jt.address- inf.scriptOffset);
			}

			// Resolve address of CallToken
			else if (token instanceof CallToken) {
				CallToken jt = (CallToken)token;
				out.printf("\t.byte $%02x, <_%d_%d, >_%d_%d\n", AbstractToken.TOKEN_CALL, levelIndex, jt.address- inf.scriptOffset, levelIndex, jt.address- inf.scriptOffset);
			}

			// Resolve address of ConditionalTokens
			else if (token instanceof ConditionalToken) {
				ConditionalToken ct = (ConditionalToken)token;
				byte[] data = new byte[ct.rawData.length-2];
				System.arraycopy(ct.rawData, 0, data, 0, data.length);
				AsmUtil.dumpByteArray(out, null, data);
				out.printf("\t.word _%d_%d\n", levelIndex, ct.falseAddress - inf.scriptOffset);
			}

			// Highlight encounters
			else if (token instanceof EncounterToken) {
				EncounterToken et = (EncounterToken)token;
				out.printf("\t;Encounter $%02x at %d,%d on level %d\n", et.index, lastRefX, lastRefY, levelIndex);
		 		AsmUtil.dumpByteArray(out, null, token.rawData);				
			}
			else
		 		AsmUtil.dumpByteArray(out, null, token.rawData);
	 	}
	}
	
	static {
		translated.put("Going down...","Gehe runter...");
		translated.put("You can't go that way.","Hier geht's nicht weiter.");
		translated.put("It smells terrible here.","Es riecht furchtbar hier.");
		translated.put("This slimy, smelly drain pipe reveals nothing.","Dieses schleimige, stinkende Abflussrohr l�sst nichts erkennen.");
		translated.put("There is a drainage grate here.","Hier ist ein Abflussrost.");
		translated.put("Something scurries deeper into the floor drain.","Etwas huscht tiefer in den Abfluss im Boden.");
		translated.put("Kobold rune for 'entrance'.","Kobold-Rune f�r 'Eingang'.");
		translated.put("This rune is unrecognized.","Diese Rune ist unbekannt.");
		translated.put("Special q�st for this level!","Extraaufgabe auf dieser Ebene!");
		translated.put("Fallen rocks block the way.","Heruntergefallene Felsen blockieren den Weg.");
		translated.put("It d�sn't fit.","Das passt nicht.");
		translated.put("Failed lock pick attempt.","Das Schloss l�sst sich nicht knacken.");
		translated.put("The lock picks break!.","Der Dietrich bricht!");
		translated.put("Only a thief can pick locks.","Nur Diebe k�nnen Schl�sser knacken.");
		translated.put("This lock requires a key.","F�r dieses Schloss braucht man einen Schl�ssel.");
		translated.put("The lock appears jammed.","Das Schloss klemmt wohl.");
		translated.put("Going up...","Gehe hoch...");
		translated.put("The lock has been picked!","Das Schloss ist geknackt!");
		translated.put("Dead end?","Sackgasse?");
		translated.put("The dagger fits.","Der Dolch passt.");
		translated.put("What an odd carving to place here.","Was f�r eine seltsame Rune das ist.");
		translated.put("A hollow laughter ech�s faintly.","Dumpfes Gel�chter verhallt in der Ferne.");
		translated.put("A whispering voice says 'illusion, solid illusion'","Eine Stimme fl�stert: 'Illusion, solide Illusion'");
		translated.put("The door appears stuck.","Die t�r scheint zu klemmen.");
		translated.put("There seems to be movement inside this floor drain.","In dem Abfluss im Boden scheint sich etwas zu bewegen.");
		translated.put("Entry level.","Eingangsebene.");
		translated.put("Death section.","Todessektion.");
		translated.put("Correction facility.","Umerziehungseinrichtung.");
		translated.put("The room seems to move.","Der Raum scheint sich zu bewegen.");
		translated.put("This carving looks familiar.","Diese Rune sieht bekannt aus.");
		translated.put("This is an orc rune of passage.","Das ist eine orkische Passierrune.");
		translated.put("This looks like a 'travel' marker.","Das sieht wie eine Wegmarkierung aus.");
		translated.put("Not all is as it appears","Nicht alles ist wie es erscheint.");
		translated.put("Watch your step.","Geht vorsichtig.");
		translated.put("Only the strong shall pass.","Nur die Starken sollen passieren");
		translated.put("Chamber of the great warriors.","Kammer der gro�en Krieger.");
		translated.put("You feel dizzy.","Ihr f�hlt euch schwindelig.");
		translated.put("The key fits.","Der Schl�ssel passt.");
		translated.put("Stay thy hand at the first opportunity, for the second will yield greater reward.","Lass es bleiben bei der ersten Gelegenheit, so wird die Zweite h�her belohnt.");
		translated.put("This hole looks like the inside of a jewel box.","Dieses Loch sieht wie die Innenseite einer Schmuckschatulle aus.");
		translated.put("This hole is tightly sealed.","Dieses Loch ist fest versiegelt.");
		translated.put("Museum.","Museum.");
		translated.put("Riches are a small sacrifice for greater freedom.","Kostbarkeiten sind ein kleines Opfer f�r mehr Freiheit.");
		translated.put("What g�s around, comes around.","Wie man in den Wald ruft, so schallt es heraus.");
		translated.put("City of Waterdeep - rapid access transport system","RATS - Schnell");
		translated.put("Doors open, doors close.  Items come, items go.","T�ren �ffnen, T�ren schliessen.   Habe kommt, Habe geht.");
		translated.put("Dwarvish writing of some kind.","Irgendetwas in Zwergenschrift.");
		translated.put("Access control lever.","Zugangskontrollschalter.");
		translated.put("Emergency exit.","Notausgang.");
		translated.put("Dwarvish totem of treasure guarding.","Zwergentotems als Schatzwache.");
		translated.put("This is a dwarven kingdom marker.","Dies ist die Grenzmarke eines Zwergenk�nigreiches.");
		translated.put("Feeding instructions of some kind.","Irgendwelche F�tterungsanweisungen.");
		translated.put("Kr�n - king under the mountain.","Kr�n - K�nig unter dem Berg.");
		translated.put("Kr�n - the holder of wisdom.","Kr�n - Tr�ger der Weisheit.");
		translated.put("Kr�n - destroyer of the ancient one.","Kr�n - Zerst�rer des Alten Volkes.");
		translated.put("Kr�n - the fearless one.","Kr�n - der Furchtlose.");
		translated.put("There is evil that lies beyond this room.","Das B�se lauert hinter diesem Raum.");
		translated.put("Oracle of knowledge.","Orakel des Wissens.");
		translated.put("You've made it this far. Good luck.","Bis hierher habt ihr es geschafft. Viel Gl�ck.");
		translated.put("Things are not always as they appear","Die Dinge sind nicht immer wie sie scheinen.");
		translated.put("Please reset drain holes when finished.","Zum Schluss bitte die Abflussl�cher zur�cksetzen.");
		translated.put("Greed will be your downfall.","Gier wird euer Untergang sein.");
		translated.put("Dwarvish rune of 'safe passage'.","Zwergenrune f�r 'Sicherer Durchgang'.");
		translated.put("This is a dwarvish rune.","Dies ist eine Zwergenrune.");
		translated.put("Pantry","Speisekammer");
		translated.put("Looks like the dwarvish word for 'closet' or 'cupboard'.","Sieht wie das Zwergenwort f�r 'Schrank' oder 'Kabinett' aus.");
		translated.put("Store weapons before proceeding.","Vor dem Weitergehen, Waffen ablegen.");
		translated.put("Dwarvish writing saying 'fasten' and 'hammer'.","Zwergenschrift, die besagt 'Hammer' und 'Festschnallen'.");
		translated.put("Veritcal access shaft.","Vertikaler Zugangsschacht.");
		translated.put("Silverware rack... Be neat.","Besteckregal... seid ordentlich.");
		translated.put("Dart rack.","Wurfpfeilst�nder.");
		translated.put("Key taken.","Schl�ssel genommen.");
		translated.put("Round and round...","Immer rundherum...");
		translated.put("Nest","Nest");
		translated.put("This is the symbol of a dwarven tribe.","Das ist das Symbol eines Zwergenstammes.");
		translated.put("A firing mechanism of some sort.","Eine Art Abschussmechanismus.");
		translated.put("Nothing can be seen within the holes.","In den L�chern ist nichts zu sehen.");
		translated.put("Don't delay!","Nicht tr�deln!");
		translated.put("It looks like these holes are scorched.","Es sieht aus, als w�ren diese L�cher versengt.");
		translated.put("Test of courage.","Mutprobe.");
		translated.put("Treasure room.  No admittance.","Schatzkammer.  Kein Zutritt.");
		translated.put("One sacrifice made...","Ein geleistetes Opfer...");
		translated.put("Thank you for your donation.","Vielen Dank f�r ihre Spende.");
		translated.put("One leap of faith...","Ein Schritt des Glaubens...");
		translated.put("One battle for glory...","Eine Schlacht f�r die Ehre...");
		translated.put("Magic","Magie");
		translated.put("Armor","R�stung");
		translated.put("Weapon","Waffe");
		translated.put("No turning back.","Kein zur�ck.");
		translated.put("One way.","Einbahn.");
		translated.put("Fight for your freedom.","K�mpft f�r eure Freiheit.");
		translated.put("Use front entrance.  Exit only.","Vorderen Eingang benutzen. Nur Ausgang.");
		translated.put("No trespassing.","Kein Durchgang.");
		translated.put("These holes look scorched.","Diese L�cher sehen versengt aus.");
		translated.put("One gem for one key","Ein edelstein pro Schl�ssel");
		translated.put("One's faith repaid.","Jemandes ausbezahlter Glaube.");
		translated.put("Turn around.","Dreht um.");
		translated.put("You shouldn't have pressed the button.","Ihr h�ttet den Knopf nicht dr�cken sollen.");
		translated.put("Don't press the button.","Nicht den Knopf dr�cken.");
		translated.put("Only one choice allowed.","Nur eine Wahl erlaubt.");
		translated.put("Gaultlet of flame. Turn back!","Flammender Panzerhandschuh. Dreht um!");
		translated.put("Looks like a mounting device of some sort.","Sieht wie eine Art Steighilfe aus.");
		translated.put("Storage","Lager");
		translated.put("A glowing rock is firmly mounted into the wall.","Ein gl�hender Stein ist fest in der Wand verankert.");
		translated.put("The rock fits!","Der Stein passt!");
		translated.put("You need a special key.","Ihr ben�tigt einen speziellen Schl�ssel.");
		translated.put("Watch your head...","Passt auf eure K�pfe auf...");
		translated.put("Hall of thieves.","Halle der Diebe.");
		translated.put("You forgot something.","Ihr habt etwas vergessen.");
		translated.put("Thank you.","Danke sch�n.");
		translated.put("The cunning and agile shall survive.","Die Listigen und Agilen sollen �berleben.");
		translated.put("Combination lock -- be quick.","Kombinationsschloss -- Beeilung.");
		translated.put("Drow word for 'button'.","Dunkelelfenwort f�r 'Knopf'.");
		translated.put("Be generous.","Seid gro�z�gig.");
		translated.put("Drow writing of some kind.","Irgendwelche Dunkelelfenworte.");
		translated.put("Donate sword","Spendet ein Schwert");
		translated.put("Donate food","Spendet Essen");
		translated.put("Donate armor","Spendet R�stung");
		translated.put("Donate missile","Spendet ein Geschoss");
		translated.put("Oracle of devouring","Orakel der Eifersucht");
		translated.put("It is written, the key lies on the other side.","Es steht geschrieben, der Schl�ssel liegt auf der anderen Seite.");
		translated.put("Drow writing is written here.","Dunkelelfenworte stehen hier.");
		translated.put("Stow yer weapons.","Verstaut eure Waffen.");
		translated.put("Do not disturb the hives.","St�rt den Stock nicht.");
		translated.put("Hive","Stock");
		translated.put("Proper seq�nce","Korrekte Reihenfolge.");
		translated.put("Thirteen","Dreizehn");
		translated.put("Look behind you!","Schaut hinter euch!");
		translated.put("Keep an eye out for trouble.","Seid auf der Hut vor �rger.");
		translated.put("Welcome","Willkommen");
		translated.put("In case of fire...","Im Falle eines Feuers...");
		translated.put("In case of flood...","Im Falle einer �berflutung...");
		translated.put("Jump...","Springt...");
		translated.put("This is a celestial star of navigation.","Dies ist ein himmlischer Stern zur Navigation.");
		translated.put("Your fate lies in the stars.","Euer Schicksal liegt in den Sternen.");
		translated.put("Leave no stone unturned.","Dreht jeden Stein um.");
		translated.put("Alignment must be tr�.","Eure Einstellung muss stimmen.");
		translated.put("Chwat'","Chwat'");
		translated.put("Room of the key.","Raum des Schl�ssels.");
		translated.put("This hole reeks faintly of smoke.","Dies Loch riecht leicht nach Rauch.");
		translated.put("Turn back, no tresspassing.","Dreht um! Kein Durchgang!");
		translated.put("You were warned.","Ihr wurdet gewarnt.");
		translated.put("You get spiked!","Ihr werdet durch Spie�e verletzt!");
		translated.put("Light beam has been broken.","Eine Lichtschranke wurde unterbrochen.");
		translated.put("Room of the spheres.","Raum der Kugeln.");
		translated.put("Stone for substance.","Stein f�r Substanz.");
		translated.put("Potion for strength.","Trank f�r St�rke.");
		translated.put("Sphere for animation.","Kugel f�r Bewegung.");
	}
}
