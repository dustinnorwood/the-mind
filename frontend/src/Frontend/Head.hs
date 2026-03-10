{-# LANGUAGE OverloadedStrings #-}
module Frontend.Head (pageHead) where

import Data.Text (Text)
import Reflex.Dom

pageHead :: DomBuilder t m => m ()
pageHead = do
  el "title" $ text "The Mind"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  -- Google Font: Creepster for card numbers
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css2?family=Creepster&family=Nosifer&family=Montserrat:wght@400;600;700&display=swap") blank
  elAttr "link" ("rel" =: "icon" <> "type" =: "image/svg+xml" <> "href" =: faviconDataUri) blank
  el "style" $ text css

css :: Text
css = "\
  \* { box-sizing: border-box; margin: 0; padding: 0; }\n\
  \:root {\n\
  \  --bg: #0b0e1a;\n\
  \  --bg2: #12162b;\n\
  \  --bg3: #1a1f3a;\n\
  \  --card-bg: linear-gradient(145deg, #1e0a0a 0%, #3a0a0a 40%, #2a0505 100%);\n\
  \  --card-border: #6b1a1a;\n\
  \  --card-glow: rgba(180,40,40,0.4);\n\
  \  --gold: #d4a834;\n\
  \  --gold-dim: #8a6e22;\n\
  \  --red: #c0392b;\n\
  \  --green: #27ae60;\n\
  \  --blue: #2e86de;\n\
  \  --text: #d4d4e8;\n\
  \  --text2: #8888aa;\n\
  \  --text3: #555570;\n\
  \  --border: rgba(255,255,255,0.06);\n\
  \}\n\
  \html, body { height: 100%; overflow: hidden; }\n\
  \body {\n\
  \  background: var(--bg);\n\
  \  color: var(--text);\n\
  \  font-family: 'Montserrat', sans-serif;\n\
  \  display: flex; flex-direction: column;\n\
  \}\n\
  \\n\
  \/* ===== LOGIN ===== */\n\
  \.login-screen {\n\
  \  display: flex; align-items: center; justify-content: center;\n\
  \  height: 100vh;\n\
  \  background: radial-gradient(ellipse at 50% 30%, #1a1040 0%, var(--bg) 70%);\n\
  \}\n\
  \.login-card {\n\
  \  background: var(--bg2); border: 1px solid var(--border);\n\
  \  border-radius: 16px; padding: 48px 40px;\n\
  \  width: 100%; max-width: 400px;\n\
  \  box-shadow: 0 20px 60px rgba(0,0,0,0.5);\n\
  \}\n\
  \.login-title {\n\
  \  font-family: 'Nosifer', cursive;\n\
  \  font-size: 2.2rem; text-align: center;\n\
  \  color: var(--gold); margin-bottom: 4px;\n\
  \  text-shadow: 0 0 20px rgba(212,168,52,0.3);\n\
  \}\n\
  \.login-subtitle {\n\
  \  text-align: center; color: var(--text2);\n\
  \  font-size: 0.85rem; margin-bottom: 28px;\n\
  \}\n\
  \.input-group { margin-bottom: 14px; }\n\
  \.input-label {\n\
  \  display: block; font-size: 0.75rem; color: var(--text2);\n\
  \  text-transform: uppercase; letter-spacing: 1px; margin-bottom: 4px;\n\
  \}\n\
  \.game-input {\n\
  \  width: 100%; padding: 10px 14px;\n\
  \  background: var(--bg3); border: 1px solid var(--border);\n\
  \  border-radius: 6px; color: var(--text);\n\
  \  font-family: inherit; font-size: 0.95rem; outline: none;\n\
  \}\n\
  \.game-input:focus { border-color: var(--blue); }\n\
  \.game-input::placeholder { color: var(--text3); }\n\
  \.button-group { display: flex; gap: 10px; margin-top: 20px; }\n\
  \\n\
  \/* ===== BUTTONS ===== */\n\
  \.btn {\n\
  \  padding: 10px 20px; border: none; border-radius: 6px;\n\
  \  font-family: inherit; font-weight: 600; cursor: pointer;\n\
  \  transition: all 0.15s; text-transform: uppercase;\n\
  \  letter-spacing: 1px; font-size: 0.85rem;\n\
  \}\n\
  \.btn-primary {\n\
  \  background: var(--blue); color: white; flex: 1;\n\
  \}\n\
  \.btn-primary:hover { filter: brightness(1.2); }\n\
  \.btn-secondary {\n\
  \  background: transparent; color: var(--blue);\n\
  \  border: 1px solid var(--blue); flex: 1;\n\
  \}\n\
  \.btn-secondary:hover { background: rgba(46,134,222,0.1); }\n\
  \.btn-large { padding: 14px 36px; font-size: 1rem; }\n\
  \.btn-play {\n\
  \  background: var(--green); color: white;\n\
  \  padding: 12px 28px; font-size: 1rem;\n\
  \}\n\
  \.btn-play:hover { filter: brightness(1.15); transform: translateY(-1px); }\n\
  \.btn-play.btn-disabled { opacity: 0.35; cursor: not-allowed; transform: none; }\n\
  \.btn-star {\n\
  \  background: var(--gold-dim); color: white;\n\
  \  padding: 12px 20px; font-size: 0.95rem; margin-left: 10px;\n\
  \}\n\
  \.btn-star:hover { filter: brightness(1.15); }\n\
  \.btn-send {\n\
  \  background: var(--blue); color: white; padding: 8px 14px;\n\
  \  border: none; border-radius: 6px; cursor: pointer; font-family: inherit; font-weight: 600;\n\
  \}\n\
  \\n\
  \/* ===== APP LAYOUT ===== */\n\
  \.app-layout { display: flex; height: 100vh; overflow: hidden; }\n\
  \.game-area { flex: 1; overflow-y: auto; padding: 16px 24px; padding-bottom: 200px; }\n\
  \\n\
  \/* ===== ROOM LOBBY ===== */\n\
  \.room-lobby {\n\
  \  display: flex; flex-direction: column;\n\
  \  align-items: center; justify-content: center; min-height: 80vh;\n\
  \}\n\
  \.room-title { font-size: 1.6rem; color: var(--gold); margin-bottom: 20px; }\n\
  \.section-title {\n\
  \  color: var(--text2); font-size: 0.8rem;\n\
  \  text-transform: uppercase; letter-spacing: 2px; margin-bottom: 10px;\n\
  \}\n\
  \.players-list {\n\
  \  background: var(--bg2); border: 1px solid var(--border);\n\
  \  border-radius: 12px; padding: 20px; margin-bottom: 20px; min-width: 280px;\n\
  \}\n\
  \.player-badge {\n\
  \  display: flex; align-items: center; gap: 10px;\n\
  \  padding: 8px 14px; background: var(--bg3);\n\
  \  border-radius: 6px; margin-bottom: 6px; font-weight: 500;\n\
  \}\n\
  \.player-avatar {\n\
  \  width: 32px; height: 32px; border-radius: 50%;\n\
  \  background: linear-gradient(135deg, #7c3aed, var(--blue));\n\
  \  display: flex; align-items: center; justify-content: center;\n\
  \  font-weight: 700; text-transform: uppercase;\n\
  \}\n\
  \.room-actions { text-align: center; }\n\
  \\n\
  \/* ===== GAME BOARD ===== */\n\
  \.game-board { max-width: 860px; margin: 0 auto; }\n\
  \.game-header {\n\
  \  display: flex; align-items: center; justify-content: space-between;\n\
  \  padding: 12px 0; border-bottom: 1px solid var(--border); margin-bottom: 20px;\n\
  \}\n\
  \.game-title {\n\
  \  font-family: 'Nosifer', cursive; font-size: 1.2rem;\n\
  \  color: var(--gold); letter-spacing: 3px;\n\
  \  text-shadow: 0 0 12px rgba(212,168,52,0.25);\n\
  \}\n\
  \.game-stats { display: flex; gap: 20px; }\n\
  \.stat { display: flex; flex-direction: column; align-items: center; }\n\
  \.stat-label {\n\
  \  font-size: 0.65rem; color: var(--text3);\n\
  \  text-transform: uppercase; letter-spacing: 1px; margin-bottom: 2px;\n\
  \}\n\
  \.stat-value { font-size: 1.1rem; font-weight: 700; }\n\
  \\n\
  \/* ===== NOTIFICATIONS ===== */\n\
  \.notification {\n\
  \  padding: 10px 16px; border-radius: 6px; margin-bottom: 14px;\n\
  \  font-weight: 500; animation: notifIn 0.3s ease;\n\
  \}\n\
  \.notification-error {\n\
  \  background: rgba(192,57,43,0.15); border: 1px solid var(--red); color: #e87468;\n\
  \}\n\
  \@keyframes notifIn {\n\
  \  from { transform: translateY(-8px); opacity: 0; }\n\
  \  to { transform: translateY(0); opacity: 1; }\n\
  \}\n\
  \\n\
  \/* ===== OVERLAYS ===== */\n\
  \.overlay {\n\
  \  position: fixed; inset: 0;\n\
  \  display: flex; flex-direction: column; align-items: center; justify-content: center;\n\
  \  z-index: 500; animation: fadeIn 0.5s ease;\n\
  \}\n\
  \.overlay-gameover { background: rgba(8,10,20,0.93); }\n\
  \.overlay-win { background: rgba(8,10,20,0.93); }\n\
  \.overlay-title { font-family: 'Nosifer', cursive; font-size: 2.8rem; margin-bottom: 12px; }\n\
  \.overlay-gameover .overlay-title { color: var(--red); text-shadow: 0 0 30px rgba(192,57,43,0.5); }\n\
  \.overlay-win .overlay-title { color: var(--gold); text-shadow: 0 0 30px rgba(212,168,52,0.5); }\n\
  \.overlay-text { color: var(--text2); font-size: 1.1rem; }\n\
  \@keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }\n\
  \\n\
  \/* ===== CARDS ===== */\n\
  \.card {\n\
  \  display: inline-flex; align-items: center; justify-content: center;\n\
  \  border-radius: 10px; font-weight: 700;\n\
  \  transition: all 0.18s; user-select: none;\n\
  \  background: var(--card-bg);\n\
  \  border: 2px solid var(--card-border);\n\
  \  box-shadow: 0 4px 16px rgba(0,0,0,0.5), inset 0 1px 0 rgba(255,255,255,0.05);\n\
  \  position: relative;\n\
  \}\n\
  \.card-number {\n\
  \  font-family: 'Creepster', cursive;\n\
  \  color: #e8d0c0;\n\
  \  text-shadow: 0 2px 8px rgba(0,0,0,0.6), 0 0 20px rgba(180,40,40,0.3);\n\
  \}\n\
  \.card-primary {\n\
  \  width: 90px; height: 130px;\n\
  \}\n\
  \.card-primary .card-number { font-size: 2.6rem; }\n\
  \.card-secondary {\n\
  \  width: 56px; height: 80px;\n\
  \}\n\
  \.card-secondary .card-number { font-size: 1.4rem; }\n\
  \\n\
  \/* Hand cards: draggable */\n\
  \.card-hand {\n\
  \  cursor: grab; margin: 0 8px;\n\
  \}\n\
  \.card-hand:hover {\n\
  \  transform: translateY(-12px) scale(1.04);\n\
  \  box-shadow: 0 12px 30px rgba(0,0,0,0.6), 0 0 20px var(--card-glow);\n\
  \  border-color: var(--gold);\n\
  \}\n\
  \.card-hand:active { cursor: grabbing; }\n\
  \\n\
  \/* ===== CARD PILE ===== */\n\
  \.pile-area {\n\
  \  display: flex; flex-direction: column; align-items: center;\n\
  \  margin: 24px 0;\n\
  \}\n\
  \.pile-container {\n\
  \  position: relative; width: 110px; height: 155px;\n\
  \  margin: 0 auto;\n\
  \}\n\
  \.pile-card {\n\
  \  position: absolute; top: 0; left: 0;\n\
  \  transition: transform 0.2s;\n\
  \}\n\
  \.pile-card-latest {\n\
  \  animation: cardDrop 0.35s ease;\n\
  \}\n\
  \@keyframes cardDrop {\n\
  \  0% { transform: scale(0.6) translateY(-40px) rotate(0deg); opacity: 0; }\n\
  \  60% { transform: scale(1.06) translateY(4px); opacity: 1; }\n\
  \  100% { opacity: 1; }\n\
  \}\n\
  \.pile-drop-zone {\n\
  \  width: 110px; height: 155px;\n\
  \  border: 2px dashed rgba(255,255,255,0.1);\n\
  \  border-radius: 12px;\n\
  \  display: flex; align-items: center; justify-content: center;\n\
  \  color: var(--text3); font-size: 0.8rem;\n\
  \  transition: all 0.2s;\n\
  \}\n\
  \.pile-drop-zone.pile-hover,\n\
  \.pile-drop-zone:hover {\n\
  \  border-color: var(--gold);\n\
  \  background: rgba(212,168,52,0.06);\n\
  \  color: var(--gold);\n\
  \}\n\
  \\n\
  \/* ===== TEAM ===== */\n\
  \.team-section { margin-bottom: 16px; }\n\
  \.team-grid { display: flex; gap: 10px; flex-wrap: wrap; }\n\
  \.teammate-card {\n\
  \  background: var(--bg2); border: 1px solid var(--border);\n\
  \  border-radius: 10px; padding: 14px; min-width: 120px; text-align: center;\n\
  \}\n\
  \.teammate-avatar {\n\
  \  width: 36px; height: 36px; border-radius: 50%;\n\
  \  background: linear-gradient(135deg, #7c3aed, var(--blue));\n\
  \  display: flex; align-items: center; justify-content: center;\n\
  \  font-weight: 700; text-transform: uppercase; margin: 0 auto 6px;\n\
  \}\n\
  \.teammate-name { font-weight: 600; font-size: 0.9rem; margin-bottom: 2px; }\n\
  \.teammate-cards { font-size: 0.8rem; color: var(--text2); }\n\
  \.teammate-vote { font-size: 1.1rem; min-height: 20px; }\n\
  \.teammate-topcard {\n\
  \  margin-top: 4px; padding: 2px 8px;\n\
  \  background: var(--gold); color: var(--bg); border-radius: 4px;\n\
  \  font-family: 'Creepster', cursive; font-size: 1rem;\n\
  \  display: inline-block;\n\
  \}\n\
  \\n\
  \/* ===== HAND ===== */\n\
  \.hand-section {\n\
  \  background: var(--bg2); border: 1px solid var(--border);\n\
  \  border-radius: 12px; padding: 18px; margin-bottom: 16px;\n\
  \}\n\
  \.hand-cards {\n\
  \  display: flex; align-items: flex-end; gap: 0;\n\
  \  min-height: 140px; justify-content: center;\n\
  \  perspective: 800px;\n\
  \}\n\
  \.hand-empty { color: var(--text3); font-style: italic; padding: 20px; text-align: center; }\n\
  \.action-buttons {\n\
  \  display: flex; align-items: center; justify-content: center;\n\
  \  gap: 12px; padding: 12px 0;\n\
  \}\n\
  \\n\
  \/* ===== CHAT (bottom-right, grows upward) ===== */\n\
  \.chat-container {\n\
  \  position: fixed; bottom: 0; right: 0;\n\
  \  width: 300px; max-height: 400px;\n\
  \  display: flex; flex-direction: column;\n\
  \  z-index: 100;\n\
  \}\n\
  \.chat-header-bar {\n\
  \  background: var(--bg3); border: 1px solid var(--border);\n\
  \  border-bottom: none;\n\
  \  border-radius: 8px 8px 0 0;\n\
  \  padding: 8px 14px; cursor: pointer;\n\
  \  display: flex; align-items: center; justify-content: space-between;\n\
  \  font-weight: 600; font-size: 0.85rem; color: var(--text2);\n\
  \  letter-spacing: 1px; text-transform: uppercase;\n\
  \}\n\
  \.chat-header-bar:hover { background: #222750; }\n\
  \.chat-body {\n\
  \  background: var(--bg2); border: 1px solid var(--border);\n\
  \  border-top: none;\n\
  \  display: flex; flex-direction: column;\n\
  \  max-height: 340px;\n\
  \}\n\
  \.chat-body-hidden { display: none; }\n\
  \.chat-messages {\n\
  \  flex: 1; overflow-y: auto; padding: 8px 12px;\n\
  \  display: flex; flex-direction: column;\n\
  \  max-height: 260px;\n\
  \}\n\
  \.chat-msg { padding: 4px 0; font-size: 0.82rem; line-height: 1.35; }\n\
  \.chat-msg-user { color: var(--text); }\n\
  \.chat-msg-system { color: var(--text3); font-style: italic; font-size: 0.75rem; }\n\
  \.chat-input-area {\n\
  \  display: flex; padding: 8px; border-top: 1px solid var(--border); gap: 6px;\n\
  \}\n\
  \.chat-input {\n\
  \  flex: 1; padding: 8px 10px;\n\
  \  background: var(--bg3); border: 1px solid var(--border);\n\
  \  border-radius: 4px; color: var(--text);\n\
  \  font-family: inherit; font-size: 0.82rem; outline: none;\n\
  \}\n\
  \.chat-input:focus { border-color: var(--blue); }\n\
  \.chat-input::placeholder { color: var(--text3); }\n\
  \\n\
  \/* ===== SCROLLBAR ===== */\n\
  \::-webkit-scrollbar { width: 5px; }\n\
  \::-webkit-scrollbar-track { background: transparent; }\n\
  \::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.08); border-radius: 3px; }\n\
  \"

faviconDataUri :: Text
faviconDataUri = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E\
  \%3Crect width='64' height='64' rx='12' fill='%230b0e1a'/%3E\
  \%3Cstyle%3E@import url('https://fonts.googleapis.com/css2?family=Nosifer')%3C/style%3E\
  \%3Ctext x='32' y='44' text-anchor='middle' font-family='Nosifer,cursive' font-size='26' fill='%23d4a834'%3ETM%3C/text%3E\
  \%3C/svg%3E"
