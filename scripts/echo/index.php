<?php
/**
 * editor: AksiIDE, https://aksiide.com
 * File: index.php 
 * API Example for Simple Telegram BOT with Carik NLP Engine
 * see NLP file 
 * - others-intents.txt
 * - others-response.txt
 * 
 * @date 
 * @category api
 * @package CarikBot
 * @subpackage apitools
 * @copyright Copyright (c) 2013-endless AksiIDE
 * @link https://carik.id
 */

$ResponseFound = array(
  'Kamu barusan bilang:',
  'Klo ga salah, tadi nulis ini yaa?',
  'Saya tebak-tebak, ada tulisanini',
  'Dalam penerawangan saya, barusan kamu nulis ini'
);
$ResponseNotFound = array(
  'Ga jelas nih, barusan nulis apa?',
  'barusan nulis apa sih?',
  'coba tulis lebih lengkap lagi dong',
  'Saya tidak berhasil menerawang tulisan kamu, coba lagi.'
);
$index = rand(0, (count($ResponseFound)-1));

$Text = $ResponseFound[$index] . "\n";
$value = urldecode(@$_POST['Anything_value']);
$value = str_replace('_', '', $value);
$Text .= "_'".$value."'_";

if (empty($value)){
	$index = rand(0, (count($ResponseNotFound)-1));
	$Text = $ResponseNotFound[$index];
}

$RESULT['code'] = 0;
$RESULT['text'] = $Text;

$output = json_encode( $RESULT, JSON_UNESCAPED_UNICODE);
header('Content-Type: application/json');
echo $output;
